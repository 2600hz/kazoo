%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Listen for inbound route requests and processes them
%%% against the routes db
%%% @end
%%% Created : 14 June 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(stepswitch_inbound).

-behaviour(gen_server).

-include("stepswitch.hrl").

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
	   amqp_q = <<>> :: binary()
	 }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ?LOG_SYS("ensuring database ~s exists", [?ROUTES_DB]),
    couch_mgr:db_create(?ROUTES_DB),
    couch_mgr:revise_views_from_folder(?ROUTES_DB, stepswitch),
    {ok, #state{}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({lookup_number, Number}, From, State) ->
    spawn(fun() ->
                  Num = wh_util:to_e164(wh_util:to_binary(Number)),
                  case lookup_account_by_number(Num) of
                      {ok, AccountId, _}=Ok ->
                          ?LOG("found number is associated to account ~s", [AccountId]),
                          gen_server:reply(From, Ok);
                      {error, Reason}=E ->
                          ?LOG("number is not associated to any account, ~w", [Reason]),
                          gen_server:reply(From, E)
                  end
          end),
    {noreply, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{amqp_q = <<>>}=State) ->
    try
	{ok, Q} = start_amqp(),
	{noreply, State#state{amqp_q=Q}}
    catch
	_:_ ->
            ?LOG_SYS("attempting to connect AMQP again in ~b ms", [?AMQP_RECONNECT_INIT_TIMEOUT]),
            timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
	    {noreply, State}
    end;

handle_info({amqp_reconnect, T}, State) ->
    try
	{ok, NewQ} = start_amqp(),
	{noreply, State#state{amqp_q=NewQ}}
    catch
	_:_ ->
            case T * 2 of
                Timeout when Timeout > ?AMQP_RECONNECT_MAX_TIMEOUT ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [?AMQP_RECONNECT_MAX_TIMEOUT]),
                    timer:send_after(?AMQP_RECONNECT_MAX_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_MAX_TIMEOUT}),
                    {noreply, State};
                Timeout ->
                    ?LOG_SYS("attempting to reconnect AMQP again in ~b ms", [Timeout]),
                    timer:send_after(Timeout, {amqp_reconnect, Timeout}),
                    {noreply, State}
            end
    end;

handle_info({amqp_host_down, _}, State) ->
    ?LOG_SYS("lost AMQP connection, attempting to reconnect"),
    timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
    {noreply, State#state{amqp_q = <<>>}};

handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) when Props#'P_basic'.content_type == <<"application/json">> ->
    spawn(fun() ->
                  JObj = mochijson2:decode(Payload),
                  whapps_util:put_callid(JObj),
                  _ = process_req(whapps_util:get_event_type(JObj), JObj)
          end),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% ensure the exhanges exist, build a queue, bind, and consume
%% @end
%%--------------------------------------------------------------------
-spec start_amqp/0 :: () -> tuple(ok, binary()) | tuple(error, amqp_error).
start_amqp() ->
    try
        _ = amqp_util:callmgr_exchange(),
        Q = amqp_util:new_callmgr_queue(<<>>),
        amqp_util:bind_q_to_callmgr(Q, ?KEY_ROUTE_REQ),
        amqp_util:basic_consume(Q),
        ?LOG_SYS("connected to AMQP"),
        {ok, Q}
    catch
        _:R ->
            ?LOG_SYS("failed to connect to AMQP ~w", [R]),
            {error, amqp_error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec process_req/2 :: (MsgType, JObj) -> no_return() when
      MsgType :: tuple(binary(), binary()),
      JObj :: json_object().
process_req({<<"dialplan">>, <<"route_req">>}, JObj) ->
    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),
    case wh_json:get_value(<<"Account-ID">>, CCVs) of
        undefined ->
            case wh_json:get_value(<<"Offnet-Loopback-Number">>, CCVs) of
                undefined ->
                    ?LOG_START("received new inbound dialplan route request"),
                    _ =  inbound_handler(JObj);
                Number ->
                    ?LOG_START("received inter account inbound dialplan route request"),
                    _ =  inbound_handler(Number, JObj)
            end;
        _ ->
            ok
    end;

process_req({_, _}, _) ->
    {error, invalid_event}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% handle a request inbound from offnet
%% @end
%%--------------------------------------------------------------------
-spec inbound_handler/1 :: (JObj) -> no_return() when
      JObj :: json_object().
inbound_handler(JObj) ->
    Number = get_dest_number(JObj),
    inbound_handler(Number, JObj).
inbound_handler(Number, JObj) ->
    case lookup_account_by_number(Number) of
        {ok, AccountId, _} ->
            ?LOG("number associated with account ~s", [AccountId]),
            relay_route_req(
              wh_json:set_value(<<"Custom-Channel-Vars">>, custom_channel_vars(AccountId, undefined, JObj), JObj)
             );
        {error, R} ->
            ?LOG_END("unable to get account id ~w", [R])
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% determine the e164 format of the inbound number
%% @end
%%--------------------------------------------------------------------
-spec get_dest_number/1 :: (JObj) -> binary() when
      JObj :: json_object().
get_dest_number(JObj) ->
    User = case binary:split(wh_json:get_value(<<"To">>, JObj), <<"@">>) of
               [<<"nouser">>, _] ->
                   [ReqUser, _] = binary:split(wh_json:get_value(<<"Request">>, JObj), <<"@">>),
                   ReqUser;
               [ToUser, _] ->
                   ToUser
           end,
    wh_util:to_e164(User).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% lookup the account ID by number
%% @end
%%--------------------------------------------------------------------
-spec lookup_account_by_number/1 :: (Number) -> tuple(ok, binary(), boolean())|tuple(error, atom()) when
      Number :: binary().
lookup_account_by_number(Number) ->
    ?LOG("lookup account for ~s", [Number]),
    case wh_cache:fetch({stepswitch_number, Number}) of
	{ok, {AccountId, ForceOut}} ->
            {ok, AccountId, ForceOut};
	{error, not_found} ->
            Options = [{<<"key">>, Number}],
	    case couch_mgr:get_results(?ROUTES_DB, ?LIST_ROUTES_BY_NUMBER, Options) of
		{error, _}=E ->
		    E;
		{ok, []} ->
		    {error, not_found};
		{ok, [{struct, _}=JObj]} ->
                    AccountId = wh_json:get_value(<<"id">>, JObj),
                    ForceOut = wh_util:is_true(wh_json:get_value([<<"value">>, <<"force_outbound">>], JObj, false)),
                    wh_cache:store({stepswitch_number, Number}, {AccountId, ForceOut}),
		    {ok, AccountId, ForceOut};
		{ok, [{struct, _}=JObj | _Rest]} ->
		    ?LOG("number lookup resulted in more than one result, using the first"),
                    AccountId = wh_json:get_value(<<"id">>, JObj),
                    ForceOut = wh_util:is_true(wh_json:get_value([<<"value">>, <<"force_outbound">>], JObj, false)),
                    wh_cache:store({stepswitch_number, Number}, {AccountId, ForceOut}),
		    {ok, AccountId, ForceOut}
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% build the JSON to set the custom channel vars with the calls
%% account and authorizing  ID
%% @end
%%--------------------------------------------------------------------
-spec custom_channel_vars/3 :: (AccountId, AuthId, JObj) -> json_object() when
      AccountId :: undefined | binary(),
      AuthId :: undefined | binary(),
      JObj :: json_object().
custom_channel_vars(AccountId, AuthId, JObj) ->
    {struct, CCVs} = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, ?EMPTY_JSON_OBJECT),
    Vars = [{<<"Account-ID">>, AccountId}
            ,{<<"Inception">>, <<"off-net">>}
            ,{<<"Authorizing-ID">>, AuthId}
            |[Var || {K, _}=Var <- CCVs
                         ,K =/= <<"Account-ID">>
                         ,K =/= <<"Inception">>
                         ,K =/= <<"Authorizing-ID">>]
            ],
    {struct, [ KV || {_, V}=KV <- Vars, V =/= undefined ]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% relay a route request once populated with the new properties
%% @end
%%--------------------------------------------------------------------
-spec relay_route_req/1 :: (JObj) -> no_return() when
      JObj :: json_object().
relay_route_req(JObj) ->
    {ok, Payload} = wh_api:route_req(JObj),
    amqp_util:callmgr_publish(Payload, <<"application/json">>, ?KEY_ROUTE_REQ),
    ?LOG_END("relayed route request").
