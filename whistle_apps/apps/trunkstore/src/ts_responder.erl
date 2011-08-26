%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, VoIP INC
%%% @doc
%%% Trunk-Store responder waits for Auth and Route requests on the broadcast
%%% Exchange, and delievers the requests to the corresponding handler.
%%% TS responder also receives responses from the handlers and returns them
%%% to the requester.
%%% Each request received by TS_RESPONDER should be put into a new spawn()
%%% to avoid blocking on each request.
%%% @end
%%% Created : 31 Aug 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ts_responder).

-behaviour(gen_server).

%% API
-export([start_link/0, start_responder/0, transfer_auth/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ts.hrl").

-define(SERVER, ?MODULE).
-define(ROUTE_QUEUE_NAME, <<"ts_responder.route.queue">>).
-define(AUTH_QUEUE_NAME, <<"ts_responder.auth.queue">>).

-record(state, {
	  is_amqp_up = false :: boolean()
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
    spawn(fun() ->
		  _ = transfer_auth(),
		  [ ts_responder_sup:start_handler() || _ <- [1,2,3] ]
	  end),
    ignore.

start_responder() ->
    ?LOG("Starting responder"),
    gen_server:start_link(?MODULE, [], []).

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
    process_flag(trap_exit, true),
    couch_mgr:db_create(?TS_DB),
    ?LOG_SYS("Ensured ~s is created", [?TS_DB]),
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
handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

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

%% receive resource requests from Apps
handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State) ->
    {_Pid, _Ref} = spawn_monitor(fun() -> handle_req(Props#'P_basic'.content_type, Payload) end),
    {noreply, State, hibernate};

handle_info({'DOWN', _, process, _Pid, _Reason}, State) ->
    ?LOG_SYS("handle_req for ~p down: ~p", [_Pid, _Reason]),
    {noreply, State, hibernate};

handle_info(timeout, #state{is_amqp_up=false}=S) ->
    ?LOG_SYS("starting amqp"),
    {ok, CQ} = start_amqp(),
    ?LOG_SYS("Starting up responder with AMQP Queue: ~s", [CQ]),
    {noreply, S#state{is_amqp_up=is_binary(CQ)}, 1000};

handle_info(timeout, #state{is_amqp_up=true}=S) ->
    {noreply, S};

handle_info({amqp_host_down, H}, S) ->
    ?LOG_SYS("AMQP Host(~s) down", [H]),
    {noreply, S#state{is_amqp_up=false}, 1000};

handle_info({amqp_lost_channel, no_connection}, S) ->
    ?LOG_SYS("AMQP channel lost due to no connection"),
    {noreply, S#state{is_amqp_up=false}, 1000};

handle_info(Req, #state{is_amqp_up=false}=S) ->
    case start_amqp() of
	{ok, _} ->
	    handle_info(Req, S#state{is_amqp_up=true});
	{error, _} ->
	    ?LOG_SYS("Dropping request, AMQP down: ~p~n", [Req]),
	    {noreply, S}
    end;

handle_info(#'basic.consume_ok'{}, S) ->
    {noreply, S, hibernate};

%% catch all so we don't lose state
handle_info(_Unhandled, State) ->
    ?LOG_SYS("Unknown message: ~p~n", [_Unhandled]),
    {noreply, State, 1000}.

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
terminate(_Reason, _) ->
    ?LOG_SYS("Terminating: ~p~n", [_Reason]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    ?LOG_SYS("Code Change from ~p~n", [_OldVsn]),
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
-spec(handle_req/2 :: (ContentType :: binary(), Payload :: binary()) -> no_return()).
handle_req(<<"application/json">>, Payload) ->
    JObj = mochijson2:decode(binary_to_list(Payload)),

    put(callid, wh_json:get_value(<<"Call-ID">>, JObj, wh_json:get_value(<<"Msg-ID">>, JObj, <<"0000000000">>))),
    ?LOG_START("Received ~s", [Payload]),

    process_req(get_msg_type(JObj), JObj);
handle_req(_ContentType, _Payload) ->
    ?LOG_SYS("Received payload with unknown content type: ~p -> ~s", [_ContentType, _Payload]).

-spec get_msg_type/1 :: (JObj) -> {binary(), binary()} when
      JObj :: json_object().
get_msg_type(JObj) ->
    { wh_json:get_value(<<"Event-Category">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj) }.

-spec process_req/2 :: (MsgType, ApiJObj) -> no_return() when
      MsgType :: {binary(), binary()},
      ApiJObj :: json_object().
process_req({<<"dialplan">>,<<"route_req">>}, ApiJObj) ->
    try
        true = wh_api:route_req_v(ApiJObj),
        CallID = wh_json:get_value(<<"Call-ID">>, ApiJObj),
        case {wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], ApiJObj), wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Authorizing-ID">>], ApiJObj)} of
            {AcctID, undefined} when is_binary(AcctID) ->
                %% Coming from carrier (off-net)
                ?LOG_START(CallID, "Offnet call starting", []),
                ts_offnet_sup:start_handler(<<"offnet-", CallID/binary>>, ApiJObj);
            {AcctID, AuthID} when is_binary(AcctID) andalso is_binary(AuthID) ->
                %% Coming from PBX (on-net); authed by Registrar or ts_auth
                ?LOG_START(CallID, "Onnet call starting", []),
                ts_onnet_sup:start_handler(<<"onnet-", CallID/binary>>, ApiJObj);
            {_AcctID, _AuthID} ->
                ?LOG("Error in routing: AcctID: ~s AuthID: ~s", [_AcctID, _AuthID])
        end
    catch
	A:{error,B} ->
	    ?LOG_END("Route request exception: ~p:~p", [A, B]),
	    ?LOG_SYS("Stacktrace: ~p", [erlang:get_stacktrace()])
    end.

-spec start_amqp/0 :: () -> {ok, binary()} | {error, amqp_error}.
start_amqp() ->
    ReqQueue = amqp_util:new_queue(?ROUTE_QUEUE_NAME, [{exclusive, false}]),

    try
	amqp_util:basic_qos(1), %% control egress of messages from the queue, only send one at time (load balances)

	?LOG_SYS("QOS=1 set"),

	%% Bind the queue to an exchange
	_ = amqp_util:bind_q_to_callmgr(ReqQueue, ?KEY_ROUTE_REQ),

	?LOG_SYS("Bound queue"),

	%% Register a consumer to listen to the queue
	amqp_util:basic_consume(ReqQueue, [{exclusive, false}]),

	?LOG_SYS("Consuming"),

	{ok, ReqQueue}
    catch
	_A:_B ->
	    ?LOG_SYS("Error starting AMQP: ~p: ~p", [_A, _B]),
	    {error, amqp_error}
    end.

-spec transfer_auth/0 :: () -> ok.
transfer_auth() ->
    case couch_mgr:get_results(?TS_DB, ?TS_VIEW_USERAUTHREALM, []) of
	{ok, AuthJObjs} ->
	    ?LOG_SYS("Importing ~b accounts into sip_auth", [length(AuthJObjs)]),
	    _ = [ transfer_auth(AuthJObj) || AuthJObj <- AuthJObjs],
	    ok;
	_E ->
	    ok
    end.

-spec transfer_auth/1 :: (AuthJObj) -> no_return() when
      AuthJObj :: json_object().
transfer_auth(AuthJObj) ->
    ID = wh_json:get_value(<<"id">>, AuthJObj),
    spawn(fun() -> ?LOG_SYS("del doc ~s: ~p", [ID, couch_mgr:del_doc(<<"sip_auth">>, ID)]) end), %% cleanup old imports

    AuthData = {struct, AuthProps}
        = wh_json:get_value(<<"value">>, AuthJObj, ?EMPTY_JSON_OBJECT),

    DocID = <<ID/binary, (wh_json:get_binary_value(<<"server_id">>, AuthData, <<"0">>))/binary>>,

    SipAuthDoc = {struct, [{<<"_id">>, DocID}
			   ,{<<"sip">>, {struct, [
						  {<<"realm">>, wh_json:get_value(<<"auth_realm">>, AuthData, <<"">>)}
						  ,{<<"method">>, wh_json:get_value(<<"auth_method">>, AuthData, <<"">>)}
						  ,{<<"username">>, wh_json:get_value(<<"auth_user">>, AuthData, <<"">>)}
						  ,{<<"password">>, wh_json:get_value(<<"auth_password">>, AuthData, <<"">>)}
					]}
			    }]},
    SAD1 = lists:foldl(fun({K, V}, JObj) ->
                              wh_json:set_value(K, V, JObj)
                      end, SipAuthDoc, [Pvt || {K, _}=Pvt <- AuthProps
                                                   ,binary:match(K, <<"pvt_">>) =/= nomatch]),
    %% trunkstore just has to be different, or are we all different and trunkstore normal...
    %% after all it was first ;)
    SAD2 = wh_json:set_value(<<"pvt_account_id">>, ID, SAD1),
    couch_mgr:ensure_saved(<<"sip_auth">>, SAD2).
