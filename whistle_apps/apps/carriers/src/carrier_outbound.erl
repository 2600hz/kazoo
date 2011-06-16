%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Listen for outbound route requests and processes them
%%% against the carriers db
%%% @end
%%% Created : 14 June 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(carrier_outbound).

-behaviour(gen_server).

-include("carriers.hrl").

%% API
-export([start_link/0, reload_resources/0, print_debug/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(gateway, {
           server = undefined
          ,realm = undefined
          ,username = undefined
          ,password = undefined
          ,prefix = <<>>
          ,suffix = <<>>
          ,codecs = []
          ,caller_id_type = undefined
          ,progress_timeout = ?DEFAULT_PROGRESS_TIMEOUT
         }).

-record(resrc, {
           resrc_id
          ,weight_cost = 0
          ,flags = []
          ,rules = []
          ,gateways = []
         }).

-record(state, {
	    amqp_q = <<>> :: binary()
           ,resrcs = []
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

reload_resources() ->
    gen_server:call(?MODULE, {reload_resrcs}).

print_debug() ->
    gen_server:cast(?MODULE, {print_debug}).

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
    ?LOG_SYS("starting new carriers outbound server"),
    ?LOG_SYS("ensuring database ~s exists", [?CARRIERS_DB]),
    couch_mgr:db_create(?CARRIERS_DB),
    ?LOG_SYS("ensuring database ~s has view ~s", [?CARRIERS_DB, ?VIEW_FILE]),
    try
        {ok, _} = couch_mgr:update_doc_from_file(?CARRIERS_DB, carriers, ?VIEW_FILE)
    catch
        _:_ ->
            couch_mgr:load_doc_from_file(?CARRIERS_DB, carriers, ?VIEW_FILE)
    end,
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
handle_call({reload_resrcs}, _, State) ->
    Resrcs = load_resrcs(),
    {reply, ok, State#state{resrcs=Resrcs}};

handle_call(_Request, _From, State) ->
    {reply, {error, not_implemented}, State}.

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
handle_cast({print_debug}, State) ->
    print_debug(State#state.resrcs),
    {noreply, State};
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
	{noreply, State#state{amqp_q=Q, resrcs=load_resrcs()}}
    catch
	_:_ ->
            ?LOG_SYS("attempting to connect AMQP again in ~b ms", [?AMQP_RECONNECT_INIT_TIMEOUT]),
            timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
	    {noreply, State}
    end;

handle_info({amqp_reconnect, T}, State) ->
    try
	{ok, NewQ} = start_amqp(),
	{noreply, State#state{amqp_q=NewQ, resrcs=load_resrcs()}}
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
terminate(_Reason, #state{amqp_q=Q}) ->
    amqp_util:delete_queue(Q),
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
-spec(start_amqp/0 :: () -> tuple(ok, binary()) | tuple(error, amqp_error)).
start_amqp() ->
    try
        _ = amqp_util:callmgr_exchange(),
        _ = amqp_util:targeted_exchange(),
        Q = amqp_util:new_callmgr_queue(<<>>),
        amqp_util:bind_q_to_callmgr(Q, ?KEY_ROUTE_REQ),
        amqp_util:bind_q_to_targeted(Q),
        amqp_util:basic_consume(Q),
        ?LOG_SYS("connected to AMQP"),
        {ok, Q}
    catch
        _:R ->
            ?LOG_SYS("failed to connect to AMQP ~w", [R]),
            {error, amqp_error}
    end.

load_resrcs() ->
    case couch_mgr:get_results(?CARRIERS_DB, ?LIST_ACTIVE_RESOURCE, [{<<"include_docs">>, true}]) of
        {ok, Resrcs} ->
            [create_resrc(wh_json:get_value(<<"doc">>, R)) || R <- Resrcs];
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec(process_req/2 :: (MsgType :: tuple(binary(), binary()), JObj :: json_object()) -> no_return()).
process_req({_, _}, _) ->
    {error, invalid_event}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a resrc JSON object it builds a resrc record and
%% populates it with all enabled gateways
%% @end
%%--------------------------------------------------------------------
-spec(create_resrc/1 :: (JObj :: json_object()) -> #resrc{}).
create_resrc(JObj) ->
    Default = #resrc{},
    #resrc{resrc_id = wh_json:get_value(<<"_id">>, JObj)
              ,weight_cost =
                  constrain_weight(wh_json:get_value(<<"weight_cost">>, JObj, Default#resrc.weight_cost))
              ,flags = wh_json:get_value(<<"flags">>, JObj, Default#resrc.flags)
              ,rules = [re:compile(R) || R <- wh_json:get_value(<<"rules">>, JObj, Default#resrc.rules)]
              ,gateways = [create_gateway(G) || G <- wh_json:get_value(<<"gateways">>, JObj, []),
                                                whistle_util:is_true(wh_json:get_value(<<"enabled">>, G, true))]
             }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a gateway JSON object it builds a gateway record
%% @end
%%--------------------------------------------------------------------
-spec(create_gateway/1 :: (JObj :: json_object()) -> #gateway{}).
create_gateway(JObj) ->
    Default = #gateway{},
    #gateway{server = wh_json:get_value(<<"server">>, JObj, Default#gateway.server)
             ,realm = wh_json:get_value(<<"realm">>, JObj, Default#gateway.realm)
             ,username = wh_json:get_value(<<"username">>, JObj, Default#gateway.username)
             ,password = wh_json:get_value(<<"password">>, JObj, Default#gateway.password)
             ,prefix =
                 whistle_util:to_binary(wh_json:get_value(<<"prefix">>, JObj, Default#gateway.prefix))
             ,suffix =
                 whistle_util:to_binary(wh_json:get_value(<<"suffix">>, JObj, Default#gateway.suffix))
             ,codecs = wh_json:get_value(<<"codecs">>, JObj, Default#gateway.codecs)
             ,caller_id_type = wh_json:get_value(<<"caller_id_type">>, JObj, Default#gateway.caller_id_type)
             ,progress_timeout =
                 whistle_util:to_integer(wh_json:get_value(<<"progress_timeout">>, JObj, Default#gateway.progress_timeout))
            }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% constrain the weight on a scale from 1 to 100
%% @end
%%--------------------------------------------------------------------
-spec(constrain_weight/1 :: (W :: binary() | integer()) -> integer()).
constrain_weight(W) when not is_integer(W) ->
    constrain_weight(whistle_util:to_integer(W));
constrain_weight(W) when W > 100 -> 100;
constrain_weight(W) when W < 1 -> 1;
constrain_weight(W) -> W.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function recieves a resource rule (regex) and determines if
%% the destination number matches.  If it does and the regex has a
%% capture group return the group, if not but it matched return the
%% full destination number otherwise return an empty list.
%% @end
%%--------------------------------------------------------------------
-spec(evaluate_rule/2 :: (Regex :: tuple(), DestNum:: binary()) -> list()).
evaluate_rule({error, Reason}, _) ->
    ?LOG("regex did not compile ~w", [Reason]),
    [];
evaluate_rule(Regex, DestNum) ->
    case re:run(DestNum, Regex) of
        {match, [_, {Start,End}|_]} ->
            [binary:part(DestNum, Start, End)];
        {match, _} ->
            [DestNum];
        _ ->
            []
    end.







%% TMP
print_debug([]) ->
    ok;
print_debug([#resrc{gateways=Gateways, resrc_id=Id}|T]) ->
    io:format("Resource ~s~n", [Id]),
    [print_gateway(Gateway) || Gateway <- Gateways],
    print_debug(T).

print_gateway(#gateway{server=S, realm=R}) ->
    io:format("    gateway ~s (~s)~n", [S, R]).
