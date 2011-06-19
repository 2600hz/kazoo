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
-export([start_link/0, reload_resources/0, process_number/1, process_number/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(gateway, {
           resource_id = undefined
          ,server = undefined
          ,realm = undefined
          ,username = undefined
          ,password = undefined
          ,route = undefined
          ,prefix = <<>>
          ,suffix = <<>>
          ,codecs = []
          ,bypass_media = <<"false">>
          ,caller_id_type = undefined
          ,sip_headers = undefined
          ,progress_timeout = ?DEFAULT_PROGRESS_TIMEOUT
         }).

-record(resrc, {
           id
          ,rev
          ,weight_cost = 0
          ,grace_period = ?DEFAULT_GRACE_PERIOD
          ,flags = []
          ,rules = []
          ,gateways = []
         }).

-record(state, {
	    amqp_q = <<>> :: binary()
           ,last_doc_change = {<<>>, [<<>>]}
           ,resrcs = []
	 }).

-type endpoint() :: tuple(1..100, non_neg_integer(), binary(), #gateway{}).
-type endpoints() :: [] | [endpoint()].

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

process_number(Number) ->
    Num = whistle_util:to_binary(Number),
    gen_server:call(?MODULE, {process_number, Num}).

process_number(Number, Flags) ->
    Num = whistle_util:to_binary(Number),
    gen_server:call(?MODULE, {process_number, Num, Flags}).

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
    Resrcs = get_resrcs(),
    {reply, ok, State#state{resrcs=Resrcs}};

handle_call({process_number, Number}, From, #state{resrcs=Resrcs}=State) ->
    spawn(fun() ->
                  gen_server:reply(From, evaluate_number(Number, Resrcs))
          end),
    {noreply, State};

handle_call({process_number, Number, Flags}, From, #state{resrcs=R1}=State) ->
    spawn(fun() ->
                  R2 = evaluate_flags(Flags, R1),
                  gen_server:reply(From, evaluate_number(Number, R2))
          end),
    {noreply, State};

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
	{noreply, State#state{amqp_q=Q, resrcs=get_resrcs()}}
    catch
	_:_ ->
            ?LOG_SYS("attempting to connect AMQP again in ~b ms", [?AMQP_RECONNECT_INIT_TIMEOUT]),
            timer:send_after(?AMQP_RECONNECT_INIT_TIMEOUT, {amqp_reconnect, ?AMQP_RECONNECT_INIT_TIMEOUT}),
	    {noreply, State}
    end;

handle_info({amqp_reconnect, T}, State) ->
    try
	{ok, NewQ} = start_amqp(),
	{noreply, State#state{amqp_q=NewQ, resrcs=get_resrcs()}}
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

handle_info({document_changes, DocId, Changes}, #state{last_doc_change={DocId, Changes}}=State) ->
    %% Ignore the duplicate document change notifications used for keep alives
    {noreply, State};

handle_info({document_changes, DocId, [Changes]=C}, #state{resrcs=Resrcs}=State) ->
    Rev = wh_json:get_value(<<"rev">>, Changes),
    case lists:keysearch(DocId, #resrc.id, Resrcs) of
        {value, #resrc{rev=Rev}} -> {noreply, State#state{last_doc_change={DocId, C}}};
        _ -> {noreply, State#state{resrcs=update_resrc(DocId, Resrcs), last_doc_change={DocId, C}}}
    end;

handle_info({document_deleted, DocId}, #state{resrcs=Resrcs}=State) ->
    case lists:keysearch(DocId, #resrc.id, Resrcs) of
        false -> {noreply, State};
        {value, _} ->
            ?LOG_SYS("resource ~p deleted", [DocId]),
            couch_mgr:rm_change_handler(?CARRIERS_DB, DocId),
            {noreply, State#state{resrcs=lists:keydelete(DocId, #resrc.id, Resrcs)}}
    end;

handle_info({_, #amqp_msg{props = Props, payload = Payload}}, State)
  when Props#'P_basic'.content_type == <<"application/json">> ->
    spawn(fun() ->
                  JObj = mochijson2:decode(Payload),
                  whapps_util:put_callid(JObj),
                  _ = process_req(whapps_util:get_event_type(JObj), JObj, State)
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
terminate(_Reason, _) ->
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
        %% control egress of messages from the queue, only send one at time (load balances)
	{'basic.qos_ok'} = amqp_util:basic_qos(1),
        _ = amqp_util:resource_exchange(),
        Q = amqp_util:new_resource_queue(),
        amqp_util:bind_q_to_resource(Q, ?KEY_OFFNET_RESOURCE_REQ),
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
%% Gets a list of all active resources from the DB
%% @end
%%--------------------------------------------------------------------
-spec(get_resrcs/0 :: () -> []|[#resrc{}]).
get_resrcs() ->
    case couch_mgr:get_results(?CARRIERS_DB, ?LIST_ACTIVE_RESOURCE, [{<<"include_docs">>, true}]) of
        {ok, Resrcs} ->
            [create_resrc(wh_json:get_value(<<"doc">>, R)) || R <- Resrcs];
        {error, _}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Syncs a resource with the DB and updates it in the list
%% of resources
%% @end
%%--------------------------------------------------------------------
-spec(update_resrc/2 :: (DocId :: binary(), Resrcs :: [#resrc{}]) -> []|[#resrc{}]).
update_resrc(DocId, Resrcs) ->
    case couch_mgr:open_doc(?CARRIERS_DB, DocId) of
        {ok, JObj} ->
            case whistle_util:is_true(wh_json:get_value(<<"enabled">>, JObj)) of
                true ->
                    NewResrc = create_resrc(JObj),
                    ?LOG_SYS("resource ~s updated to rev ~s", [DocId, NewResrc#resrc.rev]),
                    [NewResrc|lists:keydelete(DocId, #resrc.id, Resrcs)];
                false ->
                    ?LOG_SYS("resource ~s disabled", [DocId]),
                    lists:keydelete(DocId, #resrc.id, Resrcs)
            end;
        {error, R} ->
            ?LOG_SYS("removing resource ~s, ~w", [DocId, R]),
            couch_mgr:rm_change_handler(?CARRIERS_DB, DocId),
            lists:keydelete(DocId, #resrc.id, Resrcs)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% process the AMQP requests
%% @end
%%--------------------------------------------------------------------
-spec(process_req/3 :: (MsgType :: tuple(binary(), binary()), JObj :: json_object(), State :: #state{})
                       -> no_return()).
process_req({<<"resource">>, <<"offnet_req">>}, JObj, #state{resrcs=R1}) ->
    whapps_util:put_callid(JObj),
    CallId = get(callid),

    <<"audio">> = wh_json:get_value(<<"Resource-Type">>, JObj),

    Number = wh_json:get_value(<<"To-DID">>, JObj),

    ?LOG_START("off-net resource bridge request to ~s for account ~s", [Number, wh_json:get_value(<<"Account-ID">>, JObj)]),

    Endpoints = case wh_json:get_value(<<"Flags">>, JObj) of
                    undefined -> evaluate_number(Number, R1);
                    Flags ->
                        [?LOG("resource must have ~s flag", [F]) || F <- Flags],
                        R2 = evaluate_flags(Flags, R1),
                        evaluate_number(Number, R2)
                end,

    Q = amqp_util:new_queue(),
    amqp_util:bind_q_to_callevt(Q, CallId),
    amqp_util:basic_consume(Q),

    BridgeReq = build_bridge_request(JObj, Endpoints, Q),

    case length(wh_json:get_value(<<"Endpoints">>, BridgeReq, [])) of
        0 ->
            ?LOG_END("no offnet resources found for request, sending failure response"),
            respond_resource_failed({struct, [
                                               {<<"Hangup-Cause">>, <<"NO_ROUTE_TRANSIT_NET">>}
                                              ,{<<"Hangup-Code">>, <<"sip:404">>}
                                             ]}, 0, JObj);
        Attempts ->
            {ok, Payload} = whistle_api:bridge_req({struct, BridgeReq}),
            amqp_util:callctl_publish(wh_json:get_value(<<"Control-Queue">>, JObj), Payload),

            case wait_for_bridge(60000) of
                {ok, BridgeResp} ->
                    ?LOG_END("offnet resource request resulted in a successful bridge"),
                    respond_bridged_to_resource(BridgeResp, JObj);
                {fail, BridgeResp} ->
                    ?LOG_END("offnet resource failed, ~s:~s", [
                                                               wh_json:get_value(<<"Hangup-Code">>, BridgeResp)
                                                               ,wh_json:get_value(<<"Application-Response">>, BridgeResp)
                                                              ]),
                    respond_resource_failed(BridgeResp, Attempts, JObj);
                {hungup, HangupResp} ->
                    ?LOG_END("requesting leg hungup, ~s:~s", [
                                                               wh_json:get_value(<<"Hangup-Code">>, HangupResp)
                                                              ,wh_json:get_value(<<"Hangup-Cause">>, HangupResp)
                                                             ]);
                {error, timeout} ->
                    ?LOG_END("resource bridge request did not respond"),
                    respond_resource_failed({struct, [{<<"Failure-Message">>, <<"TIMEOUT">>}]}, Attempts, JObj);
                {error, ErrorResp} ->
                    ?LOG_END("internal resource bridge error")
            end
    end;

process_req({_, _}, _, _) ->
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
    Id = wh_json:get_value(<<"_id">>, JObj),
    ?LOG_SYS("loading resource ~s", [Id]),
    couch_mgr:add_change_handler(?CARRIERS_DB, Id),
    #resrc{id = Id
           ,rev = wh_json:get_value(<<"_rev">>, JObj)
           ,weight_cost =
               constrain_weight(wh_json:get_value(<<"weight_cost">>, JObj, Default#resrc.weight_cost))
           ,grace_period =
               whistle_util:to_integer(wh_json:get_value(<<"grace_period">>, JObj, Default#resrc.grace_period))
           ,flags = wh_json:get_value(<<"flags">>, JObj, Default#resrc.flags)
           ,rules = [R2 || R1 <- wh_json:get_value(<<"rules">>, JObj, Default#resrc.rules)
                               ,(R2 = compile_rule(R1, Id)) =/= error]
           ,gateways = [create_gateway(G, Id) || G <- wh_json:get_value(<<"gateways">>, JObj, []),
                                             whistle_util:is_true(wh_json:get_value(<<"enabled">>, G, true))]
          }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a gateway JSON object it builds a gateway record
%% @end
%%--------------------------------------------------------------------
-spec(create_gateway/2 :: (JObj :: json_object(), Id :: binary()) -> #gateway{}).
create_gateway(JObj, Id) ->
    Default = #gateway{},
    #gateway{resource_id = Id
             ,server = wh_json:get_value(<<"server">>, JObj, Default#gateway.server)
             ,realm = wh_json:get_value(<<"realm">>, JObj, Default#gateway.realm)
             ,username = wh_json:get_value(<<"username">>, JObj, Default#gateway.username)
             ,password = wh_json:get_value(<<"password">>, JObj, Default#gateway.password)
             ,route = wh_json:get_value(<<"route">>, JObj, Default#gateway.route)
             ,prefix =
                 whistle_util:to_binary(wh_json:get_value(<<"prefix">>, JObj, Default#gateway.prefix))
             ,suffix =
                 whistle_util:to_binary(wh_json:get_value(<<"suffix">>, JObj, Default#gateway.suffix))
             ,codecs = wh_json:get_value(<<"codecs">>, JObj, Default#gateway.codecs)
             ,bypass_media = wh_json:get_value(<<"bypass_media">>, JObj, Default#gateway.bypass_media)
             ,caller_id_type = wh_json:get_value(<<"caller_id_type">>, JObj, Default#gateway.caller_id_type)
             ,sip_headers = wh_json:get_value(<<"custom_sip_headers">>, JObj, Default#gateway.sip_headers)
             ,progress_timeout =
                 whistle_util:to_integer(wh_json:get_value(<<"progress_timeout">>, JObj, Default#gateway.progress_timeout))
            }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wrapper for re:compile so we can log rules that fail (including
%% which resource it was on).
%% @end
%%--------------------------------------------------------------------
-spec(compile_rule/2 :: (Rule :: binary(), Id :: binary()) -> re:mp()|error).
compile_rule(Rule, Id) ->
    case re:compile(Rule) of
        {ok, MP} ->
            ?LOG_SYS("compiled ~s on resource ~s", [Rule, Id]),
            MP;
        {error, R} ->
            ?LOG_SYS("bad rule '~s' on resource ~s, ~p", [Rule, Id, R]),
            error
    end.

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
%% Filter the list of resources returning only those that have every
%% flag provided
%% @end
%%--------------------------------------------------------------------
-spec(evaluate_flags/2 :: (F1 :: list(), Resrcs :: [#resrc{}]) -> []|[#resrc{}]).
evaluate_flags(F1, Resrcs) ->
    [Resrc || #resrc{flags=F2}=Resrc <- Resrcs
                  ,lists:all(fun(Flag) -> lists:member(Flag, F2) end, F1)].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Filter the list of resources returning only those with a rule that
%% matches the number.  The list is of tuples with three elements,
%% the weight, the captured component of the number, and the gateways.
%% @end
%%--------------------------------------------------------------------
-spec(evaluate_number/2 :: (Number :: binary(), Resrcs :: [#resrc{}]) -> endpoints()).
evaluate_number(Number, Resrcs) ->
    sort_endpoints(get_endpoints(Number, Resrcs)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sort the gateway tuples returned by evalutate_resrcs according to
%% weight.
%% @end
%%--------------------------------------------------------------------
-spec(sort_endpoints/1 :: (Endpoints :: endpoints()) -> endpoints()).
sort_endpoints(Endpoints) ->
    lists:sort(fun({W1, _, _, _}, {W2, _, _, _}) ->
                       W1 =< W2
               end, Endpoints).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec(get_endpoints/2 :: (Number :: binary(), Resrcs :: [#resrc{}]) -> endpoints()).
get_endpoints(Number, Resrcs) ->
    [Endpoint || Endpoint <- lists:map(fun(R) -> get_endpoint(Number, R) end, Resrcs)
                     ,Endpoint =/= no_match].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a gateway JSON object it builds a gateway record
%% @end
%%--------------------------------------------------------------------
-spec(get_endpoint/2 :: (Number :: binary(), Resrc :: #resrc{}) -> endpoint()|no_match).
get_endpoint(Number, #resrc{weight_cost=WC, gateways=Gtws, rules=Rules, grace_period=GP}) ->
    case evaluate_rules(Rules, Number) of
        {ok, DestNum} ->
            {WC, GP, DestNum, Gtws};
        {error, no_match} ->
            no_match
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function loops over rules (regex) and until one matches
%% the destination number.  If the matching rule has a
%% capture group return the largest group, otherwise return the whole
%% number.  In the event that no rules match then return an error.
%% @end
%%--------------------------------------------------------------------
-spec(evaluate_rules/2 :: (Regex :: re:mp(), Number:: binary()) -> tuple(ok, binary())|tuple(error, no_match)) .
evaluate_rules([], _) ->
    {error, no_match};
evaluate_rules([Regex|T], Number) ->
    case re:run(Number, Regex) of
        {match, [{Start,End}]} ->
            {ok, binary:part(Number, Start, End)};
        {match, CaptureGroups} ->
            %% find the largest matching group if present by sorting the position of the
            %% matching groups by list, reverse so head is largest, then take the head of the list
            {Start, End} = hd(lists:reverse(lists:keysort(2, tl(CaptureGroups)))),
            {ok, binary:part(Number, Start, End)};
        _ ->
            evaluate_rules(T, Number)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the proplist for a whistle API bridge request from the
%% off-net request, endpoints, and our AMQP Q
%% @end
%%--------------------------------------------------------------------
-spec(build_bridge_request/3 :: (JObj :: json_object(), Endpoints :: endpoints(), Q :: binary())
                                -> proplist()).
build_bridge_request(JObj, Endpoints, Q) ->
    Command = [
                {<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, build_endpoints(Endpoints, 0, [])}
               ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, JObj)}
               ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, JObj)}
               ,{<<"Outgoing-Caller-ID-Name">>, wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, JObj)}
               ,{<<"Outgoing-Caller-ID-Number">>, wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj)}
               ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, JObj)}
               ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
               ,{<<"SIP-Headers">>, wh_json:get_value(<<"SIP-Headers">>, JObj)}
               ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
               | whistle_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    [ KV || {_, V}=KV <- Command, V =/= undefined ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the proplist for a whistle API bridge request from the
%% off-net request, endpoints, and our AMQP Q
%% @end
%%--------------------------------------------------------------------
-spec(build_endpoints/3 :: (Endpoints :: endpoints(), Delay :: non_neg_integer(), Acc :: proplist())
                           -> proplist()).
build_endpoints([], _, Acc) ->
    lists:reverse(Acc);
build_endpoints([{_, GracePeriod, Number, [Gateway]}|T], Delay, Acc0) ->
    build_endpoints(T, Delay + GracePeriod, [build_endpoint(Number, Gateway, Delay)|Acc0]);
build_endpoints([{_, GracePeriod, Number, Gateways}|T], Delay, Acc0) ->
    {D2, Acc1} = lists:foldl(fun(Gateway, {0, AccIn}) ->
                                     {2, [build_endpoint(Number, Gateway, 0)|AccIn]};
                                 (Gateway, {D0, AccIn}) ->
                                     {D0 + 2, [build_endpoint(Number, Gateway, D0)|AccIn]}
                            end, {Delay, Acc0}, Gateways),
    build_endpoints(T, D2 - 2 + GracePeriod, Acc1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Build the endpoint for use in the route request
%% @end
%%--------------------------------------------------------------------
-spec(build_endpoint/3 :: (Number :: non_neg_integer(), Gateway :: #gateway{}, Delay :: non_neg_integer())
                          -> proplist()).
build_endpoint(Number, Gateway, Delay) ->
    Route = get_dialstring(Gateway, Number),
    ?LOG("using ~s on ~s delayed by ~b sec", [Route, Gateway#gateway.resource_id, Delay]),
    Prop = [
             {<<"Invite-Format">>, <<"route">>}
            ,{<<"Route">>, get_dialstring(Gateway, Number)}
            ,{<<"Callee-ID-Number">>, whistle_util:to_binary(Number)}
            ,{<<"Caller-ID-Type">>, Gateway#gateway.caller_id_type}
            ,{<<"Endpoint-Delay">>, whistle_util:to_binary(Delay)}
            ,{<<"Bypass-Media">>, Gateway#gateway.bypass_media}
            ,{<<"Endpoint-Progress-Timeout">>, whistle_util:to_binary(Gateway#gateway.progress_timeout)}
            ,{<<"Codecs">>, Gateway#gateway.codecs}
            ,{<<"Auth-User">>, Gateway#gateway.username}
            ,{<<"Auth-Password">>, Gateway#gateway.password}
            ,{<<"SIP-Headers">>, Gateway#gateway.sip_headers}
            ,{<<"Custom-Channel-Vars">>, {struct, [{<<"Resource-ID">>, Gateway#gateway.resource_id}]}}
           ],
    {struct, [ KV || {_, V}=KV <- Prop, V =/= undefined andalso V =/= <<"0">>]}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the route dialstring
%% @end
%%--------------------------------------------------------------------
-spec(get_dialstring/2 :: (Gateway :: #gateway{}, Number :: non_neg_integer())
                          -> binary()).
get_dialstring(#gateway{route=undefined}=Gateway, Number) ->
    <<"sip:"
      ,(whistle_util:to_binary(Gateway#gateway.prefix))/binary,
      Number/binary
      ,(whistle_util:to_binary(Gateway#gateway.suffix))/binary
      ,"@", (whistle_util:to_binary(Gateway#gateway.server))/binary
    >>;
get_dialstring(#gateway{route=Route}, _) ->
    Route.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% waits for the return from the bridge request
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_bridge/1 :: (Timeout :: non_neg_integer()) -> tuple(ok, json_object())|tuple(error, atom())).
wait_for_bridge(Timeout) ->
    Start = erlang:now(),
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
            JObj = mochijson2:decode(Payload),
            case { wh_json:get_value(<<"Application-Name">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj), wh_json:get_value(<<"Event-Category">>, JObj) } of
                { _, <<"CHANNEL_BRIDGE">>, <<"call_event">> } ->
                    {ok, JObj};
                { <<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">> } ->
                    {fail, JObj};
                { _, <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
                    {hungup, JObj};
                { _, _, <<"error">> } ->
                    {error, JObj};
                _ ->
		    DiffMicro = timer:now_diff(erlang:now(), Start),
                    wait_for_bridge(Timeout - (DiffMicro div 1000))
            end;
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            DiffMicro = timer:now_diff(erlang:now(), Start),
            wait_for_bridge(Timeout - (DiffMicro div 1000))
    after
        Timeout ->
            {error, timeout}
    end.

respond_bridged_to_resource(BridgeResp, JObj) ->
    Q = wh_json:get_value(<<"Server-ID">>, BridgeResp),
    Response = [
                 {<<"Call-ID">>, wh_json:get_value(<<"Other-Leg-Unique-ID">>, BridgeResp)}
                ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                ,{<<"Control-Queue">>, wh_json:get_value(<<"Control-Queue">>, JObj)}
                ,{<<"To">>, wh_json:get_value(<<"Other-Leg-Destination-Number">>, BridgeResp)}
                ,{<<"Caller-ID-Name">>, wh_json:get_value(<<"Other-Leg-Caller-ID-Name">>, BridgeResp)}
                ,{<<"Caller-ID-Number">>, wh_json:get_value(<<"Other-Leg-Caller-ID-Number">>, BridgeResp)}
                ,{<<"Custom-Channel-Vars">>, wh_json:get_value(<<"Custom-Channel-Vars">>, BridgeResp)}
                ,{<<"Timestamp">>, wh_json:get_value(<<"Timestamp">>, BridgeResp)}
                ,{<<"Channel-Call-State">>, wh_json:get_value(<<"Channel-Call-State">>, BridgeResp)}
               | whistle_api:default_headers(Q, <<"resource">>, <<"offnet_resp">>, ?APP_NAME, ?APP_VERSION)
            ],
    {ok, Payload} = whistle_api:resource_resp([ KV || {_, V}=KV <- Response, V =/= undefined ]),
    amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload).

respond_resource_failed(BridgeResp, Attempts, JObj) ->
    Q = wh_json:get_value(<<"Server-ID">>, BridgeResp, <<>>),
    Response = [
                 {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                ,{<<"Failed-Attempts">>, whistle_util:to_binary(Attempts)}
                ,{<<"Failure-Message">>,
                  wh_json:get_value(<<"Application-Response">>, BridgeResp, wh_json:get_value(<<"Hangup-Cause">>, BridgeResp))}
                ,{<<"Failure-Code">>, wh_json:get_value(<<"Hangup-Code">>, BridgeResp)}
               | whistle_api:default_headers(Q, <<"resource">>, <<"resource_error">>, ?APP_NAME, ?APP_VERSION)
            ],
    {ok, Payload} = whistle_api:resource_error([ KV || {_, V}=KV <- Response, V =/= undefined ]),
    amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload).
