%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Listen for outbound route requests and processes them
%%% against the resources db
%%% @end
%%% Created : 14 June 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(stepswitch_outbound).

-behaviour(gen_server).

-include("stepswitch.hrl").

%% API
-export([start_link/0]).

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
          ,bypass_media = undefined
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

-type endpoint() :: tuple(1..100, non_neg_integer(), binary(), []|[#gateway{}]).
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
    ?LOG_SYS("starting new stepswitch outbound responder"),
    ?LOG_SYS("ensuring database ~s exists", [?RESOURCES_DB]),
    couch_mgr:db_create(?RESOURCES_DB),
    couch_mgr:revise_views_from_folder(?RESOURCES_DB, stepswitch),
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
                  Num = wh_util:to_e164(wh_util:to_binary(Number)),
                  EPs = print_endpoints(evaluate_number(Num, Resrcs), 0, []),
                  gen_server:reply(From, EPs)
          end),
    {noreply, State};

handle_call({process_number, Number, Flags}, From, #state{resrcs=R1}=State) ->
    spawn(fun() ->
                  R2 = evaluate_flags(Flags, R1),
                  Num = wh_util:to_e164(wh_util:to_binary(Number)),
                  EPs = print_endpoints(evaluate_number(Num, R2), 0, []),
                  gen_server:reply(From, EPs)
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
            couch_mgr:rm_change_handler(?RESOURCES_DB, DocId),
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
-spec start_amqp/0 :: () -> tuple(ok, binary()) | tuple(error, amqp_error).
start_amqp() ->
    try
        %% control egress of messages from the queue, only fetch one at time (load balances)
	{'basic.qos_ok'} = amqp_util:basic_qos(1),
        _ = amqp_util:resource_exchange(),
        Q = amqp_util:new_resource_queue(),
        amqp_util:bind_q_to_resource(Q, ?KEY_OFFNET_RESOURCE_REQ),
        amqp_util:basic_consume(Q, [{exclusive, false}]),
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
-spec process_req/3 :: (MsgType, JObj, State) -> no_return() when
      MsgType :: tuple(binary(), binary()),
      JObj :: json_object(),
      State :: #state{}.
process_req({<<"resource">>, <<"offnet_req">>}, JObj, #state{resrcs=R1}) ->
    whapps_util:put_callid(JObj),
    CallId = get(callid),

    <<"audio">> = wh_json:get_value(<<"Resource-Type">>, JObj),

    Number = wh_json:get_value(<<"To-DID">>, JObj),

    ?LOG_START("off-net resource bridge request to ~s for account ~s", [Number, wh_json:get_value(<<"Account-ID">>, JObj)]),

    Q = amqp_util:new_queue(),
    amqp_util:bind_q_to_callevt(Q, CallId),
    amqp_util:basic_consume(Q),

    BridgeReq = try
                    {ok, AccountId, false} = gen_server:call(stepswitch_inbound, {lookup_number, Number}),
                    ?LOG("number belongs to another on-net account, loopback back to account ~s", [AccountId]),
                    build_loopback_request(JObj, Number, Q)
                catch
                    _:_ ->
                        EPs = case wh_json:get_value(<<"Flags">>, JObj) of
                                  undefined -> evaluate_number(Number, R1);
                                  Flags ->
                                      [?LOG("resource must have ~s flag", [F]) || F <- Flags],
                                      R2 = evaluate_flags(Flags, R1),
                                      evaluate_number(Number, R2)
                              end,
                        build_bridge_request(JObj, EPs, Q)
                end,

    case length(wh_json:get_value(<<"Endpoints">>, BridgeReq, [])) of
        0 ->
            ?LOG_END("no offnet resources found for request, sending failure response"),
            respond_resource_failed({struct, [
                                               {<<"Hangup-Cause">>, <<"NO_RESOURCES">>}
                                              ,{<<"Hangup-Code">>, <<"sip:404">>}
                                             ]}, 0, JObj);
        Attempts ->
            {ok, Payload} = wh_api:bridge_req({struct, BridgeReq}),
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
                    ?LOG_END("internal resource bridge error"),
                    respond_erroneously(ErrorResp, JObj)
            end
    end;

process_req({_, _}, _, _) ->
    {error, invalid_event}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Gets a list of all active resources from the DB
%% @end
%%--------------------------------------------------------------------
-spec get_resrcs/0 :: () -> [#resrc{}].
get_resrcs() ->
    case couch_mgr:get_results(?RESOURCES_DB, ?LIST_RESOURCES_BY_ID, [{<<"include_docs">>, true}]) of
        {ok, Resrcs} ->
            [couch_mgr:add_change_handler(?RESOURCES_DB, wh_json:get_value(<<"id">>, R))
             || R <- Resrcs],
            [create_resrc(wh_json:get_value(<<"doc">>, R))
             || R <- Resrcs, wh_util:is_true(wh_json:get_value([<<"doc">>, <<"enabled">>], R, true))];
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
-spec update_resrc/2 :: (DocId, Resrcs) -> [#resrc{}] when
      DocId :: binary(),
      Resrcs :: [#resrc{}].
update_resrc(DocId, Resrcs) ->
    ?LOG_SYS("received notification that resource ~s has changed", [DocId]),
    case couch_mgr:open_doc(?RESOURCES_DB, DocId) of
        {ok, JObj} ->
            case wh_util:is_true(wh_json:get_value(<<"enabled">>, JObj)) of
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
            couch_mgr:rm_change_handler(?RESOURCES_DB, DocId),
            lists:keydelete(DocId, #resrc.id, Resrcs)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a resrc JSON object it builds a resrc record and
%% populates it with all enabled gateways
%% @end
%%--------------------------------------------------------------------
-spec create_resrc/1 :: (JObj) -> #resrc{} when
      JObj :: json_object().
create_resrc(JObj) ->
    Default = #resrc{},
    Id = wh_json:get_value(<<"_id">>, JObj),
    ?LOG_SYS("loading resource ~s", [Id]),
    #resrc{id = Id
           ,rev =
               wh_json:get_value(<<"_rev">>, JObj)
           ,weight_cost =
               constrain_weight(wh_json:get_value(<<"weight_cost">>, JObj, Default#resrc.weight_cost))
           ,grace_period =
               wh_util:to_integer(wh_json:get_value(<<"grace_period">>, JObj, Default#resrc.grace_period))
           ,flags =
               wh_json:get_value(<<"flags">>, JObj, Default#resrc.flags)
           ,rules =
               [R2 || R1 <- wh_json:get_value(<<"rules">>, JObj, Default#resrc.rules)
                          ,(R2 = compile_rule(R1, Id)) =/= error]
           ,gateways =
               [create_gateway(G, Id) || G <- wh_json:get_value(<<"gateways">>, JObj, []),
                                         wh_util:is_true(wh_json:get_value(<<"enabled">>, G, true))]
          }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a gateway JSON object it builds a gateway record
%% @end
%%--------------------------------------------------------------------
-spec create_gateway/2 :: (JObj, Id) -> #gateway{} when
      JObj :: json_object(),
      Id :: binary().
create_gateway(JObj, Id) ->
    Default = #gateway{},
    #gateway{resource_id = Id
             ,server =
                 wh_json:get_value(<<"server">>, JObj, Default#gateway.server)
             ,realm =
                 wh_json:get_value(<<"realm">>, JObj, Default#gateway.realm)
             ,username =
                 wh_json:get_value(<<"username">>, JObj, Default#gateway.username)
             ,password =
                 wh_json:get_value(<<"password">>, JObj, Default#gateway.password)
             ,route =
                 wh_json:get_value(<<"route">>, JObj, Default#gateway.route)
             ,prefix =
                 wh_util:to_binary(wh_json:get_value(<<"prefix">>, JObj, Default#gateway.prefix))
             ,suffix =
                 wh_util:to_binary(wh_json:get_value(<<"suffix">>, JObj, Default#gateway.suffix))
             ,codecs =
                 wh_json:get_value(<<"codecs">>, JObj, Default#gateway.codecs)
             ,bypass_media =
                 wh_json:get_value(<<"bypass_media">>, JObj)
             ,caller_id_type =
                 wh_json:get_value(<<"caller_id_type">>, JObj, Default#gateway.caller_id_type)
             ,sip_headers =
                 wh_json:get_value(<<"custom_sip_headers">>, JObj, Default#gateway.sip_headers)
             ,progress_timeout =
                 wh_util:to_integer(wh_json:get_value(<<"progress_timeout">>, JObj, Default#gateway.progress_timeout))
            }.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wrapper for re:compile so we can log rules that fail (including
%% which resource it was on).
%% @end
%%--------------------------------------------------------------------
-spec compile_rule/2 :: (Rule, Id) -> re:mp() | error when
      Rule :: binary(),
      Id :: binary().
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
-spec constrain_weight/1 :: (W) -> integer() when
      W :: binary() | integer().
constrain_weight(W) when not is_integer(W) ->
    constrain_weight(wh_util:to_integer(W));
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
-spec evaluate_flags/2 :: (F1, Resrcs) -> [#resrc{}] when
      F1 :: list(),
      Resrcs :: [#resrc{}].
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
-spec evaluate_number/2 :: (Number, Resrcs) -> endpoints() when
      Number :: binary(),
      Resrcs :: [#resrc{}].
evaluate_number(Number, Resrcs) ->
    sort_endpoints(get_endpoints(Number, Resrcs)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sort the gateway tuples returned by evalutate_resrcs according to
%% weight.
%% @end
%%--------------------------------------------------------------------
-spec sort_endpoints/1 :: (Endpoints) -> endpoints() when
      Endpoints :: endpoints().
sort_endpoints(Endpoints) ->
    lists:sort(fun({W1, _, _, _}, {W2, _, _, _}) ->
                       W1 =< W2
               end, Endpoints).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_endpoints/2 :: (Number, Resrcs) -> endpoints() when
      Number :: binary(),
      Resrcs :: [#resrc{}].
get_endpoints(Number, Resrcs) ->
    [Endpoint || Endpoint <- lists:map(fun(R) -> get_endpoint(Number, R) end, Resrcs)
                     ,Endpoint =/= no_match].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a gateway JSON object it builds a gateway record
%% @end
%%--------------------------------------------------------------------
-spec get_endpoint/2 :: (Number, Resrc) -> endpoint() | no_match when
      Number :: binary(),
      Resrc :: #resrc{}.
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
-spec evaluate_rules/2 :: (Regex, Number) -> tuple(ok, binary()) | tuple(error, no_match) when
      Regex :: re:mp(),
      Number:: binary().
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
-spec build_loopback_request/3 :: (JObj, Number, Q) -> proplist() when
      JObj :: json_object(),
      Number :: binary(),
      Q :: binary().
build_loopback_request(JObj, Number, Q) ->
    Endpoints = [{struct, [{<<"Invite-Format">>, <<"route">>}
                           ,{<<"Route">>, <<"loopback/", (Number)/binary>>}
                           ,{<<"Custom-Channel-Vars">>, {struct,[{<<"Offnet-Loopback-Number">>, Number}
                                                                 ,{<<"Offnet-Loopback-Account-ID">>, wh_json:get_value(<<"Account-ID">>, JObj)}
                                                                ]}}
                         ]}],
    Command = [{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, Endpoints}
               ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, JObj)}
               ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, JObj)}
               ,{<<"Outgoing-Caller-ID-Name">>, wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, JObj)}
               ,{<<"Outgoing-Caller-ID-Number">>, wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj)}
               ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, JObj)}
               %% TODO: Do not enable this feature until WHISTLE-408 is completed
               %% ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
               ,{<<"Dial-Endpoint-Method">>, <<"single">>}
               ,{<<"Continue-On-Fail">>, <<"true">>}
               ,{<<"SIP-Headers">>, wh_json:get_value(<<"SIP-Headers">>, JObj)}
               ,{<<"Custom-Channel-Vars">>, wh_json:get_value(<<"Custom-Channel-Vars">>, JObj)}
               ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    [ KV || {_, V}=KV <- Command, V =/= undefined ].


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the proplist for a whistle API bridge request from the
%% off-net request, endpoints, and our AMQP Q
%% @end
%%--------------------------------------------------------------------
-spec build_bridge_request/3 :: (JObj, Endpoints, Q) -> proplist() when
      JObj :: json_object(),
      Endpoints :: endpoints(),
      Q :: binary().
build_bridge_request(JObj, Endpoints, Q) ->
    CCVs = wh_json:set_value(<<"Account-ID">>, wh_json:get_value(<<"Account-ID">>, JObj, <<>>)
                             ,wh_json:get_value(<<"Custom-Channel-Vars">>, JObj, ?EMPTY_JSON_OBJECT)),
    Command = [{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, build_endpoints(Endpoints, 0, [])}
               ,{<<"Timeout">>, wh_json:get_value(<<"Timeout">>, JObj)}
               ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"Ignore-Early-Media">>, JObj)}
               ,{<<"Media">>, wh_json:get_value(<<"Media">>, JObj)}
               ,{<<"Outgoing-Caller-ID-Name">>, wh_json:get_value(<<"Outgoing-Caller-ID-Name">>, JObj)}
               ,{<<"Outgoing-Caller-ID-Number">>, wh_json:get_value(<<"Outgoing-Caller-ID-Number">>, JObj)}
               ,{<<"Ringback">>, wh_json:get_value(<<"Ringback">>, JObj)}
               %% TODO: Do not enable this feature until WHISTLE-408 is completed
               %% ,{<<"Dial-Endpoint-Method">>, <<"simultaneous">>}
               ,{<<"Dial-Endpoint-Method">>, <<"single">>}
               ,{<<"Continue-On-Fail">>, <<"true">>}
               ,{<<"SIP-Headers">>, wh_json:get_value(<<"SIP-Headers">>, JObj)}
               ,{<<"Custom-Channel-Vars">>, CCVs}
               ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
               | wh_api:default_headers(Q, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    [ KV || {_, V}=KV <- Command, V =/= undefined ].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the proplist for a whistle API bridge request from the
%% off-net request, endpoints, and our AMQP Q
%% @end
%%--------------------------------------------------------------------
-spec build_endpoints/3 :: (Endpoints, Delay, Acc) -> proplist() when
      Endpoints :: endpoints(),
      Delay :: non_neg_integer(),
      Acc :: proplist().
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
-spec build_endpoint/3 :: (Number, Gateway, Delay) -> json_object() when
      Number :: binary(),
      Gateway :: #gateway{},
      Delay :: non_neg_integer().
build_endpoint(Number, Gateway, Delay) ->
    Route = get_dialstring(Gateway, Number),
    ?LOG("using ~s on ~s delayed by ~b sec", [Route, Gateway#gateway.resource_id, Delay]),
    Prop = [
             {<<"Invite-Format">>, <<"route">>}
            ,{<<"Route">>, get_dialstring(Gateway, Number)}
            ,{<<"Callee-ID-Number">>, wh_util:to_binary(Number)}
            ,{<<"Caller-ID-Type">>, Gateway#gateway.caller_id_type}
            %% TODO: Do not enable this feature until WHISTLE-408 is completed
            %%,{<<"Endpoint-Delay">>, wh_util:to_binary(Delay)}
            ,{<<"Bypass-Media">>, Gateway#gateway.bypass_media}
            ,{<<"Endpoint-Progress-Timeout">>, wh_util:to_binary(Gateway#gateway.progress_timeout)}
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
%% Builds a list of tuples for humans from the lookup number request
%% @end
%%--------------------------------------------------------------------
-spec print_endpoints/3 :: (Endpoints, Delay, Acc) -> list() when
      Endpoints :: endpoints(),
      Delay :: non_neg_integer(),
      Acc :: list().
print_endpoints([], _, Acc) ->
    lists:reverse(Acc);
print_endpoints([{_, GracePeriod, Number, [Gateway]}|T], Delay, Acc0) ->
    print_endpoints(T, Delay + GracePeriod, [print_endpoint(Number, Gateway, Delay)|Acc0]);
print_endpoints([{_, GracePeriod, Number, Gateways}|T], Delay, Acc0) ->
    {D2, Acc1} = lists:foldl(fun(Gateway, {0, AccIn}) ->
                                     {2, [print_endpoint(Number, Gateway, 0)|AccIn]};
                                 (Gateway, {D0, AccIn}) ->
                                     {D0 + 2, [print_endpoint(Number, Gateway, D0)|AccIn]}
                            end, {Delay, Acc0}, Gateways),
    print_endpoints(T, D2 - 2 + GracePeriod, Acc1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds a tuple for humans from the lookup number request
%% @end
%%--------------------------------------------------------------------
-spec print_endpoint/3 :: (Number, Gateway, Delay) -> tuple(binary(), non_neg_integer(), binary()) when
      Number :: binary(),
      Gateway :: #gateway{},
      Delay :: non_neg_integer().
print_endpoint(Number, Gateway, Delay) ->
    {Gateway#gateway.resource_id, Delay, get_dialstring(Gateway, Number)}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the route dialstring
%% @end
%%--------------------------------------------------------------------
-spec get_dialstring/2 :: (Gateway, Number) -> binary() when
      Gateway :: #gateway{},
      Number :: binary().
get_dialstring(#gateway{route=undefined}=Gateway, Number) ->
    <<"sip:"
      ,(wh_util:to_binary(Gateway#gateway.prefix))/binary,
      Number/binary
      ,(wh_util:to_binary(Gateway#gateway.suffix))/binary
      ,"@", (wh_util:to_binary(Gateway#gateway.server))/binary
    >>;
get_dialstring(#gateway{route=Route}, _) ->
    Route.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% waits for the return from the bridge request
%% @end
%%--------------------------------------------------------------------
-spec wait_for_bridge/1 :: (Timeout) -> tuple(ok | fail | hungup | error, json_object())
                                            | tuple(error, timeout) when
      Timeout :: non_neg_integer().
wait_for_bridge(Timeout) ->
    Start = erlang:now(),
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
            JObj = mochijson2:decode(Payload),
            case { wh_json:get_value(<<"Application-Name">>, JObj), wh_json:get_value(<<"Event-Name">>, JObj), wh_json:get_value(<<"Event-Category">>, JObj) } of
                { _, <<"CHANNEL_BRIDGE">>, <<"call_event">> } ->
                    {ok, JObj};
                { <<"bridge">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"call_event">> } ->
		    case wh_json:get_value(<<"Application-Response">>, JObj) of
			<<"SUCCESS">> -> {ok, JObj};
			_ -> {fail, JObj}
		    end;
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% creates an offnet resource response and sends it to the tageted
%% queue of the requestor
%% @end
%%--------------------------------------------------------------------
-spec respond_bridged_to_resource/2 :: (BridgeResp, JObj) -> ok when
      BridgeResp :: json_object(),
      JObj :: json_object().
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
               | wh_api:default_headers(Q, <<"resource">>, <<"offnet_resp">>, ?APP_NAME, ?APP_VERSION)
            ],
    {ok, Payload} = wh_api:resource_resp([ KV || {_, V}=KV <- Response, V =/= undefined ]),
    amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% creates an offnet resource failure response and sends it back to
%% the targeted queue of the requestor
%% @end
%%--------------------------------------------------------------------
-spec respond_resource_failed/3 :: (BridgeResp, Attempts, JObj) -> ok when
      BridgeResp :: json_object(),
      Attempts :: non_neg_integer(),
      JObj :: json_object().
respond_resource_failed(BridgeResp, Attempts, JObj) ->
    Q = wh_json:get_value(<<"Server-ID">>, BridgeResp, <<>>),
    Response = [
                 {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                ,{<<"Failed-Attempts">>, wh_util:to_binary(Attempts)}
                ,{<<"Failure-Message">>,
                  wh_json:get_value(<<"Application-Response">>, BridgeResp, wh_json:get_value(<<"Hangup-Cause">>, BridgeResp))}
                ,{<<"Failure-Code">>, wh_json:get_value(<<"Hangup-Code">>, BridgeResp)}
                ,{<<"Hangup-Cause">>, wh_json:get_value(<<"Hangup-Cause">>, BridgeResp)}
                ,{<<"Hangup-Code">>, wh_json:get_value(<<"Hangup-Code">>, BridgeResp)}
               | wh_api:default_headers(Q, <<"resource">>, <<"resource_error">>, ?APP_NAME, ?APP_VERSION)
            ],
    {ok, Payload} = wh_api:resource_error([ KV || {_, V}=KV <- Response, V =/= undefined ]),
    _ = amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% creates a generic error response and sends it back to
%% the targeted queue of the requestor
%% @end
%%--------------------------------------------------------------------
-spec respond_erroneously/2 :: (ErrorResp, JObj) -> ok when
      ErrorResp :: json_object(),
      JObj :: json_object().
respond_erroneously(ErrorResp, JObj) ->
    Q = wh_json:get_value(<<"Server-ID">>, ErrorResp, <<>>),
    Response = [
                 {<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj, <<>>)}
                ,{<<"Error-Message">>, wh_json:get_value(<<"Error-Message">>, ErrorResp)}
               | wh_api:default_headers(Q, <<"error">>, <<"resource_error">>, ?APP_NAME, ?APP_VERSION)
            ],
    {ok, Payload} = wh_api:error_resp([ KV || {_, V}=KV <- Response, V =/= undefined ]),
    _ = amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload).
