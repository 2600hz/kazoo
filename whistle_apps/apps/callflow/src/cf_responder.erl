%%%============================================================================
%%% @author Vladimir Darmin <vova@2600hz.org>
%%% @copyright (C) 2011, Vladimir Darmin
%%% @doc
%%% Callflow route handler, listens to route requests and responds to
%%% inbound calls with valid callflows
%%%
%%% @end
%%% Created :      3  Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%% Last Modified: 17 Feb 2011 by Vladimir Darmin <vova@2600hz.org>
%%%============================================================================
-module(cf_responder).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-import(logger, [format_log/3]).

-include("callflow.hrl").

-define(SERVER, ?MODULE).

-record(state, {
            self :: pid()
	   ,amqp_q = <<>> :: binary()
         }).

%%-----------------------------------------------------------------------------
%% PUBLIC API
%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @public
%% @doc
%% Starts the server
%%
%% @end
%%------------------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%-----------------------------------------------------------------------------
%% GEN SERVER CALLBACKS
%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @end
%%------------------------------------------------------------------------------
init([]) ->
    ?LOG_SYS("starting new callflow responder"),
    ?LOG("ensuring callflow views exist in all accounts"),
    spawn(fun() -> whapps_util:revise_whapp_views_in_accounts(callflow) end),
    {ok, #state{self=self()}, 0}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handles call messages
%%
%% @end
%%------------------------------------------------------------------------------
handle_call({find_flow, Number, AccountId}, From, State) ->
    spawn(fun() ->
                  case lookup_callflow(#cf_call{request_user=Number, account_id=AccountId}) of
                      {ok, _, _}=OK ->
                          gen_server:reply(From, OK);
                      {error, _} ->
                          gen_server:reply(From, {error, not_found})
                  end
          end),
    {noreply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handles cast messages
%%
%% @end
%%------------------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Handles all non call/cast messages
%%
%% @end
%%------------------------------------------------------------------------------
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
                  _ = process_req(whapps_util:get_event_type(JObj), JObj, State)
          end),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Is called by a gen_server when it is about to terminate. It should be the
%% opposite of Module:init/1 and do any necessary cleaning up. When it returns,
%% the gen_server terminates with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
terminate( _Reason, _State) ->
    ?LOG_SYS("callflow responder ~p termination", [_Reason]),
    ok.

%%------------------------------------------------------------------------------
%% @private
%% @doc
%% Converts process state when code is changed
%%
%% @end
%%------------------------------------------------------------------------------
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
-spec start_amqp/0 :: () -> tuple(ok, binary()).
start_amqp() ->
    try
        _ = amqp_util:callmgr_exchange(),
        _ = amqp_util:targeted_exchange(),
        _ = amqp_util:callevt_exchange(),
        Q = amqp_util:new_callmgr_queue(<<>>),
        amqp_util:bind_q_to_callmgr(Q, ?KEY_ROUTE_REQ),
        amqp_util:bind_q_to_targeted(Q),
        amqp_util:basic_consume(Q),
        ?LOG_SYS("connected to AMQP"),
        {ok, Q}
    catch
        _:R ->
            ?LOG_SYS("failed to connect to AMQP ~p", [R]),
            {error, amqp_error}
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% process AMQP request
%% @end
%%-----------------------------------------------------------------------------
-spec process_req/3 :: (MsgType, JObj, State) -> no_return() when
      MsgType :: tuple(binary(), binary()),
      JObj :: json_object(),
      State :: #state{}.
process_req({<<"dialplan">>, <<"route_req">>}, JObj, #state{amqp_q=Q}) ->
    case wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Account-ID">>], JObj) of
        undefined ->
            ok;
        AccountId ->
            CVs = wh_json:get_value(<<"Custom-Channel-Vars">>, JObj),

            ?LOG_START("received route request"),

            Request = wh_json:get_value(<<"Request">>, JObj, <<"nouser@norealm">>),
            From = wh_json:get_value(<<"From">>, JObj, <<"nouser@norealm">>),
            To = wh_json:get_value(<<"To">>, JObj, <<"nouser@norealm">>),

            [ToUser, ToRealm] = binary:split(To, <<"@">>),
            [FromUser, FromRealm] = binary:split(From, <<"@">>),
            [RequestUser, RequestRealm] = binary:split(Request, <<"@">>),

            Call = #cf_call{call_id = get(callid)
                            ,bdst_q = Q
                            ,cid_name = wh_json:get_value(<<"Caller-ID-Name">>, JObj)
                            ,cid_number = wh_json:get_value(<<"Caller-ID-Number">>, JObj)
                            ,request = Request
                            ,request_user = wh_util:to_e164(RequestUser)
                            ,request_realm = RequestRealm
                            ,from = From
                            ,from_user = FromUser
                            ,from_realm = FromRealm
                            ,to = To
                            ,to_user = ToUser
                            ,to_realm = ToRealm
                            ,inception = wh_json:get_value(<<"Inception">>, CVs)
                            ,account_id = AccountId
                            ,authorizing_id = wh_json:get_value(<<"Authorizing-ID">>, CVs)
                            ,channel_vars = CVs
                            ,inception_during_transfer = wh_json:is_true(<<"During-Transfer">>, JObj)
                           },
            fulfill_call_request(Call, JObj)
    end;

process_req({_, <<"route_win">>}, JObj, #state{self=Self}) ->
    ?LOG_START("received route win"),

    {ok, #cf_call{request_user=Number, account_id=AccountId, no_match=NoMatch, authorizing_id=AuthId}=C1}
        = wh_cache:fetch({cf_call, get(callid)}),
    {ok, Flow} = case NoMatch of
                     true -> wh_cache:fetch({cf_flow, ?NO_MATCH_CF, AccountId});
                     false -> wh_cache:fetch({cf_flow, Number, AccountId})
                 end,
    ?LOG("fetched call record and flow from cache"),

    C2 = C1#cf_call{cf_responder = Self
                    ,ctrl_q = wh_json:get_value(<<"Control-Queue">>, JObj)
                    ,account_db = whapps_util:get_db_name(AccountId, encoded)
                    ,capture_group = wh_json:get_value(<<"capture_group">>, Flow)
                   },
    ?LOG_END("imported custom channel vars, starting callflow"),
    case AuthId of
        undefined ->
            cf_call_command:set(undefined
                                ,{struct, [{<<"Transferred">>, <<"false">>}]}
                                ,C2);
        _ ->
            ?LOG("set transfer fallback to ~s", [AuthId]),
            cf_call_command:set(undefined
                                ,{struct, [{<<"Transferred">>, <<"false">>}
                                           ,{<<"Transfer-Fallback">>, AuthId}]}
                                ,C2)
    end,
    cf_exe_sup:start_proc(C2, wh_json:get_value(<<"flow">>, Flow));

process_req({_, _}, _, _) ->
    {error, invalid_event}.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% attempt to fulfill authorized call requests
%% @end
%%-----------------------------------------------------------------------------
-spec fulfill_call_request/2 :: (Call, JObj) -> no_return() when
      Call :: #cf_call{},
      JObj :: json_object().
fulfill_call_request(Call, JObj) ->
    case lookup_callflow(Call) of
        {ok, Flow, NoMatch} ->
            FlowId = wh_json:get_value(<<"_id">>, Flow),
            ?LOG("callflow ~s satisfies request", [FlowId]),
            wh_cache:store({cf_call, get(callid)}, Call#cf_call{flow_id = FlowId, no_match = NoMatch}, 5),
            _ = send_route_response(Call, JObj);
        {error, R} ->
            ?LOG_END("unable to find callflow ~w", [R])
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% lookup the callflow based on the requested number in the account
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_callflow/1 :: (Call) -> tuple(ok, binary(), boolean()) | tuple(error, atom()) when
      Call :: #cf_call{}.
lookup_callflow(#cf_call{request_user=Number, account_id=AccountId}=Call) ->
    ?LOG("lookup callflow for ~s in account ~s", [Number, AccountId]),
    case wh_cache:fetch({cf_flow, Number, AccountId}) of
	{ok, Flow} ->
	    {ok, Flow, Number =:= ?NO_MATCH_CF};
	{error, not_found} ->
            Options = [{<<"key">>, Number}, {<<"include_docs">>, true}],
            Db = whapps_util:get_db_name(AccountId, encoded),
	    case couch_mgr:get_results(Db, ?LIST_BY_NUMBER, Options) of
		{ok, []} when Number =/= ?NO_MATCH_CF ->
                    case lookup_callflow_patterns(Call) of
                        {error, _} ->
                            lookup_callflow(Call#cf_call{request_user = ?NO_MATCH_CF});
                        {ok, {Flow, Capture}} ->
                            F = wh_json:set_value(<<"capture_group">>, Capture, Flow),
                            wh_cache:store({cf_flow, Number, AccountId}, F),
                            {ok, F, false}
                    end;
		{ok, []} ->
                    {error, not_found};
		{ok, [{struct, _}=JObj]} ->
                    Flow = wh_json:get_value(<<"doc">>, JObj),
                    wh_cache:store({cf_flow, Number, AccountId}, Flow),
		    {ok, Flow, Number =:= ?NO_MATCH_CF};
		{ok, [{struct, _}=JObj | _Rest]} ->
		    ?LOG("lookup resulted in more than one result, using the first"),
                    Flow = wh_json:get_value(<<"doc">>, JObj),
                    wh_cache:store({cf_flow, Number, AccountId}, Flow),
		    {ok, Flow, Number =:= ?NO_MATCH_CF};
                {error, _}=E ->
		    E
	    end
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% send a route response for a route request that can be fulfilled by this
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_callflow_patterns/1 :: (Call) -> tuple(ok, tuple(undefined | json_object(), binary()))
                                                 | tuple(error, atom()) when
      Call :: #cf_call{}.
lookup_callflow_patterns(#cf_call{request_user=Number, account_id=AccountId}) ->
    ?LOG("lookup callflow patterns for ~s in account ~s", [Number, AccountId]),
    Db = whapps_util:get_db_name(AccountId, encoded),
    case couch_mgr:get_results(Db, ?LIST_BY_PATTERN, [{<<"include_docs">>, true}]) of
        {ok, Patterns} ->
            case test_callflow_patterns(Patterns, Number, {undefined, <<>>}) of
                {undefined, <<>>} -> {error, not_found};
                Match -> {ok, Match}
            end;
        {error, _}=E ->
            E
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%-----------------------------------------------------------------------------
-spec test_callflow_patterns/3 :: (Patterns, Number, Result) -> tuple(undefined | json_object(), binary()) when
      Patterns :: json_objects(),
      Number :: binary(),
      Result :: tuple(undefined | json_object(), binary()).
test_callflow_patterns([], _, Result) ->
    Result;
test_callflow_patterns([Pattern|T], Number, {_, Capture}=Result) ->
    Regex = wh_json:get_value(<<"key">>, Pattern),
    case re:run(Number, Regex) of
        {match, [{Start,End}]} ->
            Match = binary:part(Number, Start, End),
            case binary:part(Number, Start, End) of
                Match when size(Match) > size(Capture) ->
                    F = wh_json:get_value(<<"doc">>, Pattern),
                    test_callflow_patterns(T, Number, {F, Match});
                _ ->
                    test_callflow_patterns(T, Number, Result)
            end;
        {match, CaptureGroups} ->
            %% find the largest matching group if present by sorting the position of the
            %% matching groups by list, reverse so head is largest, then take the head of the list
            {Start, End} = hd(lists:reverse(lists:keysort(2, tl(CaptureGroups)))),
            case binary:part(Number, Start, End) of
                Match when size(Match) > size(Result) ->
                    F = wh_json:get_value(<<"doc">>, Pattern),
                    test_callflow_patterns(T, Number, {F, Match});
                _ ->
                    test_callflow_patterns(T, Number, Result)
            end;
        _ ->
            test_callflow_patterns(T, Number, Result)
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% send a route response for a route request that can be fulfilled by this
%% process
%% @end
%%-----------------------------------------------------------------------------
-spec send_route_response/2 :: (Call, JObj) -> no_return() when
      Call :: #cf_call{},
      JObj :: json_object().
send_route_response(#cf_call{channel_vars=CVs, bdst_q=Q}, JObj) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Routes">>, []}
            ,{<<"Method">>, <<"park">>}
            ,{<<"channel_vars">>, CVs}
            | wh_api:default_headers(Q, <<"dialplan">>, <<"route_resp">>, ?APP_NAME, ?APP_VERSION)
           ],
    {ok, Payload} = wh_api:route_resp(Resp),
    amqp_util:targeted_publish(wh_json:get_value(<<"Server-ID">>, JObj), Payload),
    ?LOG_END("replied to route request").
