%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP, INC
%%% @doc
%%% Make a request for authorization, and answer queries about the CallID
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_authz).

-behaviour(gen_server).

-export([start_link/1, start_link/2]).
-export([handle_channel_create/3]).
-export([handle_session_heartbeat/2]).
-export([rate_channel/2]).
-export([kill_channel/2]).
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3
        ]).

-include("ecallmgr.hrl").

-record(state, {node = 'undefined' :: atom()
                ,options = [] :: proplist()
               }).

-define(RATE_VARS, [<<"Rate">>, <<"Rate-Increment">>
                        ,<<"Rate-Minimum">>, <<"Surcharge">>
                        ,<<"Rate-Name">>, <<"Base-Cost">>
                        ,<<"Discount-Percentage">>
                   ]).

-define(HEARTBEAT_ON_ANSWER(CallId), <<"api_on_answer=uuid_session_heartbeat ", CallId/binary, " 60">>).

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
start_link(Node) ->
    start_link(Node, []).

start_link(Node, Options) ->
    gen_server:start_link(?MODULE, [Node, Options], []).

-spec kill_channel(wh_proplist(), atom()) -> 'ok'.
-spec kill_channel(ne_binary(), ne_binary(), atom()) -> 'ok'.

kill_channel(Props, Node) ->
    Direction = props:get_value(<<"Call-Direction">>, Props),
    CallId = props:get_value(<<"Unique-ID">>, Props),
    kill_channel(Direction, CallId, Node).

kill_channel(<<"inbound">>, CallId, Node) ->
    %% Give any pending route requests a chance to cleanly terminate this call,
    %% if it has not been processed yet.  Then chop its head off....
    timer:sleep(1000),
    _ = freeswitch:api(Node, uuid_kill, wh_util:to_list(<<CallId/binary, " INCOMING_CALL_BARRED">>)),
    ok;
kill_channel(<<"outbound">>, CallId, Node) ->
    _ = freeswitch:api(Node, uuid_kill, wh_util:to_list(<<CallId/binary, " OUTGOING_CALL_BARRED">>)),
    ok.

-spec handle_channel_create(wh_proplist(), ne_binary(), atom()) -> 'ok'.
handle_channel_create(Props, CallId, Node) ->
    put(callid, CallId),
    Authorized = maybe_authorize_channel(Props, Node),
    wh_cache:store_local(?ECALLMGR_UTIL_CACHE, ?AUTHZ_RESPONSE_KEY(CallId), Authorized).

-spec handle_session_heartbeat(wh_proplist(), atom()) -> 'ok'.
handle_session_heartbeat(Props, Node) ->
    CallId = props:get_value(<<"Unique-ID">>, Props),
    put(callid, CallId),
    lager:debug("received session heartbeat", []),
    AccountId = props:get_value(?GET_CCV(<<"Account-ID">>), Props),
    ResellerId = props:get_value(?GET_CCV(<<"Reseller-ID">>), Props),
    _ = case (not wh_util:is_empty(AccountId))
            andalso should_reauth(?GET_CCV(<<"Account-Billing">>), Props) 
        of
            false -> ok;
            AType -> 
                attempt_reauthorization(account, AccountId, AType, Props, Node)
        end,
    _ = case (not wh_util:is_empty(ResellerId))
            andalso AccountId =/= ResellerId 
            andalso should_reauth(?GET_CCV(<<"Reseller-Billing">>), Props) 
        of
            false -> ok;
            RType ->
                attempt_reauthorization(reseller, ResellerId, RType, Props, Node)
        end,    
    ok.

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
init([Node, Options]) ->
    put(callid, Node),
    lager:info("starting new fs authz listener for ~s", [Node]),
    case bind_to_events(props:get_value(client_version, Options), Node) of
        ok -> {ok, #state{node=Node, options=Options}};
        {error, Reason} ->
            lager:critical("unable to establish authz bindings: ~p", [Reason]),
            {stop, Reason}
    end.

bind_to_events(<<"mod_kazoo", _/binary>>, Node) ->
%%    case freeswitch:event(Node, ['CHANNEL_CREATE', 'SESSION_HEARTBEAT']) of
    case freeswitch:event(Node, ['CHANNEL_CREATE']) of
        timeout -> {error, timeout};
        Else -> Else
    end;
bind_to_events(_, Node) ->
    case gproc:reg({p, l, {event, Node, <<"CHANNEL_CREATE">>}}) =:= true
        andalso gproc:reg({p, l, {event, Node, <<"SESSION_HEARTBEAT">>}}) =:= true
    of
        true -> ok;
        _ -> {error, gproc_badarg}
    end.
            
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
handle_info({event, [CallId | Props]}, #state{node=Node}=State) ->
    _ = case props:get_value(<<"Event-Name">>, Props) of
            <<"SESSION_HEARTBEAT">> -> spawn(?MODULE, handle_session_heartbeat, [Props, Node]);
            <<"CHANNEL_CREATE">> -> spawn(?MODULE, handle_channel_create, [Props, CallId, Node]);
            _ -> ok
        end,
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
terminate(_Reason, #state{node=Node}) ->
    lager:info("authz listener for ~s terminating: ~p", [Node, _Reason]).

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
-spec maybe_authorize_channel(wh_proplist(), atom()) -> boolean().
maybe_authorize_channel(Props, Node) ->
    CallId = props:get_value(<<"Unique-ID">>, Props),
    case wh_util:is_true(props:get_value(<<"variable_recovered">>, Props)) of
        true -> allow_call(Props, CallId, Node);
        false -> is_authz_enabled(Props, CallId, Node)
    end.

-spec is_authz_enabled(wh_proplist(), ne_binary(), atom()) -> boolean().
is_authz_enabled(Props, CallId, Node) ->
    case wh_util:is_true(ecallmgr_config:get(<<"authz_enabled">>, false)) of
        true ->
            is_global_resource(Props, CallId, Node);
        false ->
            lager:debug("config ecallmgr.authz is disabled", []),
            allow_call(Props, CallId, Node)
    end.

-spec is_global_resource(wh_proplist(), ne_binary(), atom()) -> boolean().
is_global_resource(Props, CallId, Node) ->
    GlobalResource = props:get_value(?GET_CCV(<<"Global-Resource">>), Props, true),
    case wh_util:is_true(GlobalResource) of
        true -> is_consuming_resource(Props, CallId, Node);
        false -> 
            lager:debug("channel is a local resource", []),
            allow_call(Props, CallId, Node)
    end.
        
-spec is_consuming_resource(wh_proplist(), ne_binary(), atom()) -> boolean().
is_consuming_resource(Props, CallId, Node) ->
    case props:get_value(<<"Call-Direction">>, Props) of
        <<"outbound">> ->
            case props:get_value(?GET_CCV(<<"Resource-ID">>), Props) =/= undefined of
                true -> set_heartbeat_on_answer(Props, CallId, Node);
                false ->
                    lager:debug("outbound channel is not consuming a resource", []),
                    allow_call(Props, CallId, Node) 
            end;
        <<"inbound">> ->
            case props:get_value(?GET_CCV(<<"Authorizing-ID">>), Props) =:= undefined of
                true -> set_heartbeat_on_answer(Props, CallId, Node);
                false ->
                    lager:debug("inbound channel is not consuming a resource", []),
                    allow_call(Props, CallId, Node)
            end
    end.

-spec set_heartbeat_on_answer(wh_proplist(), ne_binary(), atom()) -> boolean().
set_heartbeat_on_answer(Props, CallId, Node) ->
    %% Ensure that even if the call is answered while we are authorizing it
    %% the session will hearbeat.
    'ok' = ecallmgr_util:send_cmd(Node, CallId, "set", ?HEARTBEAT_ON_ANSWER(CallId)),
    ensure_account_id_exists(Props, CallId, Node).

-spec ensure_account_id_exists(wh_proplist(), ne_binary(), atom()) -> boolean().
ensure_account_id_exists(Props, CallId, Node) ->
    case props:get_value(?GET_CCV(<<"Account-ID">>), Props) of
        undefined ->
            case identify_account(undefined, Props) of
                {error, not_required} ->
                    allow_call(Props, CallId, Node);
                {error, _R} -> 
                    lager:debug("unable to determine the account id: ~p", [_R]),
                    maybe_deny_call(Props, CallId, Node);
                {ok, Resp} -> 
                    update_account_id(Resp, Props, CallId, Node)
            end;
        _Else ->
            authorize_account(Props, CallId, Node)
    end.

-spec update_account_id(wh_proplist(), wh_proplist(), ne_binary(), atom()) -> boolean().
update_account_id(Resp, Props, CallId, Node) ->
    case props:get_value(?GET_CCV(<<"Account-ID">>), Resp) of
        undefined -> update_reseller_id(Resp, Props, CallId, Node);
        AccountId ->
            'ok' = ecallmgr_util:send_cmd(Node, CallId, "export", ?SET_CCV(<<"Account-ID">>, AccountId)),
            update_reseller_id(Resp, [{?GET_CCV(<<"Account-ID">>), AccountId}|Props], CallId, Node)
    end.

-spec update_reseller_id(wh_proplist(), wh_proplist(), ne_binary(), atom()) -> boolean().
update_reseller_id(Resp, Props, CallId, Node) ->
    case props:get_value(?GET_CCV(<<"Reseller-ID">>), Resp) of
        undefined -> authorize_account(Props, CallId, Node);
        ResellerId ->
            'ok' = ecallmgr_util:send_cmd(Node, CallId, "set", ?SET_CCV(<<"Reseller-ID">>, ResellerId)),
            authorize_account([{?GET_CCV(<<"Reseller-ID">>), ResellerId}|Props], CallId, Node)
    end.

-spec authorize_account(wh_proplist(), ne_binary(), atom()) -> boolean().
authorize_account(Props, CallId, Node) ->
    AccountId = props:get_value(?GET_CCV(<<"Account-ID">>), Props),
    case authorize(AccountId, Props) of
        {error, _R} ->
            lager:debug("failed to authorize account ~s: ~p", [AccountId, _R]),
            maybe_deny_call(Props, CallId, Node);
        {ok, Type} ->
            lager:debug("call authorized by account ~s as ~s", [AccountId, Type]),
            'ok' = ecallmgr_util:send_cmd(Node, CallId, "set", ?SET_CCV(<<"Account-Billing">>, Type)),
            authorize_reseller(Props, CallId, Node)
    end.

-spec authorize_reseller(wh_proplist(), ne_binary(), atom()) -> boolean().
authorize_reseller(Props, CallId, Node) ->
    ResellerId = props:get_value(?GET_CCV(<<"Reseller-ID">>), Props),
    AccountId = props:get_value(?GET_CCV(<<"Account-ID">>), Props),
    case AccountId =:= ResellerId orelse authorize(ResellerId, Props) of
        {error, account_limited} -> 
            lager:debug("reseller has no remaining resources", []),
            maybe_deny_call(Props, CallId, Node);
        {ok, Type} ->
            lager:debug("call authorized by reseller ~s as ~s", [ResellerId, Type]),
            'ok' = ecallmgr_util:send_cmd(Node, CallId, "set", ?SET_CCV(<<"Reseller-Billing">>, Type)),
            rate_call(Props, CallId, Node);
        _Else ->
            rate_call(Props, CallId, Node)
    end.
        
-spec rate_call(wh_proplist(), ne_binary(), atom()) -> 'true'.
rate_call(Props, CallId, Node) ->
    spawn(?MODULE, rate_channel, [Props, Node]),
    allow_call(Props, CallId, Node).

-spec allow_call(wh_proplist(), ne_binary(), atom()) -> 'true'.
allow_call(_, _, _) ->
    lager:debug("channel authorization succeeded, allowing call", []),
    true.

-spec maybe_deny_call(wh_proplist(), api_binary(), atom()) -> boolean().
maybe_deny_call(Props, _, Node) ->
    DryRun = wh_util:is_true(ecallmgr_config:get(<<"authz_dry_run">>, false)),
    _ = DryRun orelse spawn(?MODULE, kill_channel, [Props, Node]),
    DryRun orelse false.

-spec rate_channel(wh_proplist(), atom()) -> 'ok'.
rate_channel(Props, Node) ->
    CallId = props:get_value(<<"Unique-ID">>, Props),
    put(callid, CallId),
    lager:debug("sending rate request"),
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,rating_req(CallId, Props)
                                  ,fun wapi_rate:publish_req/1
                                  ,fun wapi_rate:resp_v/1
                                 ),
    case ReqResp of
        {error, _R} -> lager:debug("rate request lookup failed: ~p", [_R]);
        {ok, RespJObj} -> set_rating_ccvs(RespJObj, Node)
    end.

-spec authorize(api_binary(), wh_proplist()) ->
                             {'ok', ne_binary()} |
                             {'error', 'account_limited'} |
                             {'error', 'default_is_deny'}.
authorize(undefined, _) -> {error, no_account};
authorize(AccountId, Props) ->
    lager:debug("channel authorization request started"),
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,authz_req(AccountId, Props)
                                  ,fun wapi_authz:publish_authz_req/1
                                  ,fun wapi_authz:authz_resp_v/1
                                  ,5000),
    case ReqResp of
        {error, _R} ->
            lager:debug("authz request lookup failed: ~p", [_R]),
            authz_default();
        {ok, RespJObj} ->
            case wh_util:is_true(wh_json:get_value(<<"Is-Authorized">>, RespJObj)) of
                false -> {error, account_limited};
                true -> {ok, wh_json:get_value(<<"Type">>, RespJObj)}
            end
    end.

-spec reauthorize(api_binary(), ne_binary(), wh_proplist()) ->
                               {'ok', wh_json:object()} |
                               {'error', _}.
reauthorize(undefined, _, _) -> {error, no_account};
reauthorize(AccountId, Type, Props) ->
    lager:debug("channel reauthorization request started"),
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,reauthz_req(AccountId, Type, Props)
                                  ,fun wapi_authz:publish_reauthz_req/1
                                  ,fun wapi_authz:reauthz_resp_v/1
                                  ,5000),
    case ReqResp of
        {error, _R} ->
            lager:debug("reauthz request lookup failed: ~p", [_R]),
            authz_default();
        {ok, RespJObj} ->
            case wh_util:is_true(wh_json:get_value(<<"Is-Authorized">>, RespJObj)) of
                false -> {error, account_limited};
                true -> {ok, RespJObj}
            end
    end.

-spec identify_account(api_binary(), wh_proplist()) ->
                                    {'ok', wh_proplist()} |
                                    {'error', 'unidentified_channel' | 'not_required'}.
identify_account(_, Props) ->
    lager:debug("requesting account identification"),
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,authz_identify_req(Props)
                                  ,fun wapi_authz:publish_identify_req/1
                                  ,fun wapi_authz:identify_resp_v/1
                                  ,5000),
    case ReqResp of
        {error, _R} ->
            lager:debug("authz identify request lookup failed: ~p", [_R]),
            {error, unidentified_channel};
        {ok, RespJObj} ->
            case wh_json:is_false(<<"Global-Resource">>, RespJObj) of
                true -> 
                    lager:debug("identified channel as a local resource, allowing", []),
                    {error, not_required};
                false ->
                    {ok, [{?GET_CCV(<<"Account-ID">>), wh_json:get_value(<<"Account-ID">>, RespJObj)}
                          ,{?GET_CCV(<<"Reseller-ID">>), wh_json:get_value(<<"Reseller-ID">>, RespJObj)}
                          |Props
                         ]}
            end
    end.

-spec attempt_reauthorization('account'|'reseller', api_binary(), ne_binary(), wh_proplist(), atom()) -> 'ok'.
attempt_reauthorization(Billing, AccountId, Type, Props, Node) ->
    case reauthorize(AccountId, Type, Props) of
        {error, account_limited} -> 
            maybe_deny_call(Props, undefined, Node);
        {ok, JObj} ->
            CallId = props:get_value(<<"Unique-ID">>, Props),
            NewType = wh_json:get_value(<<"Type">>, JObj, Type),
            maybe_update_billing_type(Billing, Type, NewType, CallId, Node),
            CCVs = wh_json:get_ne_value(<<"Custom-Channel-Vars">>, JObj),
            maybe_update_ccvs(CCVs, CallId, Node);
        _Else -> ok
    end.

-spec maybe_update_ccvs('undefined' | wh_json:object(), ne_binary(), atom()) -> 'ok'.
maybe_update_ccvs(undefined, _, _) ->
    ok;
maybe_update_ccvs(CCVs, CallId, Node) ->
    ChannelVars = wh_json:to_proplist(CCVs),
    ecallmgr_util:set(Node, CallId, ChannelVars).

-spec maybe_update_billing_type(account|reseller, ne_binary(), ne_binary(), ne_binary(), atom()) -> 'ok'.
maybe_update_billing_type(_, Type, Type, _, _) ->
    ok;
maybe_update_billing_type(account, _, Type, CallId, Node) ->
    ecallmgr_util:send_cmd(Node, CallId, "set", ?SET_CCV(<<"Account-Billing">>, Type));
maybe_update_billing_type(reseller, _, Type, CallId, Node) ->
    ecallmgr_util:send_cmd(Node, CallId, "set", ?SET_CCV(<<"Reseller-Billing">>, Type)).
        
-spec should_reauth(ne_binary(), proplist()) -> ne_binary() | 'false'.
should_reauth(BillingVar, Props) ->
    case props:get_value(BillingVar, Props) of
        <<"allotment">> = A -> A;
        <<"per_minute">> = P -> P;
        _ -> false
    end.

-spec authz_default() -> {'ok', ne_binary()} |
                               {'error', 'account_limited'}.
authz_default() ->
    case ecallmgr_config:get(<<"authz_default_action">>, <<"deny">>) of
        <<"deny">> -> {error, account_limited};
        _Else ->
            DefaultType = ecallmgr_config:get(<<"authz_default_type">>, <<"per_minute">>),
            lager:debug("authorizing channel as config ecallmgr.authz_default_type: '~s'", [DefaultType]),
            {ok, DefaultType}
    end.

-spec set_rating_ccvs(wh_json:object(), atom()) -> 'ok'.
set_rating_ccvs(JObj, Node) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put(callid, CallId),
    lager:debug("setting rating information", []),
    Multiset = lists:foldl(fun(Key, Acc) ->
                                   case wh_json:get_binary_value(Key, JObj) of
                                       undefined -> Acc;
                                       Value ->
                                           <<"|", (?SET_CCV(Key, Value))/binary, Acc/binary>>
                                   end
                           end, <<>>, ?RATE_VARS),
    'ok' = ecallmgr_util:send_cmd(Node, CallId, "multiset", <<"^^", Multiset/binary>>).

-spec authz_req(ne_binary(), wh_proplist()) -> wh_proplist().
authz_req(AccountId, Props) ->
    [{<<"Caller-ID-Name">>, props:get_value(<<"Caller-Caller-ID-Name">>, Props, <<"noname">>)}
     ,{<<"Caller-ID-Number">>, props:get_value(<<"Caller-Caller-ID-Number">>, Props, <<"0000000000">>)}
     ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
     ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
     ,{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
     ,{<<"Call-ID">>, props:get_value(<<"Unique-ID">>, Props)}
     ,{<<"Account-ID">>, AccountId}
     ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Props)}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(ecallmgr_util:custom_channel_vars(Props))}
     ,{<<"Usage">>, ecallmgr_fs_nodes:account_summary(AccountId)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec reauthz_req(ne_binary(), ne_binary(), wh_proplist()) -> wh_proplist().
reauthz_req(AccountId, Type, Props) ->
    [{<<"Timestamp">>, get_time_value(<<"Event-Date-Timestamp">>, Props)}
     ,{<<"Caller-ID-Name">>, props:get_value(<<"Caller-Caller-ID-Name">>, Props, <<"noname">>)}
     ,{<<"Caller-ID-Number">>, props:get_value(<<"Caller-Caller-ID-Number">>, Props, <<"0000000000">>)}
     ,{<<"Type">>, Type}
     ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
     ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
     ,{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
     ,{<<"Call-ID">>, props:get_value(<<"Unique-ID">>, Props)}
     ,{<<"Account-ID">>, AccountId}
     ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Props)}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(ecallmgr_util:custom_channel_vars(Props))}
     ,{<<"Created-Time">>, get_time_value(<<"Caller-Channel-Created-Time">>, Props)}
     ,{<<"Answered-Time">>, get_time_value(<<"Caller-Channel-Answered-Time">>, Props)}
     ,{<<"Progress-Time">>, get_time_value(<<"Caller-Channel-Progress-Time">>, Props)}
     ,{<<"Progress-Media-Time">>, get_time_value(<<"Caller-Channel-Progress-Media-Time">>, Props)}
     ,{<<"Hangup-Time">>, get_time_value(<<"Caller-Channel-Hangup-Time">>, Props)}
     ,{<<"Transfer-Time">>, get_time_value(<<"Caller-Channel-Transfer-Time">>, Props)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec authz_identify_req(wh_proplist()) -> wh_proplist().
authz_identify_req(Props) ->
    [{<<"Caller-ID-Name">>, props:get_value(<<"variable_effective_caller_id_name">>, Props,
                                            props:get_value(<<"Caller-Caller-ID-Name">>, Props, <<"Unknown">>))}
     ,{<<"Caller-ID-Number">>, props:get_value(<<"variable_effective_caller_id_number">>, Props,
                                               props:get_value(<<"Caller-Caller-ID-Number">>, Props, <<"0000000000">>))}
     ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
     ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
     ,{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
     ,{<<"From-Network-Addr">>, props:get_value(<<"Caller-Network-Addr">>, Props)}
     ,{<<"Call-ID">>, props:get_value(<<"Unique-ID">>, Props)}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(ecallmgr_util:custom_channel_vars(Props))}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec rating_req(ne_binary(), wh_proplist()) -> wh_proplist().
rating_req(CallId, Props) ->
    AccountId = props:get_value(<<"variable_", ?CHANNEL_VAR_PREFIX, "Account-ID">>, Props),
    [{<<"To-DID">>, props:get_value(<<"Caller-Destination-Number">>, Props)}
     ,{<<"From-DID">>, props:get_value(<<"variable_effective_caller_id_number">>, Props
                                       ,props:get_value(<<"Caller-Caller-ID-Number">>, Props))}
     ,{<<"Call-ID">>, CallId}
     ,{<<"Account-ID">>, AccountId}
     ,{<<"Direction">>, props:get_value(<<"Call-Direction">>, Props)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec get_time_value(ne_binary(), wh_proplist()) -> non_neg_integer().
get_time_value(Key, Props) ->
    V = props:get_value(Key, Props, 0),
    wh_util:unix_seconds_to_gregorian_seconds(wh_util:microseconds_to_seconds(V)).
