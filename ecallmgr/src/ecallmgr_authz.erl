%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP, INC
%%% @doc
%%% Make a request for authorization, and answer queries about the CallID
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_authz).

-export([maybe_authorize_channel/2]).
-export([update/2]).
-export([rate_channel/1]).
-export([kill_channel/2]).

-include("ecallmgr.hrl").

-define(RATE_VARS, [<<"Rate">>, <<"Rate-Increment">>
                        ,<<"Rate-Minimum">>, <<"Surcharge">>
                        ,<<"Rate-Name">>, <<"Base-Cost">>
                   ]).

-define(HEARTBEAT_ON_ANSWER(CallId), <<"api_on_answer=uuid_session_heartbeat ", CallId/binary, " 60">>).

-spec maybe_authorize_channel/2 :: (wh_proplist(), atom()) -> boolean().
maybe_authorize_channel(Props, Node) ->
    
    CallId = props:get_value(<<"Unique-ID">>, Props),
        is_authz_enabled(Props, CallId, Node).

-spec is_authz_enabled/3 :: (wh_proplist(), ne_binary(), atom()) -> boolean().
is_authz_enabled(Props, CallId, Node) ->
    
    case wh_util:is_true(ecallmgr_config:get(<<"authz_enabled">>, false)) of
        true ->
            is_global_resource(Props, CallId, Node);
                false ->            lager:debug("config ecallmgr.authz is disabled", []),
            allow_call(Props, CallId, Node)
    end.

-spec is_global_resource/3 :: (wh_proplist(), ne_binary(), atom()) -> boolean().
is_global_resource(Props, CallId, Node) ->
    GlobalResource = props:get_value(?GET_CCV(<<"Global-Resource">>), Props, true),
    case wh_util:is_true(GlobalResource) of
        true -> is_consuming_resource(Props, CallId, Node);
        false -> 
            lager:debug("channel is a local resource", []),
            allow_call(Props, CallId, Node)
    end.
        
-spec is_consuming_resource/3 :: (wh_proplist(), ne_binary(), atom()) -> boolean().
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

-spec set_heartbeat_on_answer/3 :: (wh_proplist(), ne_binary(), atom()) -> boolean().
set_heartbeat_on_answer(Props, CallId, Node) ->
    %% Ensure that even if the call is answered while we are authorizing it
    %% the session will hearbeat.
    'ok' = ecallmgr_util:send_cmd(Node, CallId, "set", ?HEARTBEAT_ON_ANSWER(CallId)),
    ensure_account_id_exists(Props, CallId, Node).

-spec ensure_account_id_exists/3 :: (wh_proplist(), ne_binary(), atom()) -> boolean().
ensure_account_id_exists(Props, CallId, Node) ->
    case props:get_value(?GET_CCV(<<"Account-ID">>), Props) of
        undefined ->
            case identify_account(undefined, Props) of
                {error, _R} -> 
                    lager:debug("unable to determine the account id: ~p", [_R]),
                    maybe_deny_call(Props, CallId, Node);
                {ok, Resp} -> 
                    update_account_id(Resp, Props, CallId, Node)
            end;
        _Else ->
            authorize_account(Props, CallId, Node)
    end.

-spec update_account_id/4 :: (wh_proplist(), wh_proplist(), ne_binary(), atom()) -> 'ok'.
update_account_id(Resp, Props, CallId, Node) ->
    case props:get_value(?GET_CCV(<<"Account-ID">>), Resp) of
        undefined -> update_reseller_id(Resp, Props, CallId, Node);
        AccountId ->
            'ok' = ecallmgr_util:send_cmd(Node, CallId, "export", ?SET_CCV(<<"Account-ID">>, AccountId)),
            update_reseller_id(Resp, [{?GET_CCV(<<"Account-ID">>), AccountId}|Props], CallId, Node)
    end.

-spec update_reseller_id/4 :: (wh_proplist(), wh_proplist(), ne_binary(), atom()) -> 'ok'.
update_reseller_id(Resp, Props, CallId, Node) ->
    case props:get_value(?GET_CCV(<<"Reseller-ID">>), Resp) of
        undefined -> authorize_account(Props, CallId, Node);
        ResellerId ->
            'ok' = ecallmgr_util:send_cmd(Node, CallId, "set", ?SET_CCV(<<"Reseller-ID">>, ResellerId)),
            authorize_account([{?GET_CCV(<<"Reseller-ID">>), ResellerId}|Props], CallId, Node)
    end.

-spec authorize_account/3 :: (wh_proplist(), ne_binary(), atom()) -> boolean().
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

-spec authorize_reseller/3 :: (wh_proplist(), ne_binary(), atom()) -> boolean().
authorize_reseller(Props, CallId, Node) ->
    ResellerId = props:get_value(?GET_CCV(<<"Reseller-ID">>), Props),
    case authorize(ResellerId, Props) of
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
        
-spec rate_call/3 :: (wh_proplist(), ne_binary(), atom()) -> boolean().
rate_call(Props, CallId, Node) ->
    spawn(?MODULE, rate_channel, [Props]),
    allow_call(Props, CallId, Node).

-spec allow_call/3 :: (wh_proplist(), ne_binary(), atom()) -> true.
allow_call(_, _, _) ->
    lager:debug("channel authorization succeeded, allowing call", []),
    true.

-spec maybe_deny_call/3 :: (wh_proplist(), ne_binary(), atom()) -> boolean().
maybe_deny_call(Props, _, Node) ->
    DryRun = wh_util:is_true(ecallmgr_config:get(<<"authz_dry_run">>, false)),
    _ = ecallmgr_util:fs_log(Node, "channel authorization failed (allowed ~s)", [DryRun]),
    _ = DryRun orelse spawn(?MODULE, kill_channel, [Props, Node]),
    DryRun orelse false.
 
-spec update/2 :: (wh_proplist(), atom()) -> 'ok'.
update(Props, Node) ->
    CallId = props:get_value(<<"Unique-ID">>, Props),
    put(callid, CallId),
    lager:debug("received session heartbeat", []),
    Update = authz_update(CallId, Props, Node),
    wapi_authz:publish_update([KV || {_, V}=KV <- Update, V =/= undefined]).

-spec rate_channel/1 :: (wh_proplist()) -> 'ok'.
rate_channel(Props) ->
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
        {ok, RespJObj} -> set_rating_ccvs(RespJObj)
    end.

-spec kill_channel/2 :: (wh_proplist(), atom()) -> 'ok'.
-spec kill_channel/3 :: (ne_binary(), ne_binary(), atom()) -> 'ok'.

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
    _ = ecallmgr_util:fs_log(Node, "whistle terminating unathorized outbound call", []),
    _ = freeswitch:api(Node, uuid_kill, wh_util:to_list(<<CallId/binary, " OUTGOING_CALL_BARRED">>)),
    ok.

-spec authorize/2 :: ('undefined' | ne_binary(), wh_proplist()) -> {'ok', ne_binary()} |
                                                                   {'error', 'account_limited'} |
                                                                   {'error', 'default_is_deny'}.
authorize(undefined, _) ->
    {error, no_account};
authorize(AccountId, Props) ->
    lager:debug("channel authorization request started"),
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,authz_req(AccountId, Props)
                                  ,fun wapi_authz:publish_req/1
                                  ,fun wapi_authz:resp_v/1
                                  ,5000),
    case ReqResp of
        {error, _R} ->
            lager:debug("authz request lookup failed: ~p", [_R]),
            authz_default();
        {ok, RespJObj} ->
            case wh_util:is_true(wh_json:get_value(<<"Is-Authorized">>, RespJObj)) of
                false -> {error, account_limited};
                true ->
                    wapi_authz:publish_win(wh_json:get_value(<<"Server-ID">>, RespJObj)
                                           ,wh_json:delete_key(<<"Event-Name">>, RespJObj)),
                    Type = wh_json:get_value(<<"Type">>, RespJObj),
                    {ok, Type}
            end
    end.

-spec identify_account/2 :: ('undefined' | ne_binary(), wh_proplist()) -> {'ok', wh_proplist()} |
                                                                          {'error', 'unidentified_channel'}.
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

-spec authz_default/0 :: () -> {'ok', ne_binary()} | {'error', 'default_is_deny'}.
authz_default() ->
    case ecallmgr_config:get(<<"authz_default_action">>, <<"deny">>) of
        <<"deny">> -> {error, default_is_deny};
        _Else ->
            DefaultType = ecallmgr_config:get(<<"authz_default_type">>, <<"flat_rate">>),
            lager:debug("authorizing channel as config ecallmgr.authz_default_type: '~s'", [DefaultType]),
            {ok, DefaultType}
    end.

-spec set_rating_ccvs/1 :: (wh_json:json_object()) -> 'ok'.
set_rating_ccvs(JObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put(callid, CallId),
    case ecallmgr_fs_nodes:channel_node(CallId) of
        {error, _} -> ok;
        {ok, Node} ->
            lager:debug("setting rating information", []),
            Multiset = lists:foldl(fun(Key, Acc) ->
                                           case wh_json:get_binary_value(Key, JObj) of
                                               undefined -> Acc;
                                               Value ->
                                                   <<"|", (?SET_CCV(Key, Value))/binary, Acc/binary>>
                                           end
                           end, <<>>, ?RATE_VARS),
            'ok' = ecallmgr_util:send_cmd(Node, CallId, "multiset", <<"^^", Multiset/binary>>),
            ok
    end.

-spec authz_req/2 :: (ne_binary(), wh_proplist()) -> wh_proplist().
authz_req(AccountId, Props) ->
    [{<<"Caller-ID-Name">>, props:get_value(<<"Caller-Caller-ID-Name">>, Props, <<"noname">>)}
     ,{<<"Caller-ID-Number">>, props:get_value(<<"Caller-Caller-ID-Number">>, Props, <<"0000000000">>)}
     ,{<<"To">>, ecallmgr_util:get_sip_to(Props)}
     ,{<<"From">>, ecallmgr_util:get_sip_from(Props)}
     ,{<<"Request">>, ecallmgr_util:get_sip_request(Props)}
     ,{<<"Call-ID">>, props:get_value(<<"Unique-ID">>, Props)}
     ,{<<"Account-ID">>, AccountId}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(ecallmgr_util:custom_channel_vars(Props))}
     ,{<<"Usage">>, ecallmgr_fs_nodes:account_summary(AccountId)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec authz_identify_req/1 :: (wh_proplist()) -> wh_proplist().
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

-spec authz_update/3 :: (ne_binary(), wh_proplist(), atom()) -> wh_proplist().
authz_update(CallId, Props, Node) ->
    [{<<"Call-ID">>, CallId}
     ,{<<"Handling-Server-Name">>, wh_util:to_binary(Node)}
     ,{<<"Timestamp">>, get_time_value(<<"Event-Date-Timestamp">>, Props)}
     ,{<<"Custom-Channel-Vars">>, wh_json:from_list(ecallmgr_util:custom_channel_vars(Props))}
     ,{<<"Caller-ID-Name">>, props:get_value(<<"variable_effective_caller_id_name">>, Props
                                             ,props:get_value(<<"Caller-Caller-ID-Name">>, Props))}
     ,{<<"Caller-ID-Number">>, props:get_value(<<"variable_effective_caller_id_number">>, Props
                                               ,props:get_value(<<"Caller-Caller-ID-Number">>, Props))}
     ,{<<"Callee-ID-Name">>, props:get_value(<<"variable_effective_callee_id_name">>, Props
                                             ,props:get_value(<<"Caller-Callee-ID-Name">>, Props))}
     ,{<<"Callee-ID-Number">>, props:get_value(<<"variable_effective_callee_id_number">>, Props
                                               ,props:get_value(<<"Caller-Callee-ID-Number">>, Props))}
     ,{<<"Other-Leg-Call-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Props
                                                ,props:get_value(<<"variable_holding_uuid">>, Props))}
     ,{<<"Call-Direction">>, props:get_value(<<"Call-Direction">>, Props)}
     ,{<<"To-Uri">>, props:get_value(<<"variable_sip_to_uri">>, Props)}
     ,{<<"From-Uri">>, props:get_value(<<"variable_sip_from_uri">>, Props)}
     ,{<<"Created-Time">>, get_time_value(<<"Caller-Channel-Created-Time">>, Props)}
     ,{<<"Answered-Time">>, get_time_value(<<"Caller-Channel-Answered-Time">>, Props)}
     ,{<<"Progress-Time">>, get_time_value(<<"Caller-Channel-Progress-Time">>, Props)}
     ,{<<"Progress-Media-Time">>, get_time_value(<<"Caller-Channel-Progress-Media-Time">>, Props)}
     ,{<<"Hangup-Time">>, get_time_value(<<"Caller-Channel-Hangup-Time">>, Props)}
     ,{<<"Transfer-Time">>, get_time_value(<<"Caller-Channel-Transfer-Time">>, Props)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec rating_req/2 :: (ne_binary(), wh_proplist()) -> wh_proplist().
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

-spec get_time_value/2 :: (ne_binary(), wh_proplist()) -> non_neg_integer().
get_time_value(Key, Props) ->
    V = props:get_value(Key, Props, 0),
    wh_util:unix_seconds_to_gregorian_seconds(wh_util:microseconds_to_seconds(V)).
