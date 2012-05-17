%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% Make a request for authorization, and answer queries about the CallID
%%% @end
%%% Created :  7 Jul 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_authz).

-export([maybe_authorize_channel/2]).
-export([update/2]).

-define(RATE_VARS, [<<"Rate">>, <<"Rate-Increment">>
                        ,<<"Rate-Minimum">>, <<"Surcharge">>
                        ,<<"Rate-Name">>, <<"Base-Cost">>
                   ]).

-include_lib("ecallmgr/src/ecallmgr.hrl").

-spec maybe_authorize_channel/2 :: (proplist(), atom()) -> boolean().
maybe_authorize_channel(Props, Node) ->
    RequiresAuthz = case props:get_value(<<"Call-Direction">>, Props) of
                        <<"inbound">> -> props:get_value(<<"variable_", ?CHANNEL_VAR_PREFIX, "Authorizing-ID">>, Props) =:= undefined;
                        <<"outbound">> -> props:get_value(<<"variable_", ?CHANNEL_VAR_PREFIX, "Resource-ID">>, Props) =/= undefined
                    end,
    case RequiresAuthz andalso wh_util:is_true(ecallmgr_config:get(<<"authz_enabled">>, false)) of
        false when RequiresAuthz ->
            lager:debug("config ecallmgr.authz_enabled is 'false', allowing call", []),
            true;
        false ->
            lager:debug("channel does not require authorization, allowing call", []),
            true;
        true -> 
            CallId = props:get_value(<<"Unique-ID">>, Props),
            ecallmgr_util:send_cmd(Node, CallId, "set", <<"api_on_answer=uuid_session_heartbeat ", CallId/binary, " 60">>),
            case authorize(Props, Node) of
                true -> true;
                false -> 
                    kill_uuid(Props, Node),
                    false
            end
    end.

-spec update/2 :: (proplist(), atom()) -> 'ok'.
update(Props, Node) ->
    Update = authz_update(Props, Node),
    wapi_authz:publish_update([KV || {_, V}=KV <- Update, V =/= undefined]).

-spec authorize/2 :: (proplist(), atom()) -> 'ok'.
authorize(Props, Node) ->
    lager:debug("channel authorization request started"),
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,authz_req(Props)
                                  ,fun wapi_authz:publish_req/1
                                  ,fun wapi_authz:resp_v/1),
    case ReqResp of 
        {error, _R} -> 
            lager:debug("authz request lookup failed: ~p", [_R]),
            default();                 
        {ok, RespJObj} ->
            handle_authz_response(Props, RespJObj, Node)
    end.

-spec request_rating/1 :: (proplist()) -> 'ok'.
request_rating(Props) ->
    lager:debug("sending rate request"),
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,rating_req(Props)
                                  ,fun wapi_rate:publish_req/1
                                  ,fun wapi_rate:resp_v/1),
    case ReqResp of 
        {error, _R} -> 
            lager:debug("rate request lookup failed: ~p", [_R]);
        {ok, RespJObj} ->
            set_rating_ccvs(RespJObj)
    end.    

-spec handle_authz_response/3 :: (proplist(), wh_json:json_object(), atom()) -> boolean().
handle_authz_response(Props, JObj, Node) ->
    case wh_util:is_true(wh_json:get_value(<<"Is-Authorized">>, JObj)) of
        true -> 
            lager:debug("channel authorization received affirmative response, allowing call", []),
            wapi_authz:publish_win(wh_json:get_value(<<"Server-ID">>, JObj)
                                   ,wh_json:delete_key(<<"Event-Name">>, JObj)),
            CallId = wh_json:get_value(<<"Call-ID">>, JObj),    
            case wh_json:get_value(<<"Type">>, JObj) of
                <<"per_minute">> ->
                    lager:debug("call authorized as per_minute, setting flag", []),
                    spawn(fun() -> request_rating(Props) end),
                    ecallmgr_util:send_cmd(Node, CallId, "set", <<?CHANNEL_VAR_PREFIX, "Per-Minute=true">>);
                _Else -> 
                    lager:debug("call authorized as ~s", [_Else]),
                    %% this may fail, depends how fast they answered. Doesnt really matter tho...
                    ecallmgr_util:send_cmd(Node, CallId, "set", <<"api_on_answer=">>),
                    ok
            end,
            true;
        false ->
            lager:debug("channel authorization received negative response", []),
            false
    end.

-spec default/0 :: () -> boolean().
default() ->
    Default = ecallmgr_config:get(<<"authz_default">>, <<"deny">>),
    lager:debug("channel authorization did not received response, config ecallmgr.authz_default is '~s'", [Default]),
    Default =/= <<"deny">>.

-spec kill_uuid/2 :: (proplist(), atom()) -> 'ok'.
-spec kill_uuid/3 :: (ne_binary(), ne_binary(), atom()) -> 'ok'.

kill_uuid(Props, Node) ->
    Direction = props:get_value(<<"Call-Direction">>, Props),
    CallId = props:get_value(<<"Unique-ID">>, Props),    
    kill_uuid(Direction, CallId, Node).

kill_uuid(<<"inbound">>, CallId, Node) ->
    _ = ecallmgr_util:fs_log(Node, "whistle terminating unathorized inbound call", []),
    freeswitch:api(Node, uuid_kill, wh_util:to_list(<<CallId/binary, " INCOMING_CALL_BARRED">>)),
    ok;
kill_uuid(<<"outbound">>, CallId, Node) ->
    _ = ecallmgr_util:fs_log(Node, "whistle terminating unathorized outbound call", []),
    freeswitch:api(Node, uuid_kill, wh_util:to_list(<<CallId/binary, " OUTGOING_CALL_BARRED">>)),
    ok.

-spec set_rating_ccvs/1 :: (wh_json:json_object()) -> 'ok'.
set_rating_ccvs(JObj) ->
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put(callid, CallId),
    case ecallmgr_fs_nodes:channel_node(CallId) of
        {error, _} -> ok;
        {ok, Node} ->
            lager:debug("setting rating information", []),
            [ecallmgr_util:send_cmd(Node, CallId, "set", <<?CHANNEL_VAR_PREFIX, Key/binary, "=", Value/binary>>)
             || Key <- ?RATE_VARS
                    ,(Value = wh_json:get_binary_value(Key, JObj)) =/= undefined
            ],
            ok
    end.
            
-spec authz_req/1 :: (proplist()) -> proplist().
authz_req(Props) ->
    AccountId = props:get_value(<<"variable_", ?CHANNEL_VAR_PREFIX, "Account-ID">>, Props),
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

-spec authz_update/2 :: (proplist(), atom()) -> proplist().
authz_update(Props, Node) ->
    [{<<"Call-ID">>, props:get_value(<<"Caller-Unique-ID">>, Props)}
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
     ,{<<"Other-Leg-Call-ID">>, props:get_value(<<"Other-Leg-Unique-ID">>, Props,                                                                                                                                                                                      
                                                props:get_value(<<"variable_holding_uuid">>, Props))}
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

-spec rating_req/1 :: (proplist()) -> proplist().
rating_req(Props) ->
    AccountId = props:get_value(<<"variable_", ?CHANNEL_VAR_PREFIX, "Account-ID">>, Props),
    [{<<"To-DID">>, props:get_value(<<"Caller-Destination-Number">>, Props)}
     ,{<<"From-DID">>, props:get_value(<<"variable_effective_caller_id_number">>, Props
                                       ,props:get_value(<<"Caller-Caller-ID-Number">>, Props))}
     ,{<<"Call-ID">>, props:get_value(<<"Caller-Unique-ID">>, Props)}
     ,{<<"Account-ID">>, AccountId}
     ,{<<"Direction">>, props:get_value(<<"Call-Direction">>, Props)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].

-spec get_time_value/2 :: (ne_binary(), proplist()) -> integer() | 'undefined'.
get_time_value(Key, Props) ->
    V = props:get_value(Key, Props, 0),
    wh_util:unix_seconds_to_gregorian_seconds(wh_util:microseconds_to_seconds(V)).
