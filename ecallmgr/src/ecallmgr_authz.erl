%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% Make a request for authorization, and answer queries about the CallID
%%% @end
%%% Created :  7 Jul 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_authz).

-export([enable_authz/0]).
-export([disable_authz/0]).
-export([maybe_authorize_channel/2]).

-include_lib("ecallmgr/src/ecallmgr.hrl").

enable_authz() ->
    ecallmgr_config:set(<<"authz_enabled">>, true).

disable_authz() ->
    ecallmgr_config:set(<<"authz_enabled">>, false).

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
            case authorize(Props, Node) of
                true -> true;
                false -> 
                    kill_uuid(Props, Node),
                    false
            end
    end.

-spec authorize/2 :: (proplist(), atom()) -> 'ok'.
authorize(Props, Node) ->
    lager:debug("channel authorization request started"),
    ReqResp = wh_amqp_worker:call(?ECALLMGR_AMQP_POOL
                                  ,request(Props)
                                  ,fun wapi_authz:publish_req/1
                                  ,fun wapi_authz:resp_v/1),
    case ReqResp of 
        {error, _R} -> 
            lager:debug("authz request lookup failed: ~p", [_R]),
            default();                 
        {ok, RespJObj} ->
            handle_authz_response(RespJObj, Node)
    end.

-spec handle_authz_response/2 :: (wh_json:json_object(), atom()) -> boolean().
handle_authz_response(JObj, Node) ->
    case wh_util:is_true(wh_json:get_value(<<"Is-Authorized">>, JObj)) of
        true -> 
            lager:debug("channel authorization received affirmative response, allowing call", []),
            wapi_authz:publish_win(wh_json:get_value(<<"Server-ID">>, JObj)
                                   ,wh_json:delete_key(<<"Event-Name">>, JObj)),
            case wh_json:get_value(<<"Type">>, JObj) of
                <<"per_minute">> ->
                    CallId = wh_json:get_value(<<"Call-ID">>, JObj),    
                    lager:debug("call authorized as per-minute, ensuring call event process for ~s exists", [CallId]),
                    {ok, _} = ecallmgr_call_sup:start_event_process(Node, CallId);
                _Else -> ok
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

-spec request/1 :: (proplist()) -> proplist().
request(Props) ->
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
     | wh_api:default_headers(<<>>, <<"dialplan">>, <<"authz_req">>, ?APP_NAME, ?APP_VERSION)
    ].
