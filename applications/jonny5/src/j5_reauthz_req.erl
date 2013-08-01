%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(j5_reauthz_req).

-export([handle_req/2]).
-export([send_allow_resp/1
         ,send_allow_resp/2
        ]).
-export([send_deny_resp/1
         ,send_deny_resp/2
        ]).

-include("jonny5.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_authz:reauthz_req_v(JObj),
    wh_util:put_callid(JObj),

    timer:sleep(crypto:rand_uniform(0, 1000)),

    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    lager:debug("attempting to reauthorize call via account ~s", [AccountId]),

    Limits = j5_util:get_limits(AccountId),

    maybe_skip_reauth(wh_json:get_value(<<"Call-Direction">>, JObj), Limits, JObj).

-spec maybe_skip_reauth(ne_binary(), #limits{}, wh_json:object()) -> 'ok'.
maybe_skip_reauth(<<"outbound">>, #limits{soft_limit_outbound='true'}, JObj) ->
    lager:debug("outbound calls are not enforcing (soft limit)", []),
    send_allow_resp(JObj);
maybe_skip_reauth(<<"inbound">>, #limits{soft_limit_inbound='true'}, JObj) ->
    lager:debug("inbound calls are not enforcing (soft limit)", []),
    send_allow_resp(JObj);
maybe_skip_reauth(_, Limits, JObj) ->
    case wh_json:get_value(<<"Type">>, JObj) of
        <<"allotment">> -> j5_allotments:reauthorize(Limits, JObj);
        <<"per_minute">> -> j5_credit:reauthorize(Limits, JObj);
        _Else -> send_allow_resp(JObj)
    end.

-spec send_allow_resp(wh_json:object()) -> 'ok'.
-spec send_allow_resp(wh_json:object(), wh_json:object()) -> 'ok'.

send_allow_resp(JObj) ->
    send_allow_resp(JObj, undefined).

send_allow_resp(JObj, CCVs) ->
    lager:debug("reauthorization succeeded", []),
    send_resp(JObj, CCVs, <<"true">>).

-spec send_deny_resp(wh_json:object()) -> 'ok'.
-spec send_deny_resp(wh_json:object(), wh_json:object()) -> 'ok'.

send_deny_resp(JObj) ->
    send_deny_resp(JObj, undefined).

send_deny_resp(JObj, CCVs) ->
    lager:debug("reauthorization failed", []),
    send_resp(JObj, CCVs, <<"false">>).

-spec send_resp(wh_json:object(), wh_json:object(), ne_binary()) -> 'ok'.
send_resp(JObj, CCVs, Authd) ->
    Resp = [{<<"Is-Authorized">>, Authd}
            ,{<<"Type">>, wh_json:get_value(<<"Type">>, JObj)}
            ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
            ,{<<"Custom-Channel-Vars">>, CCVs}
            | wh_api:default_headers(<<>>, ?APP_NAME, ?APP_VERSION)
           ],
    wapi_authz:publish_reauthz_resp(wh_json:get_value(<<"Server-ID">>, JObj)
                                    ,props:filter_undefined(Resp)).
