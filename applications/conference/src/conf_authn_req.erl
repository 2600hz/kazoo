%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%% Handle authn_req messages
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(conf_authn_req).

-export([handle_req/2]).

-include("conference.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_authn:req_v(JObj),
    _ = wh_util:put_callid(JObj),
    case wh_json:get_value(<<"Method">>, JObj) of
        <<"INVITE">> -> maybe_send_authn_resp(JObj);
        _Else -> 'ok'
    end.

-spec maybe_send_authn_resp(wh_json:object()) -> 'ok'.
maybe_send_authn_resp(JObj) ->
    Conference = whapps_conference:new(),
    Username = wapi_authn:get_auth_user(JObj),
    case whapps_conference:bridge_username(Conference) of
        Username ->
            Password = whapps_conference:bridge_password(Conference),
            send_authn_resp(Password, JObj);
        _Else -> 'ok'
    end.

-spec send_authn_resp(ne_binary(), wh_json:object()) -> 'ok'.
send_authn_resp(Password, JObj) -> 
    Resp = props:filter_undefined(
             [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
              ,{<<"Auth-Password">>, Password}
              ,{<<"Auth-Method">>, <<"password">>}
              | wh_api:default_headers(<<"directory">>, <<"authn_resp">>, ?APP_NAME, ?APP_VERSION)
             ]),
    lager:debug("sending SIP authentication reply, with credentials"),
    wapi_authn:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).
