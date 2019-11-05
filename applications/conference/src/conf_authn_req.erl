%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Handle authn_req messages
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(conf_authn_req).

-export([handle_req/2]).

-include("conference.hrl").

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_authn:req_v(JObj),
    _ = kz_log:put_callid(JObj),
    Conference = kapps_conference:new(),
    Username = kapi_authn:get_auth_user(JObj),
    case kapps_conference:bridge_username(Conference) of
        Username ->
            Password = kapps_conference:bridge_password(Conference),
            send_authn_resp(Password, JObj);
        _Else -> 'ok'
    end.

-spec send_authn_resp(kz_term:ne_binary(), kz_json:object()) -> 'ok'.
send_authn_resp(Password, JObj) ->
    Resp = props:filter_undefined(
             [{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
             ,{<<"Auth-Password">>, Password}
             ,{<<"Auth-Method">>, <<"password">>}
              | kz_api:default_headers(<<"directory">>, <<"authn_resp">>, ?APP_NAME, ?APP_VERSION)
             ]),
    lager:debug("sending SIP authentication reply, with credentials"),
    kapi_authn:publish_resp(kz_json:get_value(<<"Server-ID">>, JObj), Resp).
