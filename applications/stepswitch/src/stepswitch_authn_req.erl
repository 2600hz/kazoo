%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
%%% @doc Handle authn_req messages
%%% @end
%%%-----------------------------------------------------------------------------
-module(stepswitch_authn_req).

-export([handle_req/2]).

-include("stepswitch.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_authn:req_v(JObj),
    _ = kz_util:put_callid(JObj),
    case kz_json:get_value(<<"Method">>, JObj) of
        <<"reverse-lookup">> -> maybe_send_auth_resp(JObj);
        _Else -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_send_auth_resp(kz_json:object()) -> 'ok'.
maybe_send_auth_resp(JObj) ->
    case stepswitch_resources:reverse_lookup(JObj) of
        {'error', 'not_found'} -> 'ok';
        {'ok', Props} -> send_auth_resp(JObj, Props)
    end.

%%------------------------------------------------------------------------------
%% @doc extract the auth realm from the API request, using the requests to domain
%% when provided with an IP
%% @end
%%------------------------------------------------------------------------------
-spec send_auth_resp(kz_json:object(), kz_term:proplist()) -> 'ok'.
send_auth_resp(JObj, Props) ->
    Category = kz_json:get_value(<<"Event-Category">>, JObj),
    Username = props:get_value('username', Props),
    CCVs = props:filter_undefined(
             [{<<"Username">>, Username}
             ,{<<"Authorizing-ID">>, props:get_value('resource_id', Props)}
             ,{<<"Realm">>, props:get_value('realm', Props)}
             ]),
    Resp = props:filter_undefined(
             [{<<"Auth-Method">>, <<"password">>}
             ,{<<"Auth-Username">>, Username}
             ,{<<"Msg-ID">>, kz_json:get_value(<<"Msg-ID">>, JObj)}
             ,{<<"Auth-Password">>, props:get_value('password', Props)}
             ,{<<"Custom-Channel-Vars">>, kz_json:from_list(CCVs)}
              | kz_api:default_headers(Category, <<"authn_resp">>, ?APP_NAME, ?APP_VERSION)
             ]),
    lager:debug("sending SIP authentication reply, with credentials"),
    kapi_authn:publish_resp(kz_json:get_value(<<"Server-ID">>, JObj), Resp).
