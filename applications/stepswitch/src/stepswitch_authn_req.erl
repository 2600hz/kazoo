%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Handle authn_req messages
%%% @end
%%% @contributors
%%%
%%%-------------------------------------------------------------------
-module(stepswitch_authn_req).

-export([handle_req/2]).

-include("stepswitch.hrl").

%%-----------------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_authn:req_v(JObj),
    _ = wh_util:put_callid(JObj),
    case wh_json:get_value(<<"Method">>, JObj) of
        <<"reverse-lookup">> -> maybe_send_auth_resp(JObj);
        _Else -> 'ok'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% 
%% @end
%%--------------------------------------------------------------------
-spec maybe_send_auth_resp(wh_json:object()) -> 'ok'.
maybe_send_auth_resp(JObj) ->
    case stepswitch_resources:reverse_lookup(JObj) of
        {'error', 'not_found'} -> 'ok';
        {'ok', Props} -> send_auth_resp(JObj, Props)
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% extract the auth realm from the API request, using the requests to domain
%% when provided with an IP
%% @end
%%-----------------------------------------------------------------------------
-spec send_auth_resp/2  :: (wh_json:object(), wh_proplist()) -> 'ok'.
send_auth_resp(JObj, Props) ->
    Category = wh_json:get_value(<<"Event-Category">>, JObj),
    Username = props:get_value('username', Props),

    CCVs = props:filter_undefined(
             [{<<"Inception">>, <<"off-net">>}
%%              ,{<<"Authorizing-Type">>, <<"resource">>}
              ,{<<"Username">>, Username}
              ,{<<"Authorizing-ID">>, props:get_value('resource_id', Props)}
              ,{<<"Realm">>, props:get_value('realm', Props)}
             ]),
    
    Resp = props:filter_undefined(
             [{<<"Auth-Method">>, <<"password">>}
              ,{<<"Auth-Username">>, Username}
              ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
              ,{<<"Auth-Password">>, props:get_value('password', Props)}
              ,{<<"Custom-Channel-Vars">>, wh_json:from_list(CCVs)}
              | wh_api:default_headers(Category, <<"authn_resp">>, ?APP_NAME, ?APP_VERSION)
             ]),
    lager:debug("sending SIP authentication reply, with credentials"),
    wapi_authn:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).
