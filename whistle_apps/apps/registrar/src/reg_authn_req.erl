%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Handle authn_req messages
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(reg_authn_req).

-export([init/0, handle_req/2]).

-include("reg.hrl").

init() ->
    ok.

-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    true = wapi_authn:req_v(JObj),

    put(callid, wh_json:get_value(<<"Msg-ID">>, JObj, <<"000000000000">>)),

    Username = wapi_authn:get_auth_user(JObj),
    Realm = wapi_authn:get_auth_realm(JObj),

    lager:debug("trying to authenticate ~s@~s", [Username, Realm]),

    case reg_util:lookup_auth_user(Username, Realm) of
        {ok, #auth_user{}=AuthUser} ->
            send_auth_resp(AuthUser, JObj);
        {error, not_found} ->            
            IPAddress = wh_json:get_value(<<"Orig-IP">>, JObj),
            lager:info("auth failure for ~s@~s from ip ~s", [Username, Realm, IPAddress]),
            send_auth_error(JObj)
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% extract the auth realm from the API request, using the requests to domain
%% when provided with an IP
%% @end
%%-----------------------------------------------------------------------------
-spec send_auth_resp/2  :: (#auth_user{}, wh_json:json_object()) -> 'ok'.
send_auth_resp(#auth_user{password=Password, method=Method}=AuthUser, JObj) ->
    Category = wh_json:get_value(<<"Event-Category">>, JObj),
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            ,{<<"Auth-Password">>, Password}
            ,{<<"Auth-Method">>, Method}
            ,{<<"Custom-Channel-Vars">>, create_ccvs(AuthUser)}
            | wh_api:default_headers(Category, <<"authn_resp">>, ?APP_NAME, ?APP_VERSION)
           ],
    
    lager:debug("sending SIP authentication reply, with credentials"),
    wapi_authn:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% extract the auth realm from the API request, using the requests to domain
%% when provided with an IP
%% @end
%%-----------------------------------------------------------------------------
-spec send_auth_error/1 :: (wh_json:json_object()) -> 'ok'.
send_auth_error(JObj) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    lager:debug("sending SIP authentication error"),
    wapi_authn:publish_error(wh_json:get_value(<<"Server-ID">>, JObj), Resp).

-spec create_ccvs/1 :: (#auth_user{}) -> wh_json:object().
create_ccvs(#auth_user{}=AuthUser) ->    
    Props = [{<<"Username">>, AuthUser#auth_user.username}
             ,{<<"Realm">>, AuthUser#auth_user.realm}
             ,{<<"Account-ID">>, AuthUser#auth_user.account_id}
             ,{<<"Authorizing-ID">>, AuthUser#auth_user.authorizing_id}
             ,{<<"Authorizing-Type">>, AuthUser#auth_user.authorizing_type}
             ,{<<"Owner-ID">>, AuthUser#auth_user.owner_id}
             ,{<<"Inception">>, <<"on-net">>}
            ],
    wh_json:from_list(props:filter_undefined(Props)).
