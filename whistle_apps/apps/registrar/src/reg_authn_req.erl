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
handle_req(ApiJObj, _Props) ->
    true = wapi_authn:req_v(ApiJObj),

    put(callid, wh_json:get_value(<<"Msg-ID">>, ApiJObj, <<"000000000000">>)),

    AuthU = wapi_authn:get_auth_user(ApiJObj),
    AuthR = wapi_authn:get_auth_realm(ApiJObj),

    lager:debug("trying to authenticate ~s@~s", [AuthU, AuthR]),

    case reg_util:lookup_auth_user(AuthU, AuthR) of
        {ok, AuthJObj} ->
            send_auth_resp(AuthJObj, AuthU, AuthR, ApiJObj);
        {error, not_found} ->            
            lager:debug("user ~s@~s is unknown", [AuthU, AuthR]),
            send_auth_error(ApiJObj)
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% extract the auth realm from the API request, using the requests to domain
%% when provided with an IP
%% @end
%%-----------------------------------------------------------------------------
-spec send_auth_resp/4  :: (wh_json:json_object(), ne_binary(), ne_binary(), wh_json:json_object()) -> 'ok'.
send_auth_resp(AuthJObj, AuthU, AuthR, ApiJObj) ->
    AuthValue = wh_json:get_value(<<"value">>, AuthJObj),
    AuthDoc = wh_json:get_value(<<"doc">>, AuthJObj),
    Category = wh_json:get_value(<<"Event-Category">>, ApiJObj),

    CCVs = [{<<"Username">>, AuthU}
            ,{<<"Realm">>, AuthR}
            ,{<<"Account-ID">>, get_account_id(AuthDoc)}
            ,{<<"Authorizing-Type">>, wh_json:get_value(<<"authorizing_type">>, AuthValue, <<"anonymous">>)}
            ,{<<"Inception">>, <<"on-net">>}
            ,{<<"Authorizing-ID">>, wh_json:get_value(<<"_id">>, AuthDoc)}
            ,{<<"Owner-ID">>, wh_json:get_value(<<"owner_id">>, AuthDoc)}            
           ],

    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, ApiJObj)}
            ,{<<"Auth-Password">>, wh_json:get_value(<<"password">>, AuthValue)}
            ,{<<"Auth-Method">>, get_auth_method(AuthValue)}
            ,{<<"Custom-Channel-Vars">>, wh_json:from_list(props:filter_undefined(CCVs))}
            | wh_api:default_headers(Category, <<"authn_resp">>, ?APP_NAME, ?APP_VERSION)
           ],
    
    lager:debug("sending SIP authentication reply, with credentials"),
    wapi_authn:publish_resp(wh_json:get_value(<<"Server-ID">>, ApiJObj), Resp).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% extract the auth realm from the API request, using the requests to domain
%% when provided with an IP
%% @end
%%-----------------------------------------------------------------------------
-spec send_auth_error/1 :: (wh_json:json_object()) -> 'ok'.
send_auth_error(ApiJObj) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, ApiJObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    lager:debug("sending SIP authentication error"),
    wapi_authn:publish_error(wh_json:get_value(<<"Server-ID">>, ApiJObj), Resp).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% extract the account id from the auth document, using the newer pvt field
%% when present but failing back to reformating the older db name pvt.
%% @end
%%-----------------------------------------------------------------------------
-spec get_account_id/1  :: (wh_json:json_object()) -> ne_binary() | 'undefined'.
get_account_id(AuthDoc) ->
    case wh_json:get_value(<<"pvt_account_id">>, AuthDoc) of
        undefined ->
            case wh_json:get_value(<<"pvt_account_db">>, AuthDoc) of
                undefined -> undefined;
                AcctDb -> wh_util:format_account_id(AcctDb, raw)
            end;
        AcctId -> AcctId
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% extract a normalized method from the view results
%% @end
%%-----------------------------------------------------------------------------
-spec get_auth_method/1  :: (wh_json:json_object()) -> ne_binary().
get_auth_method(AuthValue) ->
    Method = wh_json:get_binary_value(<<"method">>, AuthValue, <<"password">>),
    wh_util:to_lower_binary(Method).
