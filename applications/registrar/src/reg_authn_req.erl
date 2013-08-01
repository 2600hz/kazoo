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
    'ok'.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec handle_req(wh_json:json_object(), proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_authn:req_v(JObj),
    _ = wh_util:put_callid(JObj),
    Username = wapi_authn:get_auth_user(JObj),
    Realm = wapi_authn:get_auth_realm(JObj),
    lager:debug("trying to authenticate ~s@~s", [Username, Realm]),
    case lookup_auth_user(Username, Realm) of
        {'ok', #auth_user{}=AuthUser} ->
            send_auth_resp(AuthUser, JObj);
        {'error', 'not_found'} ->
            IPAddress = wh_json:get_value(<<"Orig-IP">>, JObj),
            lager:notice("auth failure for ~s@~s from ip ~s", [Username, Realm, IPAddress]),
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
-spec send_auth_error(wh_json:json_object()) -> 'ok'.
send_auth_error(JObj) ->
    Resp = [{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
            | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
           ],
    lager:debug("sending SIP authentication error"),
    wapi_authn:publish_error(wh_json:get_value(<<"Server-ID">>, JObj), Resp).

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec create_ccvs(#auth_user{}) -> wh_json:object().
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

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%% look up the user and realm in the database and return the result
%% @end
%%-----------------------------------------------------------------------------
-spec lookup_auth_user(ne_binary(), ne_binary()) ->
                              {'ok', #auth_user{}} |
                              {'error', 'not_found'}.
lookup_auth_user(Username, Realm) ->
    case get_auth_user(Username, Realm) of
        {'error', _}=E -> E;
        {'ok', JObj} ->
            check_auth_user(JObj, Username, Realm)
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec check_auth_user(wh_json:object(), ne_binary(), ne_binary()) ->
                             {'ok', #auth_user{}} |
                             {'error', _}.
check_auth_user(JObj, Username, Realm) ->
    case wh_util:is_account_enabled(wh_json:get_value([<<"doc">>, <<"pvt_account_id">>], JObj)) of
        'false' -> {'error', 'not_found'};
        'true' ->
            {'ok', jobj_to_auth_user(JObj, Username, Realm)}
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_auth_user(ne_binary(), ne_binary()) ->
                           {'ok', wh_json:object()} |
                           {'error', 'not_found'}.
get_auth_user(Username, Realm) ->
    case whapps_util:get_account_by_realm(Realm) of
        {'error', E} ->
            lager:debug("failed to lookup realm ~s in accounts: ~p", [Realm, E]),
            get_auth_user_in_agg(Username, Realm);
        {'multiples', []} ->
            lager:debug("failed to find realm ~s in accounts", [Realm]),
            get_auth_user_in_agg(Username, Realm);
        {'multiples', [AccountDB|_]} ->
            lager:debug("found multiple accounts by realm ~s, using first: ~s", [Realm, AccountDB]),
            get_auth_user_in_account(Username, Realm, AccountDB);
        {'ok', AccountDB} ->
            get_auth_user_in_account(Username, Realm, AccountDB)
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_auth_user_in_agg(ne_binary(), ne_binary()) ->
                                  {'ok', wh_json:object()} |
                                  {'error', 'not_found'}.
get_auth_user_in_agg(Username, Realm) ->
    ViewOptions = [{'key', [Realm, Username]}
                   ,'include_docs'
                  ],
    case whapps_config:get_is_true(?CONFIG_CAT, <<"use_aggregate">>, 'false')
        andalso couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup">>, ViewOptions) 
    of
        'false' ->
            lager:debug("SIP credential aggregate db is disabled"),
            {'error', 'not_found'};
        {'error', _R} ->
            lager:warning("failed to look up SIP credentials ~p in aggregate", [_R]),
            {'error', 'not_found'};
        {'ok', []} ->
            lager:debug("~s@~s not found in aggregate", [Username, Realm]),
            {'error', 'not_found'};
        {'ok', [User|_]} ->
            lager:debug("~s@~s found in aggregate", [Username, Realm]),
            {'ok', User}
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_auth_user_in_account(ne_binary(), ne_binary(), ne_binary()) ->
                                      {'ok', wh_json:object()} |
                                      {'error', 'not_found'}.
get_auth_user_in_account(Username, Realm, AccountDB) ->
    ViewOptions = [{'key', Username}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDB, <<"devices/sip_credentials">>, ViewOptions) of
        {'error', _R} ->
            lager:warning("failed to look up SIP credentials in ~s: ~p", [AccountDB, _R]),
            get_auth_user_in_agg(Username, Realm);
        {'ok', []} ->
            lager:debug("~s@~s not found in ~s", [Username, Realm, AccountDB]),
            get_auth_user_in_agg(Username, Realm);
        {'ok', [User|_]} ->
            lager:debug("~s@~s found in account db: ~s", [Username, Realm, AccountDB]),
            {'ok', User}
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec jobj_to_auth_user(wh_json:object(), ne_binary(), ne_binary()) -> #auth_user{}.
jobj_to_auth_user(JObj, Username, Realm) ->
    AuthValue = wh_json:get_value(<<"value">>, JObj),
    AuthDoc = wh_json:get_value(<<"doc">>, JObj),
    #auth_user{realm = Realm
               ,username = Username
               ,password = wh_json:get_value(<<"password">>, AuthValue)
               ,account_id = get_account_id(AuthDoc)
               ,account_db = get_account_db(AuthDoc)
               ,authorizing_type = wh_json:get_value(<<"authorizing_type">>, AuthValue, <<"anonymous">>)
               ,authorizing_id = wh_json:get_value(<<"id">>, JObj)
               ,method = get_auth_method(AuthValue)
               ,owner_id = wh_json:get_value(<<"owner_id">>, AuthDoc)
               ,suppress_unregister_notifications = wh_json:is_true(<<"suppress_unregister_notifications">>, AuthDoc)
              }.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_account_id/1  :: (wh_json:object()) -> api_binary().
get_account_id(JObj) ->
    case wh_json:get_value(<<"pvt_account_id">>, JObj) of
        'undefined' ->
            case wh_json:get_value(<<"pvt_account_db">>, JObj) of
                'undefined' -> 'undefined';
                AccountDb -> wh_util:format_account_id(AccountDb, raw)
            end;
        AccountId -> AccountId
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_account_db/1  :: (wh_json:object()) -> api_binary().
get_account_db(JObj) ->
    case wh_json:get_value(<<"pvt_account_db">>, JObj) of
        'undefined' ->
            case wh_json:get_value(<<"pvt_account_id">>, JObj) of
                'undefined' -> 'undefined';
                AccountId -> wh_util:format_account_id(AccountId, 'encoded')
            end;
        AccountDb -> AccountDb
    end.

%%-----------------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%-----------------------------------------------------------------------------
-spec get_auth_method/1  :: (wh_json:object()) -> ne_binary().
get_auth_method(JObj) ->
    Method = wh_json:get_binary_value(<<"method">>, JObj, <<"password">>),
    wh_util:to_lower_binary(Method).
