%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%% Account module
%%%
%%% Handle client requests for account documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cb_accounts).

-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,put/1, put/2
         ,post/2
         ,delete/2
        ]).

-export([is_unique_realm/2]).

-include("../crossbar.hrl").

-define(SERVER, ?MODULE).

-define(ACCOUNTS_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".accounts">>).

-define(AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(AGG_VIEW_SUMMARY, <<"accounts/listing_by_id">>).
-define(AGG_VIEW_PARENT, <<"accounts/listing_by_parent">>).
-define(AGG_VIEW_CHILDREN, <<"accounts/listing_by_children">>).
-define(AGG_VIEW_DESCENDANTS, <<"accounts/listing_by_descendants">>).
-define(AGG_VIEW_REALM, <<"accounts/listing_by_realm">>).

-define(PVT_TYPE, <<"account">>).
-define(CHANNELS, <<"channels">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.accounts">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.accounts">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.accounts">>, ?MODULE, 'validate'),
    _ = crossbar_bindings:bind(<<"*.execute.put.accounts">>, ?MODULE, 'put'),
    _ = crossbar_bindings:bind(<<"*.execute.post.accounts">>, ?MODULE, 'post'),
    _ = crossbar_bindings:bind(<<"*.execute.delete.accounts">>, ?MODULE, 'delete').

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
-spec allowed_methods(path_token()) -> http_methods().
-spec allowed_methods(path_token(), ne_binary()) -> http_methods().
allowed_methods() ->
    [?HTTP_PUT].
allowed_methods(_) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_DELETE].
allowed_methods(_, Path) ->
    case lists:member(Path, [<<"ancestors">>, <<"children">>, <<"descendants">>, <<"siblings">>, ?CHANNELS]) of
        'true' -> [?HTTP_GET];
        'false' -> []
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists() -> 'true'.
-spec resource_exists(path_token()) -> 'true'.
-spec resource_exists(path_token(), ne_binary()) -> boolean().
resource_exists() -> 'true'.
resource_exists(_) -> 'true'.
resource_exists(_, Path) ->
    lists:member(Path, [<<"ancestors">>, <<"children">>, <<"descendants">>, <<"siblings">>, ?CHANNELS]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
-spec validate(cb_context:context(), path_token()) -> cb_context:context().
-spec validate(cb_context:context(), path_token(), ne_binary()) -> cb_context:context().

validate(Context) ->
    validate_accounts(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

validate_accounts(Context, ?HTTP_PUT, [{?WH_ACCOUNTS_DB, _}]) ->
    validate_request('undefined', prepare_context('undefined', Context)).

validate(Context, AccountId) ->
    validate_account(Context, AccountId, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

validate_account(Context, AccountId, ?HTTP_GET, [{?WH_ACCOUNTS_DB, _}]) ->
    load_account(AccountId, prepare_context(AccountId, Context));
validate_account(Context, _AccountId, ?HTTP_PUT, [{?WH_ACCOUNTS_DB, _}]) ->
    validate_request('undefined', prepare_context('undefined', Context));
validate_account(Context, AccountId, ?HTTP_POST, [{?WH_ACCOUNTS_DB, _}]) ->
    validate_request(AccountId, prepare_context(AccountId, Context));
validate_account(Context, AccountId, ?HTTP_DELETE, [{?WH_ACCOUNTS_DB, _}]) ->
    validate_delete_request(AccountId, prepare_context(AccountId, Context));
validate_account(Context, AccountId, _Verb, _Nouns) ->
    load_account_db(AccountId, Context).

validate(Context, AccountId, PathToken) ->
    validate_account_path(Context, AccountId, PathToken, cb_context:req_verb(Context)).

validate_account_path(Context, AccountId, ?CHANNELS, ?HTTP_GET) ->
    get_channels(AccountId, Context);
validate_account_path(Context, AccountId, <<"children">>, ?HTTP_GET) ->
    load_children(AccountId, prepare_context('undefined', Context));
validate_account_path(Context, AccountId, <<"descendants">>, ?HTTP_GET) ->
    load_descendants(AccountId, prepare_context('undefined', Context));
validate_account_path(Context, AccountId, <<"siblings">>, ?HTTP_GET) ->
    load_siblings(AccountId, prepare_context('undefined', Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, AccountId) ->
    Context1 = crossbar_doc:save(Context),

    case cb_context:resp_status(Context1) of
        'success' ->
            _ = provisioner_util:maybe_update_account(Context1),
            JObj = cb_context:doc(Context1),
            _ = replicate_account_definition(JObj),
            support_depreciated_billing_id(wh_json:get_value(<<"billing_id">>, JObj)
                                           ,AccountId
                                           ,leak_pvt_fields(Context1)
                                          );
        _Status -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), path_token()) -> cb_context:context().

put(Context) ->
    JObj = cb_context:doc(Context),
    AccountId = wh_json:get_value(<<"_id">>, JObj, couch_mgr:get_uuid()),
    try create_new_account_db(prepare_context(AccountId, Context)) of
        C -> leak_pvt_fields(C)
    catch
        'throw':C ->
            case cb_context:is_context(C) of
                'true' ->  delete(C, AccountId);
                'false' ->
                    C = cb_context:add_system_error('unspecified_fault', Context),
                    delete(C, AccountId)
            end;
        _:_ ->
            C = cb_context:add_system_error('unspecified_fault', Context),
            delete(C, AccountId)
    end.

put(Context, _AccountId) ->
    ?MODULE:put(Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    AccountId = wh_util:format_account_id(Account, 'raw'),
    case whapps_util:is_account_db(AccountDb) of
        'false' ->
            cb_context:add_system_error('bad_identifier', [{'details', AccountId}],  Context);
        'true' ->
            delete_remove_services(prepare_context(Context, AccountId, AccountDb))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec get_channels(ne_binary(), cb_context:context()) -> cb_context:context().
get_channels(AccountId, Context) ->
    Req = [{<<"Account-ID">>, AccountId}
           | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    case whapps_util:amqp_pool_collect(Req
                                       ,fun wapi_call:publish_query_account_channels_req/1
                                       ,{'ecallmgr', 'true'})
    of
        {'error', _R} ->
            lager:error("could not reach ecallmgr channels: ~p", [_R]),
            crossbar_util:response('error', <<"could not reach ecallmgr channels">>, Context);
        {_, Resp} ->
            Channels = merge_user_channels_jobjs(Resp),
            crossbar_util:response(Channels, Context)
    end.

-spec merge_user_channels_jobjs(wh_json:objects()) -> wh_json:objects().
merge_user_channels_jobjs(JObjs) ->
    merge_user_channels_jobjs(JObjs, dict:new()).

-spec merge_user_channels_jobjs(wh_json:objects(), dict()) -> wh_json:objects().
merge_user_channels_jobjs([], Dict) ->
    [Channel || {_, Channel} <- dict:to_list(Dict)];
merge_user_channels_jobjs([JObj|JObjs], Dict) ->
    merge_user_channels_jobjs(JObjs, merge_user_channels_jobj(JObj, Dict)).

-spec merge_user_channels_jobj(wh_json:object(), dict()) -> dict().
merge_user_channels_jobj(JObj, Dict) ->
    lists:foldl(fun(Channel, D) ->
                        UUID = wh_json:get_value(<<"uuid">>, Channel),
                        dict:store(UUID, Channel, D)
                end, Dict, wh_json:get_value(<<"Channels">>, JObj, [])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec prepare_context(api_binary(), cb_context:context()) -> cb_context:context().
-spec prepare_context(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
prepare_context('undefined', Context) ->
    cb_context:set_account_db(Context, ?WH_ACCOUNTS_DB);
prepare_context(Account, Context) ->
    AccountId = wh_util:format_account_id(Account, 'raw'),
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    prepare_context(Context, AccountId, AccountDb).

prepare_context(Context, AccountId, AccountDb) ->
    cb_context:setters(Context, [{fun cb_context:set_account_db/2, AccountDb}
                                 ,{fun cb_context:set_account_id/2, AccountId}
                                ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request(api_binary(), cb_context:context()) -> cb_context:context().
validate_request(AccountId, Context) ->
    ensure_account_has_realm(AccountId, Context).

-spec ensure_account_has_realm(api_binary(), cb_context:context()) -> cb_context:context().
ensure_account_has_realm(AccountId, Context) ->
    JObj = cb_context:req_data(Context),
    case wh_json:get_ne_value(<<"realm">>, JObj) of
        'undefined' ->
            RealmSuffix = whapps_config:get_binary(?ACCOUNTS_CONFIG_CAT, <<"account_realm_suffix">>, <<"sip.2600hz.com">>),
            Strength = whapps_config:get_integer(?ACCOUNTS_CONFIG_CAT, <<"random_realm_strength">>, 3),
            J = wh_json:set_value(<<"realm">>, list_to_binary([wh_util:rand_hex_binary(Strength), ".", RealmSuffix]), JObj),
            cleanup_leaky_keys(AccountId, cb_context:set_req_data(Context, J));
        _Else -> cleanup_leaky_keys(AccountId, Context)
    end.

-spec cleanup_leaky_keys(api_binary(), cb_context:context()) -> cb_context:context().
cleanup_leaky_keys(AccountId, Context) ->
    RemoveKeys = [<<"wnm_allow_additions">>
                  ,<<"superduper_admin">>
                  ,<<"billing_mode">>
                 ],
    validate_realm_is_unique(AccountId
                             ,cb_context:set_req_data(Context
                                                      ,wh_json:delete_keys(RemoveKeys, cb_context:req_data(Context)
                                                                          )
                                                     )
                            ).

-spec validate_realm_is_unique(api_binary(), cb_context:context()) -> cb_context:context().
validate_realm_is_unique(AccountId, Context) ->
    Realm = wh_json:get_ne_value(<<"realm">>, cb_context:req_data(Context)),
    case is_unique_realm(AccountId, Realm) of
        'true' -> validate_account_schema(AccountId, Context);
        'false' ->
            C = cb_context:add_validation_error([<<"realm">>]
                                                ,<<"unique">>
                                                ,<<"Account realm already in use">>
                                                ,Context),
            validate_account_schema(AccountId, C)
    end.

-spec validate_account_schema(api_binary(), cb_context:context()) -> cb_context:context().
validate_account_schema(AccountId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(AccountId, C) end,
    cb_context:validate_request_data(<<"accounts">>, Context, OnSuccess).

-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    set_private_properties(Context);
on_successful_validation(AccountId, Context) ->
    Context1 = crossbar_doc:load_merge(AccountId, Context),
    maybe_import_enabled(Context1).

maybe_import_enabled(Context) ->
    case cb_context:auth_account_id(Context) =:= cb_context:account_id(Context) of
        'true' ->
            cb_context:set_doc(Context
                               ,wh_json:delete_key(<<"enabled">>, cb_context:doc(Context))
                              );
        'false' ->
            maybe_import_enabled(Context, cb_context:resp_status(Context))
    end.

maybe_import_enabled(Context, 'success') ->
    AuthId = cb_context:auth_account_id(Context),
    JObj = cb_context:doc(Context),
    case lists:member(AuthId, wh_json:get_value(<<"pvt_tree">>, JObj, [])) of
        'false' ->
            cb_context:set_doc(Context, wh_json:delete_key(<<"enabled">>, JObj));
        'true' ->
            case wh_json:get_value(<<"enabled">>, JObj) of
                'undefined' -> Context;
                Enabled ->
                    cb_context:set_doc(Context
                                       ,wh_json:set_value(<<"pvt_enabled">>, wh_util:is_true(Enabled)
                                                          ,wh_json:delete_key(<<"enabled">>, JObj)
                                                         )
                                      )
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an account document from the database
%% @end
%%--------------------------------------------------------------------
-spec validate_delete_request(api_binary(), cb_context:context()) -> cb_context:context().
validate_delete_request(AccountId, Context) ->
    ViewOptions = [{'startkey', [AccountId]}
                   ,{'endkey', [AccountId, wh_json:new()]}
                  ],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, ?AGG_VIEW_DESCENDANTS, ViewOptions) of
        {'error', 'not_found'} -> cb_context:add_system_error('datastore_missing_view', Context);
        {'error', _} -> cb_context:add_system_error('datastore_fault', Context);
        {'ok', JObjs} ->
            case [JObj || JObj <- JObjs, wh_json:get_value(<<"id">>, JObj) =/= AccountId] of
                [] -> cb_context:set_resp_status(Context, 'success');
                _Else -> cb_context:add_system_error('account_has_descendants', Context)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an account document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_account(ne_binary(), cb_context:context()) -> cb_context:context().
load_account(AccountId, Context) ->
    leak_pvt_fields(crossbar_doc:load(AccountId, Context)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec leak_pvt_fields(cb_context:context()) -> cb_context:context().
leak_pvt_fields(Context) ->
    leak_pvt_fields(Context, cb_context:resp_status(Context)).

leak_pvt_fields(Context, 'success') ->
    leak_pvt_allow_additions(Context);
leak_pvt_fields(Context, _Status) -> Context.

-spec leak_pvt_allow_additions(cb_context:context()) -> cb_context:context().
leak_pvt_allow_additions(Context) ->
    JObj = cb_context:doc(Context),
    RespJObj = cb_context:resp_data(Context),
    AllowAdditions = wh_json:is_true(<<"pvt_wnm_allow_additions">>, JObj, 'false'),
    leak_pvt_superduper_admin(
      cb_context:set_resp_data(Context
                               ,wh_json:set_value(<<"wnm_allow_additions">>, AllowAdditions, RespJObj))
     ).

-spec leak_pvt_superduper_admin(cb_context:context()) -> cb_context:context().
leak_pvt_superduper_admin(Context) ->
    JObj = cb_context:doc(Context),
    RespJObj = cb_context:resp_data(Context),

    SuperAdmin = wh_json:is_true(<<"pvt_superduper_admin">>, JObj, 'false'),
    leak_pvt_created(
      cb_context:set_resp_data(Context
                               ,wh_json:set_value(<<"superduper_admin">>, SuperAdmin, RespJObj))
     ).

-spec leak_pvt_created(cb_context:context()) -> cb_context:context().
leak_pvt_created(Context) ->
    JObj = cb_context:doc(Context),
    RespJObj = cb_context:resp_data(Context),

    Created = wh_json:get_value(<<"pvt_created">>, JObj),
    leak_pvt_enabled(
      cb_context:set_resp_data(Context
                               ,wh_json:set_value(<<"created">>, Created, RespJObj))
     ).

-spec leak_pvt_enabled(#cb_context{}) -> #cb_context{}.
leak_pvt_enabled(Context) ->
    JObj = cb_context:doc(Context),
    RespJObj = cb_context:resp_data(Context),

    case wh_json:get_value(<<"pvt_enabled">>, JObj) of
        'true' ->
            leak_billing_mode(
              cb_context:set_resp_data(Context
                                       ,wh_json:set_value(<<"enabled">>, 'true', RespJObj))
             );
        'false' ->
            leak_billing_mode(
              cb_context:set_resp_data(Context
                                       ,wh_json:set_value(<<"enabled">>, 'false', RespJObj))
             );
        _ ->
            leak_billing_mode(Context)
    end.

-spec leak_billing_mode(cb_context:context()) -> cb_context:context().
leak_billing_mode(Context) ->
    {'ok', MasterAccount} = whapps_util:get_master_account_id(),

    AccountId = cb_context:account_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),
    RespJObj = cb_context:resp_data(Context),

    case wh_services:find_reseller_id(AccountId) of
        AuthAccountId ->
            cb_context:set_resp_data(Context
                                     ,wh_json:set_value(<<"billing_mode">>, <<"limits_only">>, RespJObj)
                                    );
        MasterAccount ->
            cb_context:set_resp_data(Context
                                     ,wh_json:set_value(<<"billing_mode">>, <<"normal">>, RespJObj)
                                    );
        _Else ->
            cb_context:set_resp_data(Context
                                     ,wh_json:set_value(<<"billing_mode">>, <<"manual">>, RespJObj)
                                    )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the children of this account
%% @end
%%--------------------------------------------------------------------
-spec load_children(ne_binary(), cb_context:context()) -> cb_context:context().
load_children(AccountId, Context) ->
    crossbar_doc:load_view(?AGG_VIEW_CHILDREN, [{<<"startkey">>, [AccountId]}
                                                ,{<<"endkey">>, [AccountId, wh_json:new()]}
                                               ], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the descendants of this account
%% @end
%%--------------------------------------------------------------------
-spec load_descendants(ne_binary(), cb_context:context()) -> cb_context:context().
load_descendants(AccountId, Context) ->
    crossbar_doc:load_view(?AGG_VIEW_DESCENDANTS, [{<<"startkey">>, [AccountId]}
                                                   ,{<<"endkey">>, [AccountId, wh_json:new()]}
                                                  ], Context, fun normalize_view_results/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the siblngs of this account
%% @end
%%--------------------------------------------------------------------
-spec load_siblings(ne_binary(), cb_context:context()) -> cb_context:context().
load_siblings(AccountId, Context) ->
    Context1 = crossbar_doc:load_view(?AGG_VIEW_PARENT, [{<<"startkey">>, AccountId}
                                                         ,{<<"endkey">>, AccountId}
                                                        ], Context),
    case cb_context:resp_status(Context1) of
        'success' ->
            load_siblings(AccountId, Context1, cb_context:doc(Context1));
        _Status ->
            cb_context:add_system_error('bad_identifier', [{'details', AccountId}],  Context)
    end.

load_siblings(_AccountId, Context, [JObj|_]) ->
    Parent = wh_json:get_value([<<"value">>, <<"id">>], JObj),
    load_children(Parent, Context);
load_siblings(AccountId, Context, _) ->
    cb_context:add_system_error('bad_identifier', [{'details', AccountId}],  Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(wh_json:object(), wh_json:objects()) -> wh_json:objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function returns the private fields to be added to a new account
%% document
%% @end
%%--------------------------------------------------------------------
-spec set_private_properties(cb_context:context()) -> cb_context:context().
set_private_properties(Context) ->
    PvtFuns = [fun add_pvt_type/1
               ,fun add_pvt_vsn/1
               ,fun maybe_add_pvt_api_key/1
               ,fun maybe_add_pvt_tree/1
               ,fun add_pvt_enabled/1
              ],
    lists:foldl(fun(F, C) -> F(C) end, Context, PvtFuns).

-spec add_pvt_type(cb_context:context()) -> cb_context:context().
add_pvt_type(Context) ->
    cb_context:set_doc(Context
                       ,wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, cb_context:doc(Context))
                      ).

-spec add_pvt_vsn(cb_context:context()) -> cb_context:context().
add_pvt_vsn(Context) ->
    cb_context:set_doc(Context, wh_json:set_value(<<"pvt_vsn">>, <<"1">>, cb_context:doc(Context))).

-spec add_pvt_enabled(cb_context:context()) -> cb_context:context().
add_pvt_enabled(Context) ->
    JObj = cb_context:doc(Context),
    case lists:reverse(wh_json:get_value(<<"pvt_tree">>, JObj, [])) of
        [ParentId | _] ->
            ParentDb = wh_util:format_account_id(ParentId, 'encoded'),
            case (not wh_util:is_empty(ParentId))
                andalso couch_mgr:open_doc(ParentDb, ParentId)
            of
                {'ok', Parent} ->
                    case wh_json:is_true(<<"pvt_enabled">>, Parent, 'true') of
                        'true' ->
                            cb_context:set_doc(Context
                                               ,wh_json:set_value(<<"pvt_enabled">>, 'true', JObj)
                                              );
                        'false' ->
                            cb_context:set_doc(Context
                                               ,wh_json:set_value(<<"pvt_enabled">>, 'false', JObj)
                                              )
                    end;
                _Else -> Context
            end;
        [] ->
            Context
    end.

-spec maybe_add_pvt_api_key(cb_context:context()) -> cb_context:context().
maybe_add_pvt_api_key(Context) ->
    JObj = cb_context:doc(Context),
    case wh_json:get_value(<<"pvt_api_key">>, JObj) of
        'undefined' ->
            APIKey = wh_util:to_hex_binary(crypto:rand_bytes(32)),
            cb_context:set_doc(Context
                               ,wh_json:set_value(<<"pvt_api_key">>, APIKey, JObj)
                              );
        _Else -> Context
    end.

-spec maybe_add_pvt_tree(cb_context:context()) -> cb_context:context().
maybe_add_pvt_tree(Context) ->
    case wh_json:get_value(<<"pvt_tree">>, cb_context:doc(Context)) of
        [_|_] -> Context;
        _Else -> add_pvt_tree(Context)
    end.

-spec add_pvt_tree(cb_context:context()) -> cb_context:context().
add_pvt_tree(Context) ->
    case create_new_tree(Context) of
        'error' ->
            cb_context:add_system_error('empty_tree_accounts_exist', Context);
        Tree ->
            cb_context:set_doc(Context
                               ,wh_json:set_value(<<"pvt_tree">>, Tree, cb_context:doc(Context))
                              )
    end.

-spec create_new_tree(cb_context:context() | api_binary()) -> ne_binaries() | 'error'.
create_new_tree('undefined') ->
    case whapps_util:get_master_account_id() of
        {'ok', MasterAccountId} -> [MasterAccountId];
        {'error', _} ->
            case whapps_util:get_all_accounts() of
                [] -> [];
                _Else -> 'error'
            end
    end;
create_new_tree(Parent) when is_binary(Parent) ->
    ParentId = wh_util:format_account_id(Parent, 'raw'),
    ParentDb = wh_util:format_account_id(Parent, 'encoded'),
    case couch_mgr:open_doc(ParentDb, ParentId) of
        {'error', _} -> create_new_tree('undefined');
        {'ok', JObj} ->
            wh_json:get_value(<<"pvt_tree">>, JObj, []) ++ [ParentId]
    end;
create_new_tree(Context) ->
    create_new_tree(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

create_new_tree(_Context, ?HTTP_PUT, [{?WH_ACCOUNTS_DB, [Parent]}]) ->
    create_new_tree(Parent);
create_new_tree(Context, _Verb, _Nouns) ->
    JObj = cb_context:auth_doc(Context),
    case wh_json:is_json_object(JObj) of
        'false' -> create_new_tree('undefined');
        'true' ->
            create_new_tree(wh_json:get_value(<<"account_id">>, JObj))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will attempt to load the context with the db name of
%% for this account
%% @end
%%--------------------------------------------------------------------
-spec load_account_db(ne_binary() | ne_binaries(), cb_context:context()) ->
                             cb_context:context().
load_account_db([AccountId|_], Context) ->
    load_account_db(AccountId, Context);
load_account_db(AccountId, Context) when is_binary(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {'ok', _} ->
            lager:debug("account ~s db exists, setting operating database as ~s", [AccountId, AccountDb]),
            cb_context:setters(Context
                               ,[{fun cb_context:set_resp_status/2, 'success'}
                                 ,{fun cb_context:set_account_db/2, AccountDb}
                                 ,{fun cb_context:set_account_id/2, AccountId}
                                ]);
        {'error', _R} ->
            lager:debug("unable to open account definition ~s/~s: ~p", [AccountDb, AccountId, _R]),
            cb_context:add_system_error('bad_identifier', [{'details', AccountId}],  Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create a new account and corresponding database
%% then spawn a short initial function
%% @end
%%--------------------------------------------------------------------
-spec create_new_account_db(cb_context:context()) -> cb_context:context().
create_new_account_db(Context) ->
    AccountDb = cb_context:account_db(Context),
    _ = ensure_accounts_db_exists(),
    case whapps_util:is_account_db(AccountDb)
        andalso couch_mgr:db_create(AccountDb)
    of
        'false' ->
            lager:debug("failed to create database: ~s", [AccountDb]),
            throw(cb_context:add_system_error('datastore_fault', Context));
        'true' ->
            lager:debug("created DB ~s", [AccountDb]),
            C = create_account_definition(prepare_context(AccountDb, Context)),
            _ = load_initial_views(C),
            _ = crossbar_bindings:map(<<"account.created">>, C),
            _ = notify_new_account(C),
            _ = wh_services:reconcile(AccountDb),
            _ = create_account_mod(cb_context:account_id(C)),
            C
    end.

-spec create_account_mod(ne_binary()) -> any().
create_account_mod(AccountId) ->
    Db = wh_util:format_account_mod_id(AccountId),
    _ = couch_mgr:db_create(Db),
    couch_mgr:revise_doc_from_file(Db
                                   ,'crossbar'
                                   ,<<"account/cdrs.json">>
                                  ).

-spec ensure_accounts_db_exists() -> 'ok'.
ensure_accounts_db_exists() ->
    case couch_mgr:db_exists(?WH_ACCOUNTS_DB) of
        'true' -> 'ok';
        'false' ->
            _ = whapps_maintenance:refresh(?WH_ACCOUNTS_DB),
            'ok'
    end.

-spec create_account_definition(cb_context:context()) -> cb_context:context().
create_account_definition(Context) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = cb_context:account_db(Context),

    TStamp = wh_util:current_tstamp(),
    Props = [{<<"_id">>, AccountId}
             ,{<<"pvt_account_id">>, AccountId}
             ,{<<"pvt_account_db">>, AccountDb}
             ,{<<"pvt_modified">>, TStamp}
             ,{<<"pvt_created">>, TStamp}
             ,{<<"pvt_vsn">>, <<"1">>}
            ],
    case couch_mgr:save_doc(AccountDb, wh_json:set_values(Props, cb_context:doc(Context))) of
        {'ok', AccountDef}->
            _ = replicate_account_definition(AccountDef),
            cb_context:setters(Context
                               ,[{fun cb_context:set_doc/2, AccountDef}
                                 ,{fun cb_context:set_resp_data/2, wh_json:public_fields(AccountDef)}
                                 ,{fun cb_context:set_resp_status/2, 'success'}
                                ]);
        {'error', _R} ->
            lager:debug("unable to create account definition: ~p", [_R]),
            throw(cb_context:add_system_error('datastore_fault', Context))
    end.

-spec load_initial_views(cb_context:context()) -> 'ok'.
load_initial_views(Context)->
    Views = whapps_maintenance:get_all_account_views(),
    whapps_util:update_views(cb_context:account_db(Context), Views, 'true').

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec replicate_account_definition(wh_json:object()) ->
                                          {'ok', wh_json:object()} |
                                          {'error', _}.
replicate_account_definition(JObj) ->
    AccountId = wh_json:get_value(<<"_id">>, JObj),
    case couch_mgr:lookup_doc_rev(?WH_ACCOUNTS_DB, AccountId) of
        {'ok', Rev} ->
            couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, wh_json:set_value(<<"_rev">>, Rev, JObj));
        _Else ->
            couch_mgr:ensure_saved(?WH_ACCOUNTS_DB, wh_json:delete_key(<<"_rev">>, JObj))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will determine if the realm in the request is
%% unique or belongs to the request being made
%% @end
%%--------------------------------------------------------------------
-spec is_unique_realm(api_binary(), ne_binary()) -> boolean().
is_unique_realm(AccountId, Realm) ->
    ViewOptions = [{<<"key">>, wh_util:to_lower_binary(Realm)}],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, ?AGG_VIEW_REALM, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> wh_json:get_value(<<"id">>, JObj) =:= AccountId;
        {'error', 'not_found'} -> 'true';
        _Else -> 'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send a notification that the account has been created
%% @end
%%--------------------------------------------------------------------
-spec notify_new_account(cb_context:context()) -> 'ok'.
%% NOTE: when the auth token is empty either signups or onboard allowed this request
%%       and they will notify once complete...
notify_new_account(Context) ->
    notify_new_account(Context, cb_context:auth_doc(Context)).

notify_new_account(_Context, 'undefined') -> 'ok';
notify_new_account(Context, _AuthDoc) ->
    JObj = cb_context:doc(Context),
    Notify = [{<<"Account-Name">>, wh_json:get_value(<<"name">>, JObj)}
              ,{<<"Account-Realm">>, wh_json:get_value(<<"realm">>, JObj)}
              ,{<<"Account-API-Key">>, wh_json:get_value(<<"pvt_api_key">>, JObj)}
              ,{<<"Account-ID">>, cb_context:account_id(Context)}
              ,{<<"Account-DB">>, cb_context:account_db(Context)}
              | wh_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    wapi_notifications:publish_new_account(Notify).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Support the depreciated billing id on the account definition, will
%% be phased out shortly
%% @end
%%--------------------------------------------------------------------
-spec support_depreciated_billing_id(api_binary(), api_binary(), cb_context:context()) ->
                                            cb_context:context().
support_depreciated_billing_id('undefined', _, Context) -> Context;
support_depreciated_billing_id(BillingId, AccountId, Context) ->
    try wh_services:set_billing_id(BillingId, AccountId) of
        'undefined' -> Context;
        Services ->
            _ = wh_services:save(Services),
            Context
    catch
        'throw':{Error, Reason} ->
            cb_context:add_validation_error(<<"billing_id">>, <<"not_found">>, wh_util:to_binary(Error), Reason)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
delete_remove_services(Context) ->
    case wh_services:delete(cb_context:account_id(Context)) of
        {'ok', _} -> delete_free_numbers(Context);
        _ -> crossbar_util:response('error', <<"unable to cancel services">>, 500, Context)
    end.

delete_free_numbers(Context) ->
    _ = wh_number_manager:free_numbers(cb_context:account_id(Context)),
    delete_remove_sip_aggregates(Context).

delete_remove_sip_aggregates(Context) ->
    ViewOptions = ['include_docs'
                   ,{'key', cb_context:account_id(Context)}
                  ],
    _ = case couch_mgr:get_results(?WH_SIP_DB, <<"credentials/lookup_by_account">>, ViewOptions) of
            {'error', _R} ->
                lager:debug("unable to clean sip_auth: ~p", [_R]);
            {'ok', JObjs} ->
                Docs = [wh_json:get_value(<<"doc">>, JObj) || JObj <- JObjs],
                couch_mgr:del_docs(?WH_SIP_DB, Docs)
        end,
    delete_remove_db(Context).

delete_remove_db(Context) ->
    Removed = case couch_mgr:open_doc(cb_context:account_db(Context), cb_context:account_id(Context)) of
                  {'ok', _} ->
                      couch_mgr:db_delete(cb_context:account_db(Context)),
                      delete_mod_dbs(Context);
                  {'error', 'not_found'} -> 'true';
                  {'error', _R} ->
                      lager:debug("failed to open account defintion ~s: ~p", [cb_context:account_id(Context), _R]),
                      'false'
              end,
    case Removed of
        'true' ->
            lager:debug("deleted db ~s", [cb_context:account_db(Context)]),
            delete_remove_from_accounts(Context);
        'false' ->
            lager:debug("failed to remove database ~s", [cb_context:account_db(Context)]),
            crossbar_util:response('error', <<"unable to remove database">>, 500, Context)
    end.

-spec delete_mod_dbs(cb_context:context()) -> 'true'.
-spec delete_mod_dbs(ne_binary(), wh_year(), wh_month()) -> 'true'.
delete_mod_dbs(Context) ->
    AccountId = cb_context:account_id(Context),
    {Year, Month, _} = erlang:date(),
    delete_mod_dbs(AccountId, Year, Month).

delete_mod_dbs(AccountId, Year, Month) ->
    Db = wh_util:format_account_mod_id(AccountId, Year, Month),
    case couch_mgr:db_delete(Db) of
        'true' ->
            lager:debug("removed account mod: ~s", [Db]),
            {PrevYear, PrevMonth} = prev_year_month(Year, Month),
            delete_mod_dbs(AccountId, PrevYear, PrevMonth);
        'false' ->
            lager:debug("failed to delete account mod: ~s", [Db]),
            'true'
    end.

-spec prev_year_month(wh_year(), wh_month()) -> {wh_year(), wh_month()}.
prev_year_month(Year, 1) -> {Year-1, 12};
prev_year_month(Year, Month) -> {Year, Month-1}.

delete_remove_from_accounts(Context) ->
    case couch_mgr:open_doc(?WH_ACCOUNTS_DB, cb_context:account_id(Context)) of
        {'ok', JObj} ->
            _ = provisioner_util:maybe_delete_account(Context),
            crossbar_doc:delete(
              cb_context:setters(Context
                                 ,[{fun cb_context:set_account_db/2, ?WH_ACCOUNTS_DB}
                                   ,{fun cb_context:set_doc/2, JObj}
                                  ])
             );
        {'error', 'not_found'} ->
            crossbar_util:response(wh_json:new(), Context);
        {'error', _R} ->
            crossbar_util:response('error', <<"unable to remove account definition">>, 500, Context)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

prev_year_month_test() ->
    {Year3, Month3} = {1776, 3},
    {Year2, Month2} = prev_year_month(Year3, Month3),
    {Year1, Month1} = prev_year_month(Year2, Month2),
    {Year12, Month12} = prev_year_month(Year1, Month1),
    {Year11, Month11} = prev_year_month(Year12, Month12),

    ?assertEqual({1776, 3}, {Year3, Month3}),
    ?assertEqual({1776, 2}, {Year2, Month2}),
    ?assertEqual({1776, 1}, {Year1, Month1}),
    ?assertEqual({1775, 12}, {Year12, Month12}),
    ?assertEqual({1775, 11}, {Year11, Month11}).

-endif.
