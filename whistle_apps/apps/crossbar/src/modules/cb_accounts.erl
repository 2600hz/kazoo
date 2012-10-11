%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
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

-export([rollback_db_creation/1]).
-export([init/0
         ,allowed_methods/0, allowed_methods/1, allowed_methods/2
         ,resource_exists/0, resource_exists/1, resource_exists/2
         ,validate/1, validate/2, validate/3
         ,put/1, put/2
         ,post/2
         ,delete/2
        ]).

-export([is_unique_realm/2]).

-include_lib("crossbar/include/crossbar.hrl").

-define(SERVER, ?MODULE).

-define(ACCOUNTS_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".accounts">>).

-define(AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(AGG_VIEW_SUMMARY, <<"accounts/listing_by_id">>).
-define(AGG_VIEW_PARENT, <<"accounts/listing_by_parent">>).
-define(AGG_VIEW_CHILDREN, <<"accounts/listing_by_children">>).
-define(AGG_VIEW_DESCENDANTS, <<"accounts/listing_by_descendants">>).
-define(AGG_VIEW_REALM, <<"accounts/listing_by_realm">>).

-define(PVT_TYPE, <<"account">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec rollback_db_creation/1 :: (#cb_context{}) -> #cb_context{}.
rollback_db_creation(#cb_context{db_name=?WH_ACCOUNTS_DB}) ->
    ok;
rollback_db_creation(#cb_context{db_name=undefined}) ->
    ok;
rollback_db_creation(#cb_context{db_name=AccountDb, account_id=AccountId}=Context) ->
    _ = delete_free_numbers(AccountId, AccountDb, Context),
    _ = delete_remove_services(AccountId, AccountDb, Context),
    _ = delete_remove_from_accounts(AccountId, AccountDb, Context),
    delete_remove_db(AccountId, AccountDb, Context).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec init/0 :: () -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"v1_resource.allowed_methods.accounts">>, ?MODULE, allowed_methods),
    _ = crossbar_bindings:bind(<<"v1_resource.resource_exists.accounts">>, ?MODULE, resource_exists),
    _ = crossbar_bindings:bind(<<"v1_resource.validate.accounts">>, ?MODULE, validate),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.put.accounts">>, ?MODULE, put),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.post.accounts">>, ?MODULE, post),
    _ = crossbar_bindings:bind(<<"v1_resource.execute.delete.accounts">>, ?MODULE, delete).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines the verbs that are appropriate for the
%% given Nouns.  IE: '/accounts/' can only accept GET and PUT
%%
%% Failure here returns 405
%% @end
%%--------------------------------------------------------------------
-spec allowed_methods/0 :: () -> http_methods().
-spec allowed_methods/1 :: (path_token()) -> http_methods().
-spec allowed_methods/2 :: (path_token(), ne_binary()) -> http_methods().
allowed_methods() ->
    ['PUT'].
allowed_methods(_) ->
    ['GET', 'PUT', 'POST', 'DELETE'].
allowed_methods(_, Path) ->
    case lists:member(Path, [<<"ancestors">>, <<"children">>, <<"descendants">>, <<"siblings">>]) of
        true -> ['GET'];
        false -> []
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns are valid.
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec resource_exists/0 :: () -> 'true'.
-spec resource_exists/1 :: (path_tokens()) -> 'true'.
-spec resource_exists/2 :: (path_tokens(), ne_binary()) -> boolean().
resource_exists() -> true.
resource_exists(_) -> true.
resource_exists(_, Path) ->
    lists:member(Path, [<<"ancestors">>, <<"children">>, <<"descendants">>, <<"siblings">>]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate/1 :: (#cb_context{}) -> #cb_context{}.
-spec validate/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
-spec validate/3 :: (#cb_context{}, path_token(), ne_binary()) -> #cb_context{}.

validate(#cb_context{req_nouns=[{?WH_ACCOUNTS_DB, _}], req_verb = <<"put">>}=Context) ->
    validate_request(undefined, prepare_context(undefined, Context)).

validate(#cb_context{req_nouns=[{?WH_ACCOUNTS_DB, _}], req_verb = <<"get">>}=Context, AccountId) ->
    load_account(AccountId, prepare_context(AccountId, Context));
validate(#cb_context{req_nouns=[{?WH_ACCOUNTS_DB, _}], req_verb = <<"put">>}=Context, AccountId) ->
    validate_request(AccountId, prepare_context(AccountId, Context));
validate(#cb_context{req_nouns=[{?WH_ACCOUNTS_DB, _}], req_verb = <<"post">>}=Context, AccountId) ->
    validate_request(AccountId, prepare_context(AccountId, Context));
validate(#cb_context{req_nouns=[{?WH_ACCOUNTS_DB, _}], req_verb = <<"delete">>}=Context, AccountId) ->
    validate_delete_request(AccountId, prepare_context(AccountId, Context));
validate(Context, AccountId) -> load_account_db(AccountId, Context).

validate(#cb_context{req_verb = <<"get">>}=Context, AccountId, <<"children">>) ->
    load_children(AccountId, prepare_context(undefined, Context));
validate(#cb_context{req_verb = <<"get">>}=Context, AccountId, <<"descendants">>) ->
    load_descendants(AccountId, prepare_context(undefined, Context));
validate(#cb_context{req_verb = <<"get">>}=Context, AccountId, <<"siblings">>) ->
    load_siblings(AccountId, prepare_context(undefined, Context)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec post/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
post(Context, AccountId) ->
    _ = cb_context:put_reqid(Context),
    case crossbar_doc:save(Context) of
        #cb_context{resp_status=success, doc=JObj}=Context1 ->
            _ = replicate_account_definition(JObj),
            support_depreciated_billing_id(wh_json:get_value(<<"billing_id">>, JObj)
                                           ,AccountId
                                           ,leak_pvt_fields(Context1));
        Else -> Else
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec put/1 :: (#cb_context{}) -> #cb_context{}.
-spec put/2 :: (#cb_context{}, path_token()) -> #cb_context{}.

put(Context) ->
    try create_new_account_db(Context) of
        C -> leak_pvt_fields(C)
    catch
        throw:#cb_context{}=C -> 
            _ = rollback_db_creation(C),
            C
    end.
            
put(Context, _) ->
    try create_new_account_db(Context) of
        C -> leak_pvt_fields(C)
    catch
        throw:#cb_context{}=C -> 
            _ = rollback_db_creation(C),
            C
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete/2 :: (#cb_context{}, path_token()) -> #cb_context{}.
delete(Context, Account) ->
    _ = cb_context:put_reqid(Context),
    AccountDb = wh_util:format_account_id(Account, encoded),
    case whapps_util:is_account_db(AccountDb) of
        false -> cb_context:add_system_error(account_not_found, Context);
        true ->
            AccountId = wh_util:format_account_id(Account, raw),
            rollback_db_creation(Context#cb_context{db_name=AccountDb, account_id=AccountId})
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec prepare_context/2 :: ('undefined'|ne_binary(), #cb_context{}) -> #cb_context{}.
prepare_context(undefined, Context) ->
    Context#cb_context{db_name=?WH_ACCOUNTS_DB};
prepare_context(Account, Context) ->
    AccountId = wh_util:format_account_id(Account, raw),
    AccountDb = wh_util:format_account_id(Account, encoded),
    Context#cb_context{db_name=AccountDb, account_id=AccountId}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_request/2 :: ('undefined'|ne_binary(), #cb_context{}) -> #cb_context{}.
validate_request(AccountId, Context) ->
    ensure_account_has_realm(AccountId, Context).

-spec ensure_account_has_realm/2 :: ('undefined'|ne_binary(), #cb_context{}) -> #cb_context{}.
ensure_account_has_realm(AccountId, #cb_context{req_data=JObj}=Context) ->
    case wh_json:get_ne_value(<<"realm">>, JObj) of
        undefined ->
            RealmSuffix = whapps_config:get_binary(?ACCOUNTS_CONFIG_CAT, <<"account_realm_suffix">>, <<"sip.2600hz.com">>),
            Strength = whapps_config:get_integer(?ACCOUNTS_CONFIG_CAT, <<"random_realm_strength">>, 3),
            J = wh_json:set_value(<<"realm">>, list_to_binary([wh_util:rand_hex_binary(Strength), ".", RealmSuffix]), JObj),
            cleanup_leaky_keys(AccountId, Context#cb_context{req_data=J});
        _Else -> cleanup_leaky_keys(AccountId, Context)
    end.

-spec cleanup_leaky_keys/2 :: ('undefined'|ne_binary(), #cb_context{}) -> #cb_context{}.
cleanup_leaky_keys(AccountId, #cb_context{req_data=JObj}=Context) ->
    RemoveKeys = [<<"wnm_allow_additions">>
                      ,<<"superduper_admin">>
                      ,<<"billing_mode">>
                 ],
    validate_realm_is_unique(AccountId, Context#cb_context{req_data=wh_json:delete_keys(RemoveKeys, JObj)}).

-spec validate_realm_is_unique/2 :: ('undefined'|ne_binary(), #cb_context{}) -> #cb_context{}.
validate_realm_is_unique(AccountId, #cb_context{doc=JObj}=Context) ->
    Realm = wh_json:get_ne_value(<<"realm">>, JObj),
    case is_unique_realm(AccountId, Realm) of
        true -> validate_account_schema(AccountId, Context);
        false ->
            C = cb_context:add_validation_error([<<"realm">>]
                                                   ,<<"unique">>
                                                   ,<<"Account realm already in use">>
                                                    ,Context),
            validate_account_schema(AccountId, C)
    end.

-spec validate_account_schema/2 :: ('undefined'|ne_binary(), #cb_context{}) -> #cb_context{}.
validate_account_schema(AccountId, Context) ->
    C = cb_context:validate_request_data(<<"accounts">>, Context),
    finalize_request_validation(AccountId, C).

-spec finalize_request_validation/2 :: ('undefined'|ne_binary(), #cb_context{}) -> #cb_context{}.
finalize_request_validation(undefined, Context) ->
    set_private_properties(Context); 
finalize_request_validation(AccountId, #cb_context{doc=JObj}=Context) ->
    crossbar_doc:load_merge(AccountId, JObj, Context).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an account document from the database
%% @end
%%--------------------------------------------------------------------
-spec validate_delete_request/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
validate_delete_request(AccountId, Context) ->
    ViewOptions = [{<<"startkey">>, [AccountId]}
                   ,{<<"endkey">>, [AccountId, wh_json:new()]}
                  ],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, ?AGG_VIEW_DESCENDANTS, ViewOptions) of
        {error, not_found} -> cb_context:add_system_error(database_missing_view, Context);
        {error, _} -> cb_context:add_system_error(database_fault, Context);
        {ok, JObjs} ->
            case [JObj || JObj <- JObjs, wh_json:get_value(<<"id">>, JObj) =/= AccountId] of
                [] -> Context#cb_context{resp_status=success};
                _Else -> cb_context:add_system_error(account_has_descendants, Context)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an account document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_account/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_account(AccountId, Context) ->
    leak_pvt_fields(crossbar_doc:load(AccountId, Context)).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec leak_pvt_fields/1 :: (#cb_context{}) -> #cb_context{}.
leak_pvt_fields(#cb_context{resp_status=success}=Context) ->
    leak_pvt_allow_additions(Context);
leak_pvt_fields(Context) -> Context.

-spec leak_pvt_allow_additions/1 :: (#cb_context{}) -> #cb_context{}.
leak_pvt_allow_additions(#cb_context{doc=JObj, resp_data=RespJObj}=Context) ->
    AllowAdditions = wh_json:is_true(<<"pvt_wnm_allow_additions">>, JObj, false),
    leak_pvt_superduper_admin(Context#cb_context{resp_data=wh_json:set_value(<<"wnm_allow_additions">>, AllowAdditions, RespJObj)}).

-spec leak_pvt_superduper_admin/1 :: (#cb_context{}) -> #cb_context{}.
leak_pvt_superduper_admin(#cb_context{doc=JObj, resp_data=RespJObj}=Context) ->
    SuperAdmin = wh_json:is_true(<<"pvt_superduper_admin">>, JObj, false),
    leak_billing_mode(Context#cb_context{resp_data=wh_json:set_value(<<"superduper_admin">>, SuperAdmin, RespJObj)}).

-spec leak_billing_mode/1 :: (#cb_context{}) -> #cb_context{}.
leak_billing_mode(#cb_context{auth_account_id=AuthAccountId, account_id=AccountId, resp_data=RespJObj}=Context) ->
    {ok, MasterAccount} = whapps_util:get_master_account_id(),
    case wh_services:find_reseller_id(AccountId) of
        AuthAccountId ->
            Context#cb_context{resp_data=wh_json:set_value(<<"billing_mode">>, <<"limits_only">>, RespJObj)};
        MasterAccount -> 
            Context#cb_context{resp_data=wh_json:set_value(<<"billing_mode">>, <<"normal">>, RespJObj)};
        _Else -> 
            Context#cb_context{resp_data=wh_json:set_value(<<"billing_mode">>, <<"manual">>, RespJObj)}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the children of this account
%% @end
%%--------------------------------------------------------------------
-spec load_children/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
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
-spec load_descendants/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
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
-spec load_siblings/2 :: (ne_binary(), #cb_context{}) -> #cb_context{}.
load_siblings(AccountId, Context) ->
    case crossbar_doc:load_view(?AGG_VIEW_PARENT, [{<<"startkey">>, AccountId}
                                                   ,{<<"endkey">>, AccountId}
                                                  ], Context) of
        #cb_context{resp_status=success, doc=[JObj|_]} ->
            Parent = wh_json:get_value([<<"value">>, <<"id">>], JObj),
            load_children(Parent, Context);
        _Else -> cb_context:add_system_error(account_not_found, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results/2 :: (wh_json:json_object(), wh_json:json_objects()) -> wh_json:json_objects().
normalize_view_results(JObj, Acc) ->
    [wh_json:get_value(<<"value">>, JObj)|Acc].

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function returns the private fields to be added to a new account
%% document
%% @end
%%--------------------------------------------------------------------
-spec set_private_properties/1 :: (#cb_context{}) -> #cb_context{}.
set_private_properties(Context) ->
    PvtFuns = [fun add_pvt_type/1
               ,fun add_pvt_vsn/1
               ,fun maybe_add_pvt_api_key/1
               ,fun maybe_add_pvt_tree/1
              ],
    lists:foldl(fun(F, C) -> F(C) end, Context, PvtFuns).

-spec add_pvt_type/1 :: (#cb_context{}) -> #cb_context{}.
add_pvt_type(#cb_context{doc=JObj}=Context) ->
    Context#cb_context{doc=wh_json:set_value(<<"pvt_type">>, ?PVT_TYPE, JObj)}.

-spec add_pvt_vsn/1 :: (#cb_context{}) -> #cb_context{}.
add_pvt_vsn(#cb_context{doc=JObj}=Context) ->
    Context#cb_context{doc=wh_json:set_value(<<"pvt_vsn">>, <<"1">>, JObj)}.

-spec maybe_add_pvt_api_key/1 :: (#cb_context{}) -> #cb_context{}.
maybe_add_pvt_api_key(#cb_context{doc=JObj}=Context) ->
    case wh_json:get_value(<<"pvt_api_key">>, JObj) of
        undefined ->
            APIKey = wh_util:to_hex_binary(crypto:rand_bytes(32)),
            Context#cb_context{doc=wh_json:set_value(<<"pvt_api_key">>, APIKey, JObj)};
        _Else -> Context
    end.

-spec maybe_add_pvt_tree/1 :: (#cb_context{}) -> #cb_context{}.
maybe_add_pvt_tree(#cb_context{doc=JObj}=Context) ->
    case wh_json:get_value(<<"pvt_tree">>, JObj) of
        [_|_] -> Context;
        _Else -> add_pvt_tree(Context)
    end.

-spec add_pvt_tree/1 :: (#cb_context{}) -> #cb_context{}.
add_pvt_tree(#cb_context{doc=JObj}=Context) ->
    case create_new_tree(Context) of
        error ->
            cb_context:add_system_error(empty_tree_accounts_exist, Context);
        Tree ->
            Context#cb_context{doc=wh_json:set_value(<<"pvt_tree">>, Tree, JObj)}
    end.

-spec create_new_tree/1 :: (#cb_context{}|'undefined'|ne_binary()) -> [ne_binary(),...]|[]|'error'.
create_new_tree(#cb_context{req_nouns=[{?WH_ACCOUNTS_DB, [Parent]}], req_verb = <<"put">>}) ->
    create_new_tree(Parent);
create_new_tree(#cb_context{auth_doc=JObj}) ->
    case wh_json:is_json_object(JObj) of
        false -> create_new_tree(undefined);
        true -> 
            create_new_tree(wh_json:get_value(<<"account_id">>, JObj))
    end;
create_new_tree(undefined) ->
    case whapps_util:get_master_account_id() of
        {ok, MasterAccountId} -> [MasterAccountId];
        {error, _} ->
            case whapps_util:get_all_accounts() of
                [] -> [];
                _Else -> error
            end
    end;
create_new_tree(Parent) ->
    ParentId = wh_util:format_account_id(Parent, raw),
    ParentDb = wh_util:format_account_id(Parent, encoded),
    case couch_mgr:open_doc(ParentDb, ParentId) of
        {error, _} -> create_new_tree(undefined);
        {ok, JObj} ->
            wh_json:get_value(<<"pvt_tree">>, JObj, []) ++ [ParentId]
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will attempt to load the context with the db name of
%% for this account
%% @end
%%--------------------------------------------------------------------
-spec load_account_db/2 :: (ne_binary() | [ne_binary(),...], #cb_context{}) -> #cb_context{}.
load_account_db([AccountId|_], Context) ->
    load_account_db(AccountId, Context);
load_account_db(AccountId, Context) when is_binary(AccountId) ->
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_cache_doc(AccountDb, AccountId) of
        {ok, _} ->
            lager:debug("account ~s db exists, setting operating database as ~s", [AccountId, AccountDb]),
            Context#cb_context{resp_status = success
                               ,db_name = AccountDb
                               ,account_id = AccountId
                              };
        {error, _R} ->
            lager:debug("unable to open account definition ~s/~s: ~p", [AccountDb, AccountId, _R]),
            cb_context:add_system_error(account_not_found, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will create a new account and corresponding database
%% then spawn a short initial function
%% @end
%%--------------------------------------------------------------------
-spec create_new_account_db/1 :: (#cb_context{}) -> #cb_context{}.
create_new_account_db(#cb_context{doc=Doc}=Context) ->
    AccountId = wh_json:get_value(<<"_id">>, Doc, couch_mgr:get_uuid()),
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    _ = ensure_accounts_db_exists(),
    case couch_mgr:db_create(AccountDb) of
        false ->
            lager:debug("failed to create database: ~s", [AccountDb]),
            throw(cb_context:add_system_error(database_fault, Context));
        true ->
            lager:debug("created DB ~s", [AccountDb]),
            C = prepare_context(AccountDb, Context),
            _ = create_account_definition(C),
            _ = load_initial_views(C),
            _ = crossbar_bindings:map(<<"account.created">>, C),
            _ = notfy_new_account(C),
            C
    end.

-spec ensure_accounts_db_exists/0 :: () -> 'ok'.
ensure_accounts_db_exists() ->
    case couch_mgr:db_exists(?WH_ACCOUNTS_DB) of
        true -> ok;
        false -> 
            _ = couch_mgr:db_create(?WH_ACCOUNTS_DB),
            _ = couch_mgr:revise_doc_from_file(?WH_ACCOUNTS_DB, crossbar, ?ACCOUNTS_AGG_VIEW_FILE),
            couch_mgr:revise_doc_from_file(?WH_ACCOUNTS_DB, crossbar, ?MAINTENANCE_VIEW_FILE),
            ok
    end.

-spec create_account_definition/1 :: (#cb_context{}) -> #cb_context{}.
create_account_definition(#cb_context{doc=JObj, account_id=AccountId, db_name=AccountDb}=Context) ->
    TStamp = wh_util:current_tstamp(),
    Props = [{<<"_id">>, AccountId}
             ,{<<"pvt_account_id">>, AccountId}
             ,{<<"pvt_account_db">>, AccountDb}
             ,{<<"pvt_modified">>, TStamp}
             ,{<<"pvt_created">>, TStamp}
             ,{<<"pvt_vsn">>, <<"1">>}
            ],
    J = wh_json:set_values(Props, JObj),
    case couch_mgr:save_doc(AccountDb, J) of
        {ok, AccountDef}-> 
            _ = replicate_account_definition(AccountDef),
            ok;
        {error, _R} -> 
            lager:debug("unable to create account definition: ~p", [_R]),
            throw(cb_context:add_system_error(database_fault, Context))
    end.

-spec load_initial_views/1 :: (#cb_context{}) -> #cb_context{}.
load_initial_views(#cb_context{db_name=AccountDb})->
    Views = whapps_maintenance:get_all_account_views(),
    whapps_util:update_views(AccountDb, Views, true).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec replicate_account_definition/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_object()} | {'error', _}.
replicate_account_definition(JObj) ->
    AccountId = wh_json:get_value(<<"_id">>, JObj),
    case couch_mgr:lookup_doc_rev(?WH_ACCOUNTS_DB, AccountId) of
        {ok, Rev} ->
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
-spec is_unique_realm/2 :: (ne_binary() | 'undefined', ne_binary()) -> boolean().
is_unique_realm(AccountId, Realm) ->
    ViewOptions = [{<<"key">>, Realm}],
    case couch_mgr:get_results(?WH_ACCOUNTS_DB, ?AGG_VIEW_REALM, ViewOptions) of
        {ok, []} -> true;
        {ok, [JObj]} -> wh_json:get_value(<<"id">>, JObj) =:= AccountId;
        {error, not_found} -> true;
        _Else -> false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Send a notification that the account has been created
%% @end
%%--------------------------------------------------------------------
-spec notfy_new_account/1 :: (#cb_context{}) -> ok.
%% NOTE: when the auth token is empty either signups or onboard allowed this request
%%       and they will notify once complete...
notfy_new_account(#cb_context{auth_doc = undefined}) ->
    ok;
notfy_new_account(#cb_context{doc = JObj}) ->
    Notify = [{<<"Account-Name">>, wh_json:get_value(<<"name">>, JObj)}
              ,{<<"Account-Realm">>, wh_json:get_value(<<"realm">>, JObj)}
              ,{<<"Account-API-Key">>, wh_json:get_value(<<"pvt_api_key">>, JObj)}
              ,{<<"Account-ID">>, wh_json:get_value(<<"pvt_account_id">>, JObj)}
              ,{<<"Account-DB">>, wh_json:get_value(<<"pvt_account_db">>, JObj)}
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
-spec support_depreciated_billing_id/3 :: (ne_binary(), ne_binary(), #cb_context{}) -> #cb_context{}.
support_depreciated_billing_id(undefined, _, Context) ->
    Context;
support_depreciated_billing_id(BillingId, AccountId, Context) ->
    try wh_services:set_billing_id(BillingId, AccountId) of
        undefined -> Context;
        Services ->
            _ = wh_services:save(Services),
            Context
    catch
        throw:{Error, Reason} ->
            cb_context:add_validation_error(<<"billing_id">>, wh_util:to_binary(Error), Reason)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------

delete_free_numbers(AccountId, AccountDb, Context) ->
    ok = wh_number_manager:free_numbers(AccountId),
    delete_remove_services(AccountId, AccountDb, Context).

delete_remove_services(AccountId, AccountDb, Context) ->
    case wh_services:delete(AccountId) of
        {ok, _} -> delete_remove_db(AccountId, AccountDb, Context);
        _ -> crossbar_util:response(error, <<"unable to cancel services">>, 500, Context)
    end.

delete_remove_db(AccountId, AccountDb, Context) ->
   Removed = case couch_mgr:open_doc(AccountDb, AccountId) of
                 {ok, _} -> couch_mgr:db_delete(AccountDb);
                 {error, not_found} -> true;
                 {error, _R} ->
                     lager:debug("failed to open account defintion ~s: ~p", [AccountId, _R]),
                     false
             end,
    case Removed of
        true ->
            lager:debug("deleted db ~s", [AccountDb]),
            delete_remove_from_accounts(AccountId, AccountDb, Context);
        false ->
            lager:debug("failed to remove database ~s", [AccountDb]),
            crossbar_util:response(error, <<"unable to remove database">>, 500, Context)
    end.

delete_remove_from_accounts(AccountId, _, Context) ->
    case couch_mgr:open_doc(?WH_ACCOUNTS_DB, AccountId) of
        {ok, JObj} ->
            crossbar_doc:delete(Context#cb_context{db_name=?WH_ACCOUNTS_DB
                                                   ,doc=JObj
                                                  });
        {error, not_found} -> crossbar_util:response(wh_json:new(), Context);
        {error, _R} ->
            crossbar_util:response(error, <<"unable to remove account definition">>, 500, Context)
    end.
