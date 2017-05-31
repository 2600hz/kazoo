%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz INC
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
        ,validate_resource/1, validate_resource/2, validate_resource/3
        ,validate/1, validate/2, validate/3
        ,put/1, put/2, put/3
        ,post/2, post/3
        ,delete/2, delete/3
        ,patch/2

        ,replicate_account_definition/1
        ]).

-export([notify_new_account/1]).
-export([is_unique_realm/2]).

%% needed for API docs in cb_api_endpoints
-export([allowed_methods_on_account/2]).

-compile({no_auto_import,[put/2]}).

-include("crossbar.hrl").

-define(SERVER, ?MODULE).

-define(ACCOUNTS_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".accounts">>).
-define(DEFAULT_TIMEZONE
       ,kapps_config:get_ne_binary(<<"accounts">>, <<"default_timezone">>, <<"America/Los_Angeles">>)).

-define(AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(AGG_VIEW_SUMMARY, <<"accounts/listing_by_id">>).
-define(AGG_VIEW_PARENT, <<"accounts/listing_by_parent">>).
-define(AGG_VIEW_CHILDREN, <<"accounts/listing_by_children">>).
-define(AGG_VIEW_DESCENDANTS, <<"accounts/listing_by_descendants">>).

-define(AGG_VIEW_REALM, <<"accounts/listing_by_realm">>).
-define(AGG_VIEW_NAME, <<"accounts/listing_by_name">>).

-define(PVT_TYPE, kz_account:type()).
-define(CHILDREN, <<"children">>).
-define(DESCENDANTS, <<"descendants">>).
-define(SIBLINGS, <<"siblings">>).
-define(API_KEY, <<"api_key">>).
-define(TREE, <<"tree">>).
-define(PARENTS, <<"parents">>).
-define(RESELLER, <<"reseller">>).

-define(REMOVE_SPACES, [<<"realm">>]).
-define(MOVE, <<"move">>).

-spec init() -> 'ok'.
init() ->
    Bindings = [{<<"*.allowed_methods.accounts">>, 'allowed_methods'}
               ,{<<"*.resource_exists.accounts">>, 'resource_exists'}
               ,{<<"*.validate_resource.accounts">>, 'validate_resource'}
               ,{<<"*.validate.accounts">>, 'validate'}
               ,{<<"*.execute.put.accounts">>, 'put'}
               ,{<<"*.execute.post.accounts">>, 'post'}
               ,{<<"*.execute.patch.accounts">>, 'patch'}
               ,{<<"*.execute.delete.accounts">>, 'delete'}
               ],
    cb_modules_util:bind(?MODULE, Bindings).

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

allowed_methods(AccountId) ->
    allowed_methods_on_account(AccountId, kapps_util:get_master_account_id()).

-spec allowed_methods_on_account(ne_binary(), {'ok', ne_binary()} | {'error', any()}) ->
                                        http_methods().
allowed_methods_on_account(AccountId, {'ok', AccountId}) ->
    lager:debug("accessing master account, disallowing DELETE"),
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_PATCH];
allowed_methods_on_account(_AccountId, {'ok', _MasterId}) ->
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_PATCH, ?HTTP_DELETE];
allowed_methods_on_account(_AccountId, {'error', _E}) ->
    lager:debug("failed to get master account id: ~p", [_E]),
    lager:info("disallowing DELETE while we can't determine the master account id"),
    [?HTTP_GET, ?HTTP_PUT, ?HTTP_POST, ?HTTP_PATCH].

allowed_methods(_AccountId, ?MOVE) ->
    [?HTTP_POST];
allowed_methods(_AccountId, ?RESELLER) ->
    [?HTTP_PUT, ?HTTP_DELETE];
allowed_methods(_AccountId, ?CHILDREN) -> [?HTTP_GET];
allowed_methods(_AccountId, ?DESCENDANTS) -> [?HTTP_GET];
allowed_methods(_AccountId, ?SIBLINGS) -> [?HTTP_GET];
allowed_methods(_AccountId, ?API_KEY) -> [?HTTP_GET];
allowed_methods(_AccountId, ?TREE) -> [?HTTP_GET];
allowed_methods(_AccountId, ?PARENTS) -> [?HTTP_GET].

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
    Paths =  [?CHILDREN
             ,?DESCENDANTS
             ,?SIBLINGS
             ,?API_KEY
             ,?MOVE
             ,?TREE
             ,?PARENTS
             ,?RESELLER
             ],
    lists:member(Path, Paths).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the provided list of Nouns and Resource Ids are valid.
%% If valid, updates Context with account data
%%
%% Failure here returns 404
%% @end
%%--------------------------------------------------------------------
-spec validate_resource(cb_context:context()) -> cb_context:context().
-spec validate_resource(cb_context:context(), path_token()) -> cb_context:context().
-spec validate_resource(cb_context:context(), path_token(), ne_binary()) -> cb_context:context().
validate_resource(Context) ->
    Context.
validate_resource(Context, AccountId) ->
    load_account_db(Context, AccountId).
validate_resource(Context, AccountId, _Path) ->
    load_account_db(Context, AccountId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400
%% @end
%%--------------------------------------------------------------------
-spec validate(cb_context:context()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token()) ->
                      cb_context:context().
-spec validate(cb_context:context(), path_token(), ne_binary()) ->
                      cb_context:context().

validate(Context) ->
    validate_accounts(Context, cb_context:req_verb(Context)).

-spec validate_accounts(cb_context:context(), http_method()) -> cb_context:context().
validate_accounts(Context, ?HTTP_PUT) ->
    validate_request('undefined', prepare_context('undefined', Context)).

validate(Context, AccountId) ->
    validate_account(Context, AccountId, cb_context:req_verb(Context)).

-spec validate_account(cb_context:context(), ne_binary(), http_method()) -> cb_context:context().
validate_account(Context, AccountId, ?HTTP_GET) ->
    load_account(AccountId, prepare_context(AccountId, Context));
validate_account(Context, _AccountId, ?HTTP_PUT) ->
    validate_request('undefined', prepare_context('undefined', Context));
validate_account(Context, AccountId, ?HTTP_POST) ->
    validate_request(AccountId, prepare_context(AccountId, Context));
validate_account(Context, AccountId, ?HTTP_PATCH) ->
    validate_patch_request(AccountId, prepare_context(AccountId, Context));
validate_account(Context, AccountId, ?HTTP_DELETE) ->
    validate_delete_request(AccountId, prepare_context(AccountId, Context)).

validate(Context, AccountId, PathToken) ->
    validate_account_path(Context, AccountId, PathToken, cb_context:req_verb(Context)).

-spec validate_account_path(cb_context:context(), ne_binary(), ne_binary(), http_method()) ->
                                   cb_context:context().
validate_account_path(Context, AccountId, ?CHILDREN, ?HTTP_GET) ->
    load_children(AccountId, prepare_context('undefined', Context));
validate_account_path(Context, AccountId, ?DESCENDANTS, ?HTTP_GET) ->
    load_descendants(AccountId, prepare_context('undefined', Context));
validate_account_path(Context, AccountId, ?SIBLINGS, ?HTTP_GET) ->
    load_siblings(AccountId, prepare_context('undefined', Context));
validate_account_path(Context, AccountId, ?PARENTS, ?HTTP_GET) ->
    load_parents(AccountId, prepare_context('undefined', Context));
validate_account_path(Context, AccountId, ?RESELLER, ?HTTP_PUT) ->
    case cb_context:is_superduper_admin(Context) of
        'true' -> load_account(AccountId, prepare_context(AccountId, Context));
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate_account_path(Context, AccountId, ?RESELLER, ?HTTP_DELETE) ->
    case cb_context:is_superduper_admin(Context) of
        'true' -> load_account(AccountId, prepare_context(AccountId, Context));
        'false' -> cb_context:add_system_error('forbidden', Context)
    end;
validate_account_path(Context, AccountId, ?API_KEY, ?HTTP_GET) ->
    Context1 = crossbar_doc:load(AccountId, prepare_context('undefined', Context), ?TYPE_CHECK_OPTION(?PVT_TYPE)),
    case cb_context:resp_status(Context1) of
        'success' ->
            JObj = cb_context:doc(Context1),
            ApiKey = kz_account:api_key(JObj),
            RespJObj = kz_json:from_list([{<<"api_key">>, ApiKey}]),
            cb_context:set_resp_data(Context1, RespJObj);
        _Else -> Context1
    end;
validate_account_path(Context, AccountId, ?MOVE, ?HTTP_POST) ->
    Data = cb_context:req_data(Context),
    case kz_json:get_binary_value(<<"to">>, Data) of
        'undefined' ->
            cb_context:add_validation_error(<<"to">>
                                           ,<<"required">>
                                           ,kz_json:from_list(
                                              [{<<"message">>, <<"Field 'to' is required">>}]
                                             )
                                           ,Context
                                           );
        ToAccount ->
            case validate_move(kapps_config:get_ne_binary(?ACCOUNTS_CONFIG_CAT, <<"allow_move">>, <<"superduper_admin">>)
                              ,Context
                              ,AccountId
                              ,ToAccount
                              )
            of
                'true' -> cb_context:set_resp_status(Context, 'success');
                'false' -> cb_context:add_system_error('forbidden', Context)
            end
    end;
validate_account_path(Context, AccountId, ?TREE, ?HTTP_GET) ->
    Context1 = crossbar_doc:load(AccountId, prepare_context('undefined', Context), ?TYPE_CHECK_OPTION(?PVT_TYPE)),
    case cb_context:resp_status(Context1) of
        'success' -> load_account_tree(Context1);
        _Else -> Context1
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, AccountId) ->
    {'ok', Existing} = kz_account:fetch(AccountId),
    Context1 = crossbar_doc:save(Context),

    case cb_context:resp_status(Context1) of
        'success' ->
            _ = kz_util:spawn(fun notification_util:maybe_notify_account_change/2, [Existing, cb_context:doc(Context1)]),
            _ = kz_util:spawn(fun provisioner_util:maybe_update_account/1, [Context1]),

            JObj = cb_context:doc(Context1),
            _ = replicate_account_definition(JObj),
            support_depreciated_billing_id(kz_json:get_value(<<"billing_id">>, JObj)
                                          ,AccountId
                                          ,leak_pvt_fields(AccountId, Context1)
                                          );
        _Status -> Context1
    end.

post(Context, AccountId, ?MOVE) ->
    case cb_context:resp_status(Context) of
        'success' -> move_account(Context, AccountId);
        _Status -> Context
    end.

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, AccountId) ->
    post(Context, AccountId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec put(cb_context:context()) -> cb_context:context().
-spec put(cb_context:context(), api_binary()) -> cb_context:context().
-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().

put(Context) ->
    put(Context, 'undefined').

put(Context, PathAccountId) ->
    JObj = cb_context:doc(Context),
    NewAccountId = kz_doc:id(JObj, kz_datamgr:get_uuid()),
    try create_new_account_db(prepare_context(NewAccountId, Context)) of
        C ->
            Tree = kz_account:tree(JObj),
            _ = maybe_update_descendants_count(Tree),
            _ = create_apps_store_doc(NewAccountId),
            leak_pvt_fields(PathAccountId, C)
    catch
        'throw':C ->
            lager:debug("failed to create account, unrolling changes"),

            case cb_context:is_context(C) of
                'true' -> delete(C, NewAccountId);
                'false' ->
                    _ = delete(Context, NewAccountId),
                    cb_context:add_system_error('unspecified_fault', <<"internal error, unable to create the account">>, Context)
            end;
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:debug("unexpected failure when creating account: ~s: ~p", [_E, _R]),
            kz_util:log_stacktrace(ST),
            _ = delete(Context, NewAccountId),
            cb_context:add_system_error('unspecified_fault', <<"internal error, unable to create the account">>, Context)
    end.

put(Context, AccountId, ?RESELLER) ->
    case whs_account_conversion:promote(AccountId) of
        {'error', 'master_account'} -> cb_context:add_system_error('forbidden', Context);
        {'error', 'reseller_descendants'} -> cb_context:add_system_error('account_has_descendants', Context);
        'ok' -> load_account(AccountId, Context)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(cb_context:context(), path_token()) -> cb_context:context().
-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().

delete(Context, Account) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case kapps_util:is_account_db(AccountDb) of
        'false' ->
            cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, AccountId}]),  Context);
        'true' ->
            Context1 = delete_remove_services(prepare_context(Context, AccountId, AccountDb)),
            _ = maybe_update_descendants_count(kz_account:tree(cb_context:doc(Context1))),
            Context1
    end.

delete(Context, AccountId, ?RESELLER) ->
    case whs_account_conversion:demote(AccountId) of
        {'error', 'master_account'} -> cb_context:add_system_error('forbidden', Context);
        {'error', 'reseller_descendants'} -> cb_context:add_system_error('account_has_descendants', Context);
        'ok' -> load_account(AccountId, Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec maybe_update_descendants_count(ne_binaries()) -> 'ok'.
maybe_update_descendants_count([]) -> 'ok';
maybe_update_descendants_count(Tree) ->
    _ = kz_util:spawn(fun crossbar_util:descendants_count/1, [lists:last(Tree)]),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec create_apps_store_doc(ne_binary()) -> 'ok'.
create_apps_store_doc(AccountId) ->
    _ = kz_util:spawn(fun cb_apps_util:create_apps_store_doc/1, [AccountId]),
    'ok'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec validate_move(ne_binary(), cb_context:context(), ne_binary(), ne_binary()) -> boolean().
validate_move(<<"superduper_admin">>, Context, _, _) ->
    lager:debug("using superduper_admin flag to allow move account"),
    AuthId = kz_json:get_value(<<"account_id">>, cb_context:auth_doc(Context)),
    kz_util:is_system_admin(AuthId);
validate_move(<<"tree">>, Context, MoveAccount, ToAccount) ->
    lager:debug("using tree to allow move account"),
    AuthId = kz_doc:account_id(cb_context:auth_doc(Context)),
    MoveTree = crossbar_util:get_tree(MoveAccount),
    ToTree = crossbar_util:get_tree(ToAccount),
    L = lists:foldl(fun(Id, Acc) ->
                            case lists:member(Id, ToTree) of
                                'false' -> Acc;
                                'true' -> [Id|Acc]
                            end
                    end
                   ,[]
                   ,MoveTree
                   ),
    lists:member(AuthId, L);
validate_move(_Type, _, _, _) ->
    lager:error("unknow move type ~p", [_Type]),
    'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec move_account(cb_context:context(), ne_binary()) -> cb_context:context().
move_account(Context, AccountId) ->
    Data = cb_context:req_data(Context),
    ToAccount = kz_json:get_binary_value(<<"to">>, Data),
    case crossbar_util:move_account(AccountId, ToAccount) of
        {'error', 'forbidden'} -> cb_context:add_system_error('forbidden', Context);
        {'error', _E} -> cb_context:add_system_error('datastore_fault', Context);
        {'ok', _} ->
            load_account(AccountId, prepare_context(AccountId, Context))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec prepare_context(api_binary(), cb_context:context()) -> cb_context:context().
-spec prepare_context(cb_context:context(), ne_binary(), ne_binary()) -> cb_context:context().
prepare_context('undefined', Context) ->
    cb_context:set_account_db(Context, ?KZ_ACCOUNTS_DB);
prepare_context(Account, Context) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
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
-spec validate_request(api_binary(), cb_context:context()) ->
                              cb_context:context().
validate_request(AccountId, Context) ->
    ValidateFuns = [fun ensure_account_has_realm/2
                   ,fun ensure_account_has_timezone/2
                   ,fun remove_spaces/2
                   ,fun cleanup_leaky_keys/2
                   ,fun validate_realm_is_unique/2
                   ,fun validate_account_name_is_unique/2
                   ,fun validate_account_schema/2
                   ,fun disallow_direct_clients/2
                   ],
    lists:foldl(fun(F, C) -> F(AccountId, C) end
               ,Context
               ,ValidateFuns
               ).

-spec ensure_account_has_realm(api_binary(), cb_context:context()) -> cb_context:context().
ensure_account_has_realm(_AccountId, Context) ->
    JObj = cb_context:req_data(Context),
    case kz_account:realm(JObj) of
        'undefined' ->
            Realm = random_realm(),
            lager:debug("req has no realm, creating random realm '~s'", [Realm]),
            cb_context:set_req_data(Context, kz_account:set_realm(JObj, Realm));
        _Realm ->
            lager:debug("req has realm '~s'", [_Realm]),
            Context
    end.

-spec ensure_account_has_timezone(api_binary(), cb_context:context()) -> cb_context:context().
ensure_account_has_timezone(_AccountId, Context) ->
    JObj = cb_context:req_data(Context),
    Timezone = kz_json:get_value(<<"timezone">>, JObj, get_timezone_from_parent(Context)),
    cb_context:set_req_data(Context, kz_account:set_timezone(JObj, Timezone)).

-spec get_timezone_from_parent(cb_context:context()) -> ne_binary().
get_timezone_from_parent(Context) ->
    case create_new_tree(Context) of
        [_|_]=Tree -> kz_account:timezone(lists:last(Tree));
        [] -> ?DEFAULT_TIMEZONE
    end.

-spec random_realm() -> ne_binary().
random_realm() ->
    RealmSuffix = kapps_config:get_binary(?ACCOUNTS_CONFIG_CAT, <<"account_realm_suffix">>, <<"sip.2600hz.com">>),
    Strength = kapps_config:get_integer(?ACCOUNTS_CONFIG_CAT, <<"random_realm_strength">>, 3),
    list_to_binary([kz_binary:rand_hex(Strength), ".", RealmSuffix]).

-spec remove_spaces(api_binary(), cb_context:context()) -> cb_context:context().
remove_spaces(_AccountId, Context) ->
    ReqData = lists:foldl(fun remove_spaces_fold/2
                         ,cb_context:req_data(Context)
                         ,?REMOVE_SPACES
                         ),
    cb_context:set_req_data(Context, ReqData).

-spec remove_spaces_fold(kz_json:path(), kz_json:object()) -> kz_json:object().
remove_spaces_fold(Key, Acc) ->
    case kz_json:get_value(Key, Acc) of
        'undefined' -> Acc;
        Value ->
            NoSpaces = binary:replace(Value, <<" ">>, <<>>, ['global']),
            kz_json:set_value(Key, NoSpaces, Acc)
    end.

-spec cleanup_leaky_keys(api_binary(), cb_context:context()) -> cb_context:context().
cleanup_leaky_keys(_AccountId, Context) ->
    RemoveKeys = [<<"wnm_allow_additions">>
                 ,<<"superduper_admin">>
                 ,<<"billing_mode">>
                 ],
    ReqData = kz_json:delete_keys(RemoveKeys, cb_context:req_data(Context)),
    cb_context:set_req_data(Context, ReqData).

-spec validate_realm_is_unique(api_binary(), cb_context:context()) -> cb_context:context().
validate_realm_is_unique(AccountId, Context) ->
    Realm = kz_account:realm(cb_context:req_data(Context)),
    case is_unique_realm(AccountId, Realm) of
        'true' -> Context;
        'false' ->
            cb_context:add_validation_error(
              [<<"realm">>]
                                           ,<<"unique">>
                                           ,kz_json:from_list(
                                              [{<<"message">>, <<"Account realm already in use">>}
                                              ,{<<"cause">>, Realm}
                                              ])
                                           ,Context
             )
    end.

-spec validate_account_name_is_unique(api_binary(), cb_context:context()) -> cb_context:context().
validate_account_name_is_unique(AccountId, Context) ->
    Name = kz_account:name(cb_context:req_data(Context)),
    case maybe_is_unique_account_name(AccountId, Name) of
        'true' -> Context;
        'false' ->
            cb_context:add_validation_error(
              [<<"name">>]
                                           ,<<"unique">>
                                           ,kz_json:from_list(
                                              [{<<"message">>, <<"Account name already in use">>}
                                              ,{<<"cause">>, Name}
                                              ])
                                           ,Context
             )
    end.

-spec validate_account_schema(api_binary(), cb_context:context()) -> cb_context:context().
validate_account_schema(AccountId, Context) ->
    OnSuccess = fun(C) -> on_successful_validation(AccountId, C) end,
    cb_context:validate_request_data(<<"accounts">>, Context, OnSuccess).

-spec on_successful_validation(api_binary(), cb_context:context()) -> cb_context:context().
on_successful_validation('undefined', Context) ->
    set_private_properties(Context);
on_successful_validation(AccountId, Context) ->
    Context1 = crossbar_doc:load_merge(AccountId, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE)),
    maybe_import_enabled(Context1).

-spec maybe_import_enabled(cb_context:context()) ->
                                  cb_context:context().
-spec maybe_import_enabled(cb_context:context(), crossbar_status()) ->
                                  cb_context:context().
-spec maybe_import_enabled(cb_context:context(), kz_json:object(), api_binary()) ->
                                  cb_context:context().
maybe_import_enabled(Context) ->
    case cb_context:auth_account_id(Context) =:= cb_context:account_id(Context) of
        'true' ->
            cb_context:set_doc(Context
                              ,kz_json:delete_key(<<"enabled">>, cb_context:doc(Context))
                              );
        'false' ->
            maybe_import_enabled(Context, cb_context:resp_status(Context))
    end.

maybe_import_enabled(Context, 'success') ->
    AuthId = cb_context:auth_account_id(Context),
    JObj = cb_context:doc(Context),
    case lists:member(AuthId, kz_account:tree(JObj)) of
        'false' ->
            cb_context:set_doc(Context, kz_json:delete_key(<<"enabled">>, JObj));
        'true' ->
            maybe_import_enabled(Context, JObj, kz_json:get_value(<<"enabled">>, JObj))
    end.

maybe_import_enabled(Context, _JObj, 'undefined') -> Context;
maybe_import_enabled(Context, JObj, IsEnabled) ->
    JObj1 =
        case kz_term:is_true(IsEnabled) of
            'true' -> kz_account:enable(JObj);
            'false' -> kz_account:disable(JObj)
        end,
    cb_context:set_doc(Context
                      ,kz_json:delete_key(<<"enabled">>, JObj1)
                      ).

-spec disallow_direct_clients(api_binary(), cb_context:context()) -> cb_context:context().
disallow_direct_clients(AccountId, Context) ->
    AllowDirect = kapps_config:get_is_true(?KZ_ACCOUNTS_DB, 'allow_subaccounts_for_direct', 'true'),
    maybe_disallow_direct_clients(AccountId, Context, AllowDirect).

-spec maybe_disallow_direct_clients(api_binary(), cb_context:context(), boolean()) ->
                                           cb_context:context().
maybe_disallow_direct_clients(_AccountId, Context, 'true') ->
    Context;
maybe_disallow_direct_clients(_AccountId, Context, 'false') ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    AuthAccountId = cb_context:auth_account_id(Context),
    AuthUserReseller = kz_services:get_reseller_id(AuthAccountId),
    case AuthUserReseller =/= MasterAccountId
        orelse kz_services:is_reseller(AuthAccountId)
    of
        'true' -> Context;
        'false' ->
            lager:debug("direct account ~p is disallowed from creating sub-accounts", [AuthAccountId]),
            cb_context:add_validation_error(
              [<<"account">>]
                                           ,<<"forbidden">>
                                           ,kz_json:from_list(
                                              [{<<"message">>, <<"Direct account is not allowed to create sub-accounts">>}
                                              ,{<<"cause">>, AuthAccountId}
                                              ])
                                           ,Context
             )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an account document from the database
%% @end
%%--------------------------------------------------------------------
-spec validate_delete_request(ne_binary(), cb_context:context()) -> cb_context:context().
validate_delete_request(AccountId, Context) ->
    case kapps_util:account_has_descendants(AccountId) of
        'true' ->  cb_context:add_system_error('account_has_descendants', Context);
        'false' ->
            case knm_port_request:account_has_active_port(AccountId) of
                'false' -> cb_context:set_resp_status(Context, 'success');
                'true' ->
                    lager:debug("pervent deleting account ~s due to has active port request", [AccountId]),
                    cb_context:add_system_error('account_has_active_port'
                                               ,kz_json:from_list([{<<"message">>
                                                                   ,<<"Account has active port request">>}])
                                               ,Context)
            end
    end.

%% @private
-spec validate_patch_request(ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch_request(AccountId, Context) ->
    crossbar_doc:patch_and_validate(AccountId, Context, fun validate_request/2).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load an account document from the database
%% @end
%%--------------------------------------------------------------------
-spec load_account(ne_binary(), cb_context:context()) -> cb_context:context().
load_account(AccountId, Context) ->
    leak_pvt_fields(AccountId, crossbar_doc:load(AccountId, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE))).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec leak_pvt_fields(api_binary(), cb_context:context()) -> cb_context:context().
-spec leak_pvt_fields(api_binary(), cb_context:context(), crossbar_status()) -> cb_context:context().
leak_pvt_fields(AccountId, Context) ->
    leak_pvt_fields(AccountId, Context, cb_context:resp_status(Context)).

leak_pvt_fields(AccountId, Context, 'success') ->
    Routines = [fun leak_pvt_allow_additions/1
               ,fun leak_pvt_superduper_admin/1
               ,fun leak_pvt_api_key/1
               ,fun leak_pvt_created/1
               ,fun leak_pvt_enabled/1
               ,{fun leak_reseller_id/2, AccountId}
               ,fun leak_is_reseller/1
               ,{fun leak_billing_mode/2, AccountId}
               ,fun leak_notification_preference/1
               ,fun leak_trial_time_left/1
               ],
    cb_context:setters(Context, Routines);
leak_pvt_fields(_AccountId, Context, _Status) -> Context.

-spec leak_pvt_allow_additions(cb_context:context()) -> cb_context:context().
leak_pvt_allow_additions(Context) ->
    cb_context:set_resp_data(Context
                            ,kz_json:set_value(<<"wnm_allow_additions">>
                                              ,kz_account:allow_number_additions(cb_context:doc(Context))
                                              ,cb_context:resp_data(Context)
                                              )
                            ).

-spec leak_pvt_superduper_admin(cb_context:context()) -> cb_context:context().
leak_pvt_superduper_admin(Context) ->
    cb_context:set_resp_data(Context
                            ,kz_json:set_value(<<"superduper_admin">>
                                              ,kz_account:is_superduper_admin(cb_context:doc(Context))
                                              ,cb_context:resp_data(Context)
                                              )
                            ).

-spec leak_pvt_api_key(cb_context:context()) -> cb_context:context().
leak_pvt_api_key(Context) ->
    case kz_term:is_true(cb_context:req_value(Context, <<"include_api_key">>, 'false'))
        orelse kapps_config:get_is_true(?ACCOUNTS_CONFIG_CAT, <<"expose_api_key">>, 'false')
    of
        'false' -> Context;
        'true' ->
            cb_context:set_resp_data(Context
                                    ,kz_json:set_value(<<"api_key">>
                                                      ,kz_account:api_key(cb_context:doc(Context))
                                                      ,cb_context:resp_data(Context)
                                                      )
                                    )
    end.

-spec leak_pvt_created(cb_context:context()) -> cb_context:context().
leak_pvt_created(Context) ->
    cb_context:set_resp_data(Context
                            ,kz_json:set_value(<<"created">>
                                              ,kz_doc:created(cb_context:doc(Context))
                                              ,cb_context:resp_data(Context)
                                              )
                            ).

-spec leak_pvt_enabled(cb_context:context()) -> cb_context:context().
leak_pvt_enabled(Context) ->
    RespJObj = cb_context:resp_data(Context),
    case kz_account:is_enabled(cb_context:doc(Context)) of
        'true' ->
            cb_context:set_resp_data(Context
                                    ,kz_json:set_value(<<"enabled">>, 'true', RespJObj)
                                    );
        'false' ->
            cb_context:set_resp_data(Context
                                    ,kz_json:set_value(<<"enabled">>, 'false', RespJObj)
                                    )
    end.

-spec leak_reseller_id(cb_context:context(), api_binary()) -> cb_context:context().
leak_reseller_id(Context, PathAccountId) ->
    cb_context:set_resp_data(Context
                            ,kz_json:set_value(<<"reseller_id">>
                                              ,find_reseller_id(Context, PathAccountId)
                                              ,cb_context:resp_data(Context)
                                              )
                            ).

-spec leak_is_reseller(cb_context:context()) -> cb_context:context().
leak_is_reseller(Context) ->
    IsReseller = kz_services:is_reseller(cb_context:account_id(Context)),
    cb_context:set_resp_data(Context
                            ,kz_json:set_value(<<"is_reseller">>
                                              ,IsReseller
                                              ,cb_context:resp_data(Context)
                                              )
                            ).

-spec leak_billing_mode(cb_context:context(), api_binary()) -> cb_context:context().
leak_billing_mode(Context, PathAccountId) ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    AuthAccountId = cb_context:auth_account_id(Context),
    RespJObj = cb_context:resp_data(Context),
    case find_reseller_id(Context, PathAccountId) of
        AuthAccountId ->
            cb_context:set_resp_data(Context
                                    ,kz_json:set_value(<<"billing_mode">>, <<"limits_only">>, RespJObj)
                                    );
        MasterAccountId ->
            cb_context:set_resp_data(Context
                                    ,kz_json:set_value(<<"billing_mode">>, <<"normal">>, RespJObj)
                                    );
        _AccountId ->
            cb_context:set_resp_data(Context
                                    ,kz_json:set_value(<<"billing_mode">>, <<"manual">>, RespJObj)
                                    )
    end.

-spec find_reseller_id(cb_context:context(), api_binary()) -> api_binary().
find_reseller_id(Context, 'undefined') ->
    %% only when put/1
    cb_context:reseller_id(Context);
find_reseller_id(Context, PathAccountId) ->
    IsNotSelf = PathAccountId =/= cb_context:account_id(Context),
    case kz_services:is_reseller(PathAccountId) of
        'true' when IsNotSelf -> PathAccountId;
        'true' -> cb_context:reseller_id(Context);
        'false' -> cb_context:reseller_id(Context)
    end.

-spec leak_notification_preference(cb_context:context()) -> cb_context:context().
-spec leak_notification_preference(cb_context:context(), api_binary()) -> cb_context:context().
leak_notification_preference(Context) ->
    leak_notification_preference(Context, kz_account:notification_preference(cb_context:doc(Context))).

leak_notification_preference(Context, 'undefined') ->
    Context;
leak_notification_preference(Context, Pref) ->
    UpdatedRespJObj = kz_json:set_value(<<"notification_preference">>, Pref, cb_context:resp_data(Context)),
    cb_context:set_resp_data(Context, UpdatedRespJObj).

-spec leak_trial_time_left(cb_context:context()) ->
                                  cb_context:context().
-spec leak_trial_time_left(cb_context:context(), kz_json:object(), api_integer()) ->
                                  cb_context:context().
leak_trial_time_left(Context) ->
    JObj = cb_context:doc(Context),
    leak_trial_time_left(Context, JObj, kz_account:trial_expiration(JObj)).

leak_trial_time_left(Context, _JObj, 'undefined') ->
    RespData = kz_json:delete_key(<<"trial_time_left">>
                                 ,cb_context:resp_data(Context)
                                 ),
    cb_context:set_resp_data(Context, RespData);
leak_trial_time_left(Context, JObj, _Expiration) ->
    RespData = kz_json:set_value(<<"trial_time_left">>
                                ,kz_account:trial_time_left(JObj)
                                ,cb_context:resp_data(Context)
                                ),
    cb_context:set_resp_data(Context, RespData).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the children of this account
%% @end
%%--------------------------------------------------------------------
-spec load_children(ne_binary(), cb_context:context()) -> cb_context:context().
load_children(AccountId, Context) ->
    load_children(AccountId, Context, cb_context:api_version(Context)).

-spec load_children(ne_binary(), cb_context:context(), ne_binary()) -> cb_context:context().
load_children(AccountId, Context, ?VERSION_1) ->
    load_children_v1(AccountId, Context);
load_children(AccountId, Context, _Version) ->
    load_paginated_children(AccountId, Context).

-spec load_children_v1(ne_binary(), cb_context:context()) -> cb_context:context().
load_children_v1(AccountId, Context) ->
    crossbar_doc:load_view(?AGG_VIEW_CHILDREN
                          ,[{'startkey', [AccountId]}
                           ,{'endkey', [AccountId, kz_json:new()]}
                           ]
                          ,Context
                          ,fun normalize_view_results/2
                          ).

-spec load_paginated_children(ne_binary(), cb_context:context()) -> cb_context:context().
load_paginated_children(AccountId, Context) ->
    StartKey = start_key(Context),
    fix_envelope(
      crossbar_doc:load_view(?AGG_VIEW_CHILDREN
                            ,[{'startkey', [AccountId, StartKey]}
                             ,{'endkey', [AccountId, kz_json:new()]}
                             ]
                            ,Context
                            ,fun normalize_view_results/2
                            )).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the descendants of this account
%% @end
%%--------------------------------------------------------------------
-spec load_descendants(ne_binary(), cb_context:context()) -> cb_context:context().
load_descendants(AccountId, Context) ->
    load_descendants(AccountId, Context, cb_context:api_version(Context)).

load_descendants(AccountId, Context, ?VERSION_1) ->
    load_descendants_v1(AccountId, Context);
load_descendants(AccountId, Context, _Version) ->
    load_paginated_descendants(AccountId, Context).

-spec load_descendants_v1(ne_binary(), cb_context:context()) -> cb_context:context().
load_descendants_v1(AccountId, Context) ->
    crossbar_doc:load_view(?AGG_VIEW_DESCENDANTS
                          ,[{'startkey', [AccountId]}
                           ,{'endkey', [AccountId, kz_json:new()]}
                           ]
                          ,Context
                          ,fun normalize_view_results/2
                          ).

-spec load_paginated_descendants(ne_binary(), cb_context:context()) -> cb_context:context().
load_paginated_descendants(AccountId, Context) ->
    StartKey = start_key(Context),
    lager:debug("account ~s startkey ~s", [AccountId, StartKey]),
    fix_envelope(
      crossbar_doc:load_view(?AGG_VIEW_DESCENDANTS
                            ,[{'startkey', [AccountId, StartKey]}
                             ,{'endkey',  [AccountId, kz_json:new()]}
                             ]
                            ,Context
                            ,fun normalize_view_results/2
                            )
     ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Load a summary of the siblngs of this account
%% @end
%%--------------------------------------------------------------------
-spec load_siblings(ne_binary(), cb_context:context()) -> cb_context:context().
load_siblings(AccountId, Context) ->
    case kz_util:is_system_admin(cb_context:auth_account_id(Context))
        orelse
        (AccountId =/= cb_context:auth_account_id(Context)
         andalso kapps_config:get_is_true(?ACCOUNTS_CONFIG_CAT, <<"allow_sibling_listing">>, 'true')
        )
    of
        'true' -> load_siblings(AccountId, Context, cb_context:api_version(Context));
        'false' -> cb_context:add_system_error('forbidden', Context)
    end.

-spec load_siblings(ne_binary(), cb_context:context(), ne_binary()) -> cb_context:context().
load_siblings(AccountId, Context, ?VERSION_1) ->
    load_siblings_v1(AccountId, Context);
load_siblings(AccountId, Context, _Version) ->
    load_paginated_siblings(AccountId, Context).

-spec load_siblings_v1(ne_binary(), cb_context:context()) -> cb_context:context().
load_siblings_v1(AccountId, Context) ->
    Context1 = crossbar_doc:load_view(?AGG_VIEW_PARENT
                                     ,[{'startkey', AccountId}
                                      ,{'endkey', AccountId}
                                      ]
                                     ,Context
                                     ),
    case cb_context:resp_status(Context1) of
        'success' ->
            load_siblings_results(AccountId, Context1, cb_context:doc(Context1));
        _Status ->
            cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, AccountId}]), Context)
    end.

-spec load_paginated_siblings(ne_binary(), cb_context:context()) -> cb_context:context().
load_paginated_siblings(AccountId, Context) ->
    Context1 =
        fix_envelope(
          crossbar_doc:load_view(?AGG_VIEW_PARENT
                                ,[{'startkey', AccountId}
                                 ,{'endkey', AccountId}
                                 ]
                                ,Context
                                )),
    case cb_context:resp_status(Context1) of
        'success' ->
            load_siblings_results(AccountId, Context1, cb_context:doc(Context1));
        _Status ->
            cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, AccountId}]),  Context)
    end.

-spec load_siblings_results(ne_binary(), cb_context:context(), kz_json:objects()) -> cb_context:context().
load_siblings_results(_AccountId, Context, [JObj|_]) ->
    Parent = kz_json:get_value([<<"value">>, <<"id">>], JObj),
    load_children(Parent, Context);
load_siblings_results(AccountId, Context, _) ->
    cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, AccountId}]),  Context).

-spec start_key(cb_context:context()) -> binary().
start_key(Context) ->
    case crossbar_doc:start_key(Context) of
        'undefined' -> <<>>;
        Key -> Key
    end.

-spec fix_envelope(cb_context:context()) -> cb_context:context().
fix_envelope(Context) ->
    cb_context:set_resp_envelope(
      cb_context:set_resp_data(Context, lists:reverse(cb_context:resp_data(Context)))
                                ,lists:foldl(
                                   fun fix_envelope_fold/2
                                            ,cb_context:resp_envelope(Context)
                                            ,[<<"start_key">>, <<"next_start_key">>]
                                  )
     ).

-spec fix_envelope_fold(binary(), kz_json:object()) -> kz_json:object().
fix_envelope_fold(Key, JObj) ->
    case fix_start_key(kz_json:get_value(Key, JObj)) of
        'undefined' -> kz_json:delete_key(Key, JObj);
        V -> kz_json:set_value(Key, V, JObj)
    end.

-spec fix_start_key(api_binary() | list()) -> api_binary().
fix_start_key('undefined') -> 'undefined';
fix_start_key(<<_/binary>> = StartKey) -> StartKey;
fix_start_key([StartKey]) -> StartKey;
fix_start_key([_AccountId, [_|_]=Keys]) -> lists:last(Keys);
fix_start_key([_AccountId, StartKey]) -> StartKey;
fix_start_key([StartKey|_T]) -> StartKey.

-spec load_account_tree(cb_context:context()) -> cb_context:context().
load_account_tree(Context) ->
    Tree = get_authorized_account_tree(Context),
    case kz_datamgr:open_cache_docs(?KZ_ACCOUNTS_DB, Tree) of
        {'error', R} -> crossbar_doc:handle_datamgr_errors(R, ?KZ_ACCOUNTS_DB, Context);
        {'ok', JObjs} -> format_account_tree_results(Context, JObjs)
    end.

-spec get_authorized_account_tree(cb_context:context()) -> ne_binaries().
get_authorized_account_tree(Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    lists:dropwhile(fun(E) -> E =/= AuthAccountId end
                   ,kz_account:tree(cb_context:doc(Context))
                   ).

-spec format_account_tree_results(cb_context:context(), kz_json:objects()) -> cb_context:context().
format_account_tree_results(Context, JObjs) ->
    RespData =
        [kz_json:from_list(
           [{<<"id">>, kz_doc:id(JObj)}
           ,{<<"name">>, kz_json:get_value([<<"doc">>, <<"name">>], JObj)}
           ])
         || JObj <- JObjs
        ],
    cb_context:set_resp_data(Context, RespData).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec load_parents(ne_binary(), cb_context:context()) -> cb_context:context().
load_parents(AccountId, Context) ->
    Context1 = crossbar_doc:load_view(?AGG_VIEW_SUMMARY
                                     ,[]
                                     ,cb_context:set_account_db(Context, ?KZ_ACCOUNTS_DB)
                                     ),
    case cb_context:resp_status(Context1) of
        'success' -> load_parent_tree(AccountId, Context1);
        _Status -> Context1
    end.

-spec load_parent_tree(ne_binary(), cb_context:context()) -> cb_context:context().
load_parent_tree(AccountId, Context) ->
    RespData = cb_context:resp_data(Context),
    Tree = extract_tree(AccountId, RespData),
    Parents = find_accounts_from_tree(Tree, RespData, Context),
    RespEnv =
        kz_json:set_value(<<"page_size">>
                         ,length(Parents)
                         ,cb_context:resp_envelope(Context)
                         ),
    cb_context:setters(Context
                      ,[{fun cb_context:set_resp_data/2, Parents}
                       ,{fun cb_context:set_resp_envelope/2, RespEnv}
                       ]
                      ).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec extract_tree(ne_binary(), kz_json:objects()) -> ne_binaries().
extract_tree(AccountId, JObjs) ->
    JObj = kz_json:find_value(<<"id">>, AccountId, JObjs),
    [_, Tree] = kz_json:get_value(<<"key">>, JObj),
    lists:delete(AccountId, Tree).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec find_accounts_from_tree(ne_binaries(), kz_json:objects(), cb_context:context()) -> kz_json:objects().
-spec find_accounts_from_tree(ne_binaries(), kz_json:objects(), ne_binary(), kz_json:objects()) -> kz_json:objects().
find_accounts_from_tree(Tree, JObjs, Context) ->
    find_accounts_from_tree(
      lists:reverse(Tree)
                           ,JObjs
                           ,cb_context:auth_account_id(Context)
                           ,[]
     ).

find_accounts_from_tree([], _, _, Acc) -> Acc;
find_accounts_from_tree([AuthAccountId|_], JObjs, AuthAccountId, Acc) ->
    JObj = kz_json:find_value(<<"id">>, AuthAccountId, JObjs),
    Value = kz_json:get_value(<<"value">>, JObj),
    [account_from_tree(Value)|Acc];
find_accounts_from_tree([AccountId|Tree], JObjs, AuthAccountId, Acc) ->
    JObj = kz_json:find_value(<<"id">>, AccountId, JObjs),
    Value = kz_json:get_value(<<"value">>, JObj),
    find_accounts_from_tree(
      Tree
                           ,JObjs
                           ,AuthAccountId
                           ,[account_from_tree(Value)|Acc]
     ).

-spec account_from_tree(kz_json:object()) -> kz_json:object().
account_from_tree(JObj) ->
    kz_json:from_list([{<<"id">>, kz_doc:id(JObj)}
                      ,{<<"name">>, kz_account:name(JObj)}
                      ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Normalizes the resuts of a view
%% @end
%%--------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

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
    cb_context:set_doc(Context, kz_doc:set_type(cb_context:doc(Context), ?PVT_TYPE)).

-spec add_pvt_vsn(cb_context:context()) -> cb_context:context().
add_pvt_vsn(Context) ->
    cb_context:set_doc(Context, kz_doc:set_vsn(cb_context:doc(Context), <<"1">>)).

-spec add_pvt_enabled(cb_context:context()) -> cb_context:context().
add_pvt_enabled(Context) ->
    JObj = cb_context:doc(Context),
    case lists:reverse(kz_account:tree(JObj)) of
        [ParentId | _] ->
            ParentDb = kz_util:format_account_id(ParentId, 'encoded'),
            case (not kz_term:is_empty(ParentId))
                andalso kz_datamgr:open_doc(ParentDb, ParentId)
            of
                {'ok', Parent} ->
                    case kz_account:is_enabled(Parent) of
                        'true'  -> cb_context:set_doc(Context, kz_account:enable(JObj));
                        'false' -> cb_context:set_doc(Context, kz_account:disable(JObj))
                    end;
                _Else -> Context
            end;
        [] ->
            Context
    end.

-spec maybe_add_pvt_api_key(cb_context:context()) -> cb_context:context().
maybe_add_pvt_api_key(Context) ->
    JObj = cb_context:doc(Context),
    case kz_account:api_key(JObj) of
        'undefined' ->
            APIKey = kz_term:to_hex_binary(crypto:strong_rand_bytes(32)),
            cb_context:set_doc(Context, kz_account:set_api_key(JObj, APIKey));
        _Else -> Context
    end.

-spec maybe_add_pvt_tree(cb_context:context()) -> cb_context:context().
maybe_add_pvt_tree(Context) ->
    case kz_account:tree(cb_context:doc(Context)) of
        [_|_] -> Context;
        _Else -> add_pvt_tree(Context)
    end.

-spec add_pvt_tree(cb_context:context()) -> cb_context:context().
add_pvt_tree(Context) ->
    case create_new_tree(Context) of
        'error' -> cb_context:add_system_error('empty_tree_accounts_exist', Context);
        Tree -> cb_context:set_doc(Context, kz_account:set_tree(cb_context:doc(Context), Tree))
    end.

-spec create_new_tree(cb_context:context() | api_binary()) -> ne_binaries() | 'error'.
create_new_tree('undefined') ->
    case kapps_util:get_master_account_id() of
        {'ok', MasterAccountId} -> [MasterAccountId];
        {'error', _} ->
            case kapps_util:get_all_accounts() of
                [] -> [];
                _Else -> 'error'
            end
    end;
create_new_tree(Parent) when is_binary(Parent) ->
    ParentId = kz_util:format_account_id(Parent, 'raw'),
    ParentDb = kz_util:format_account_id(Parent, 'encoded'),
    case kz_datamgr:open_doc(ParentDb, ParentId) of
        {'error', _} -> create_new_tree('undefined');
        {'ok', JObj} ->
            kz_account:tree(JObj) ++ [ParentId]
    end;
create_new_tree(Context) ->
    create_new_tree(Context, cb_context:req_verb(Context), cb_context:req_nouns(Context)).

create_new_tree(_Context, ?HTTP_PUT, [{?KZ_ACCOUNTS_DB, [Parent]}]) ->
    create_new_tree(Parent);
create_new_tree(Context, _Verb, _Nouns) ->
    JObj = cb_context:auth_doc(Context),
    case kz_json:is_json_object(JObj) of
        'false' -> create_new_tree('undefined');
        'true' -> create_new_tree(kz_json:get_value(<<"account_id">>, JObj))
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will attempt to load the context with the db name of
%% for this account
%% @end
%%--------------------------------------------------------------------
-spec load_account_db(cb_context:context(), ne_binary() | ne_binaries()) ->
                             cb_context:context().
load_account_db(Context, [AccountId|_]) ->
    load_account_db(Context, AccountId);
load_account_db(Context, AccountId) when is_binary(AccountId) ->
    case kz_account:fetch(AccountId) of
        {'ok', JObj} ->
            AccountDb = kz_util:format_account_id(AccountId, 'encoded'),
            lager:debug("account ~s db exists, setting operating database as ~s", [AccountId, AccountDb]),
            ResellerId = kz_services:find_reseller_id(AccountId),
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_status/2, 'success'}
                               ,{fun cb_context:set_account_db/2, AccountDb}
                               ,{fun cb_context:set_account_id/2, AccountId}
                               ,{fun cb_context:set_account_name/2, kz_account:name(JObj)}
                               ,{fun cb_context:set_reseller_id/2, ResellerId}
                               ]);
        {'error', 'not_found'} ->
            Msg = kz_json:from_list([{<<"cause">>, AccountId}]),
            cb_context:add_system_error('bad_identifier', Msg, Context);
        {'error', _R} ->
            crossbar_util:response_db_fatal(Context)
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
    case kapps_util:is_account_db(AccountDb)
        andalso kz_datamgr:db_create(AccountDb)
    of
        'false' ->
            lager:debug("failed to create database: ~s", [AccountDb]),
            throw(cb_context:add_system_error('datastore_fault', Context));
        'true' ->
            lager:debug("created account database: ~s", [AccountDb]),
            C = create_account_definition(prepare_context(AccountDb, Context)),
            lager:debug("created account definition"),

            _ = load_initial_views(C),
            lager:debug("laoded initial views"),

            _ = crossbar_bindings:map(<<"account.created">>, C),
            lager:debug("alerted listeners of new account"),

            _ = create_account_mod(cb_context:account_id(C)),
            lager:debug("created this month's MODb for account"),

            _ = kz_services:reconcile(AccountDb),
            lager:debug("performed initial services reconcile"),

            _ = create_first_transaction(cb_context:account_id(C)),
            lager:debug("created first transaction for account"),

            _ = maybe_set_notification_preference(C),
            lager:debug("set notification preference"),

            %% Give onboarding tools time to add initial users...
            Delay = kapps_config:get_integer(?ACCOUNTS_CONFIG_CAT, <<"new_account_notify_delay_s">>, 30),
            _ = timer:apply_after(Delay * ?MILLISECONDS_IN_SECOND, ?MODULE, 'notify_new_account', [C]),
            lager:debug("started ~ps timer for new account notification", [Delay]),
            C
    end.

-spec maybe_set_notification_preference(cb_context:context()) -> 'ok'.
maybe_set_notification_preference(Context) ->
    AccountId = cb_context:account_id(Context),
    ResellerId = kz_services:find_reseller_id(AccountId),
    case kz_account:fetch(ResellerId) of
        {'error', _E} ->
            lager:error("failed to open reseller '~s': ~p", [ResellerId, _E]);
        {'ok', AccountJObj} ->
            case kz_account:notification_preference(AccountJObj) of
                'undefined' ->
                    lager:debug("notification preference not set on reseller '~s'", [ResellerId]);
                Preference ->
                    set_notification_preference(Context, Preference)
            end
    end.

-spec set_notification_preference(cb_context:context(), ne_binary()) -> 'ok'.
set_notification_preference(Context, Preference) ->
    AccountDb = cb_context:account_db(Context),
    AccountDefinition = kz_account:set_notification_preference(cb_context:doc(Context), Preference),
    case kz_datamgr:save_doc(AccountDb, AccountDefinition) of
        {'error', _R} ->
            lager:error("failed to update account definition: ~p", [_R]);
        {'ok', AccountDef} ->
            _ = replicate_account_definition(AccountDef),
            lager:info("notification_preference set to '~s'", [Preference])
    end.

-spec create_account_mod(ne_binary()) -> any().
create_account_mod(AccountId) ->
    Db = kz_util:format_account_mod_id(AccountId),
    kazoo_modb:create(Db).

-spec create_first_transaction(ne_binary()) -> any().
create_first_transaction(AccountId) ->
    AccountMODb = kazoo_modb:get_modb(AccountId),
    wht_util:rollup(AccountMODb, 0).

-spec ensure_accounts_db_exists() -> 'ok'.
ensure_accounts_db_exists() ->
    case kz_datamgr:db_exists(?KZ_ACCOUNTS_DB) of
        'true' -> 'ok';
        'false' ->
            _ = kapps_maintenance:refresh(?KZ_ACCOUNTS_DB),
            'ok'
    end.

-spec create_account_definition(cb_context:context()) -> cb_context:context().
create_account_definition(Context) ->
    AccountId = cb_context:account_id(Context),
    AccountDb = cb_context:account_db(Context),

    TStamp = kz_time:current_tstamp(),
    Props = [{<<"_id">>, AccountId}
            ,{<<"pvt_account_id">>, AccountId}
            ,{<<"pvt_account_db">>, AccountDb}
            ,{<<"pvt_modified">>, TStamp}
            ,{<<"pvt_created">>, TStamp}
            ,{<<"pvt_vsn">>, <<"1">>}
            ],

    JObj = maybe_set_trial_expires(kz_json:set_values(Props, cb_context:doc(Context))),

    case kz_datamgr:save_doc(AccountDb, JObj) of
        {'ok', AccountDef}->
            _ = replicate_account_definition(AccountDef),
            cb_context:setters(Context
                              ,[{fun cb_context:set_doc/2, AccountDef}
                               ,{fun cb_context:set_resp_data/2, kz_doc:public_fields(AccountDef)}
                               ,{fun cb_context:set_resp_status/2, 'success'}
                               ]);
        {'error', _R} ->
            lager:debug("unable to create account definition: ~p", [_R]),
            throw(cb_context:add_system_error('datastore_fault', Context))
    end.

-spec maybe_set_trial_expires(kz_json:object()) -> kz_json:object().
maybe_set_trial_expires(JObj) ->
    case kz_account:is_trial_account(JObj) of
        'false' -> JObj;
        'true' -> set_trial_expires(JObj)
    end.

-spec set_trial_expires(kz_json:object()) -> kz_json:object().
set_trial_expires(JObj) ->
    TrialTime = kapps_config:get_integer(?ACCOUNTS_CONFIG_CAT, <<"trial_time">>, ?SECONDS_IN_DAY * 14),
    Expires = kz_time:current_tstamp() + TrialTime,
    kz_account:set_trial_expiration(JObj, Expires).


-spec load_initial_views(cb_context:context()) -> 'ok'.
load_initial_views(Context)->
    [{FirstId, _}|_] = Views = kapps_maintenance:get_all_account_views(),
    {LastId, _} = lists:last(Views),
    kapps_util:update_views(cb_context:account_db(Context), Views, 'true'),
    _ = kazoo_number_manager_maintenance:update_number_services_view(cb_context:account_db(Context)),
    ensure_views(Context, [FirstId, LastId]).

-spec ensure_views(cb_context:context(), ne_binaries()) -> 'ok'.
-spec ensure_views(cb_context:context(), ne_binaries(), 0..3) -> 'ok'.
ensure_views(Context, Ids) ->
    ensure_views(Context, Ids, 3).

ensure_views(_Context, [], _Retries) -> 'ok';
ensure_views(_Context, [_Id|_], 0) ->
    lager:debug("failed to find design doc ~s in ~s", [_Id, cb_context:account_db(_Context)]);
ensure_views(Context, [Id|Ids], Retries) ->
    AccountDb = cb_context:account_db(Context),
    case kz_datamgr:open_doc(AccountDb, Id) of
        {'ok', _} -> ensure_views(Context, Ids, 3);
        {'error', 'not_found'} ->
            timer:sleep(500),
            ensure_views(Context, [Id|Ids], Retries-1);
        {'error', _E} ->
            lager:debug("failed to open design doc ~s in ~s: ~p", [Id, AccountDb, _E]),
            load_initial_views(Context)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec replicate_account_definition(kz_json:object()) ->
                                          {'ok', kz_json:object()} |
                                          {'error', any()}.
replicate_account_definition(JObj) ->
    AccountId = kz_doc:id(JObj),
    case kz_datamgr:lookup_doc_rev(?KZ_ACCOUNTS_DB, AccountId) of
        {'ok', Rev} ->
            kz_datamgr:ensure_saved(?KZ_ACCOUNTS_DB, kz_doc:set_revision(JObj, Rev));
        _Else ->
            kz_datamgr:ensure_saved(?KZ_ACCOUNTS_DB, kz_doc:delete_revision(JObj))
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
    ViewOptions = [{'key', kz_term:to_lower_binary(Realm)}],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ?AGG_VIEW_REALM, ViewOptions) of
        {'ok', []} -> 'true';
        {'ok', [JObj]} -> kz_doc:id(JObj) =:= AccountId;
        {'error', 'not_found'} -> 'true';
        _Else -> 'false'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function will determine if the account name is unique
%% @end
%%--------------------------------------------------------------------
-spec maybe_is_unique_account_name(api_binary(), ne_binary()) -> boolean().
maybe_is_unique_account_name(AccountId, Name) ->
    case kapps_config:get_is_true(?ACCOUNTS_CONFIG_CAT, <<"ensure_unique_name">>, 'true') of
        'true' -> is_unique_account_name(AccountId, Name);
        'false' -> 'true'
    end.

-spec is_unique_account_name(api_binary(), ne_binary()) -> boolean().
is_unique_account_name(AccountId, Name) ->
    AccountName = kz_util:normalize_account_name(Name),
    ViewOptions = [{'key', AccountName}],
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, ?AGG_VIEW_NAME, ViewOptions) of
        {'ok', []} -> 'true';
        {'error', 'not_found'} -> 'true';
        {'ok', [JObj|_]} -> kz_doc:id(JObj) =:= AccountId;
        _Else ->
            lager:error("error ~p checking view ~p in ~p", [_Else, ?AGG_VIEW_NAME, ?KZ_ACCOUNTS_DB]),
            'false'
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
    cb_context:put_reqid(Context),
    JObj = cb_context:doc(Context),
    lager:debug("triggering new account notification for ~s", [cb_context:account_id(Context)]),
    Notify = [{<<"Account-Name">>, kz_account:name(JObj)}
             ,{<<"Account-Realm">>, kz_account:realm(JObj)}
             ,{<<"Account-API-Key">>, kz_account:api_key(JObj)}
             ,{<<"Account-ID">>, cb_context:account_id(Context)}
             ,{<<"Account-DB">>, cb_context:account_db(Context)}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kapi_notify_publisher:cast(Notify, fun kapi_notifications:publish_new_account/1).

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
    try kz_services:set_billing_id(BillingId, AccountId) of
        'undefined' -> Context;
        Services ->
            _ = kz_services:save(Services),
            Context
    catch
        'throw':{Error, Reason} ->
            cb_context:add_validation_error(<<"billing_id">>
                                           ,<<"not_found">>
                                           ,kz_json:from_list(
                                              [{<<"message">>, kz_term:to_binary(Error)}
                                              ,{<<"cause">>, AccountId}
                                              ])
                                           ,Reason
                                           )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_remove_services(cb_context:context()) -> cb_context:context() | boolean().
delete_remove_services(Context) ->
    case kz_services:delete(cb_context:account_id(Context)) of
        {'ok', _} -> delete_free_numbers(Context);
        _ -> crossbar_util:response('error', <<"unable to cancel services">>, 500, Context)
    end.

-spec delete_free_numbers(cb_context:context()) -> cb_context:context() | boolean().
delete_free_numbers(Context) ->
    _ = knm_numbers:free(cb_context:account_id(Context)),
    delete_remove_sip_aggregates(Context).

-spec delete_remove_sip_aggregates(cb_context:context()) -> cb_context:context() | boolean().
delete_remove_sip_aggregates(Context) ->
    ViewOptions = ['include_docs'
                  ,{'key', cb_context:account_id(Context)}
                  ],
    _ = case kz_datamgr:get_results(?KZ_SIP_DB, <<"credentials/lookup_by_account">>, ViewOptions) of
            {'error', _R} ->
                lager:debug("unable to clean sip_auth: ~p", [_R]);
            {'ok', JObjs} ->
                Docs = [kz_json:get_value(<<"doc">>, JObj) || JObj <- JObjs],
                kz_datamgr:del_docs(?KZ_SIP_DB, Docs)
        end,
    delete_remove_db(Context).

-spec delete_remove_db(cb_context:context()) -> cb_context:context() | boolean().
delete_remove_db(Context) ->
    Removed = case kz_datamgr:open_doc(cb_context:account_db(Context), cb_context:account_id(Context)) of
                  {'ok', _} ->
                      _ = provisioner_util:maybe_delete_account(Context),
                      _ = cb_mobile_manager:delete_account(Context),
                      _Deleted = kz_datamgr:db_delete(cb_context:account_db(Context)),
                      lager:info("deleting ~s: ~p", [cb_context:account_db(Context), _Deleted]),
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
-spec delete_mod_dbs(ne_binary(), kz_year(), kz_month()) -> 'true'.
delete_mod_dbs(Context) ->
    AccountId = cb_context:account_id(Context),
    {Year, Month, _} = erlang:date(),
    delete_mod_dbs(AccountId, Year, Month).

delete_mod_dbs(AccountId, Year, Month) ->
    Db = kz_util:format_account_mod_id(AccountId, Year, Month),
    case kz_datamgr:db_delete(Db) of
        'true' ->
            lager:debug("removed account mod: ~s", [Db]),
            {PrevYear, PrevMonth} = kazoo_modb_util:prev_year_month(Year, Month),
            delete_mod_dbs(AccountId, PrevYear, PrevMonth);
        'false' ->
            lager:debug("failed to delete account mod: ~s", [Db]),
            'true'
    end.

-spec delete_remove_from_accounts(cb_context:context()) -> cb_context:context().
delete_remove_from_accounts(Context) ->
    case kz_datamgr:open_doc(?KZ_ACCOUNTS_DB, cb_context:account_id(Context)) of
        {'ok', JObj} ->
            crossbar_doc:delete(
              cb_context:setters(Context
                                ,[{fun cb_context:set_account_db/2, ?KZ_ACCOUNTS_DB}
                                 ,{fun cb_context:set_doc/2, JObj}
                                 ])
             );
        {'error', 'not_found'} ->
            crossbar_util:response(kz_json:new(), Context);
        {'error', _R} ->
            crossbar_util:response('error', <<"unable to remove account definition">>, 500, Context)
    end.
