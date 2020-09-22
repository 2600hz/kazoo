%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Account module
%%% Handle client requests for account documents
%%%
%%%
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
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
        ]).

-export([delete_account/1]).

%% needed for API docs in cb_api_endpoints
-export([allowed_methods_on_account/2]).

-compile({no_auto_import,[put/2]}).

-include("crossbar.hrl").

-define(SERVER, ?MODULE).

-define(ACCOUNTS_CONFIG_CAT, <<(?CONFIG_CAT)/binary, ".accounts">>).

-define(AGG_VIEW_FILE, <<"views/accounts.json">>).
-define(AGG_VIEW_PARENT, <<"accounts/listing_by_parent">>).
-define(AGG_VIEW_CHILDREN, <<"accounts/listing_by_children">>).
-define(AGG_VIEW_DESCENDANTS, <<"accounts/listing_by_descendants">>).

-define(PVT_TYPE, kzd_accounts:type()).
-define(CHILDREN, <<"children">>).
-define(DESCENDANTS, <<"descendants">>).
-define(SIBLINGS, <<"siblings">>).
-define(API_KEY, <<"api_key">>).
-define(TREE, <<"tree">>).
-define(PARENTS, <<"parents">>).
-define(RESELLER, <<"reseller">>).

-define(MOVE, <<"move">>).

-define(ALLOW_DIRECT_CLIENTS
       ,kapps_config:get_is_true(?KZ_ACCOUNTS_DB, <<"allow_subaccounts_for_direct">>, 'true')
       ).

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

%%------------------------------------------------------------------------------
%% @doc This function determines the verbs that are appropriate for the
%% given Nouns. For example `/accounts/' can only accept `GET' and `PUT'.
%%
%% Failure here returns `405 Method Not Allowed'.
%% @end
%%------------------------------------------------------------------------------

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_PUT].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(AccountId) ->
    allowed_methods_on_account(AccountId, kapps_util:get_master_account_id()).

-spec allowed_methods_on_account(kz_term:ne_binary(), {'ok', kz_term:ne_binary()} | {'error', any()}) ->
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

-spec allowed_methods(path_token(), kz_term:ne_binary()) -> http_methods().
allowed_methods(_AccountId, ?MOVE) ->
    [?HTTP_POST];
allowed_methods(_AccountId, ?RESELLER) ->
    [?HTTP_PUT, ?HTTP_DELETE];
allowed_methods(_AccountId, ?CHILDREN) -> [?HTTP_GET];
allowed_methods(_AccountId, ?DESCENDANTS) -> [?HTTP_GET];
allowed_methods(_AccountId, ?SIBLINGS) -> [?HTTP_GET];
allowed_methods(_AccountId, ?API_KEY) -> [?HTTP_GET, ?HTTP_PUT];
allowed_methods(_AccountId, ?TREE) -> [?HTTP_GET];
allowed_methods(_AccountId, ?PARENTS) -> [?HTTP_GET].

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns are valid.
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec resource_exists(path_token(), kz_term:ne_binary()) -> boolean().
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

%%------------------------------------------------------------------------------
%% @doc This function determines if the provided list of Nouns and Resource Ids are valid.
%% If valid, updates Context with account data
%%
%% Failure here returns `404 Not Found'.
%% @end
%%------------------------------------------------------------------------------

-spec validate_resource(cb_context:context()) -> cb_context:context().
validate_resource(Context) ->
    Context.

-spec validate_resource(cb_context:context(), path_token()) -> cb_context:context().
validate_resource(Context, AccountId) ->
    load_account_db(Context, AccountId).

-spec validate_resource(cb_context:context(), path_token(), kz_term:ne_binary()) -> cb_context:context().
validate_resource(Context, AccountId, _Path) ->
    load_account_db(Context, AccountId).

%%------------------------------------------------------------------------------
%% @doc This function determines if the parameters and content are correct
%% for this request
%%
%% Failure here returns 400.
%% @end
%%------------------------------------------------------------------------------

-spec validate(cb_context:context()) ->
          cb_context:context().
validate(Context) ->
    validate_accounts(Context, cb_context:req_verb(Context)).

-spec validate_accounts(cb_context:context(), http_method()) -> cb_context:context().
validate_accounts(Context, ?HTTP_PUT) ->
    validate_request('undefined', prepare_context('undefined', Context)).

-spec validate(cb_context:context(), path_token()) ->
          cb_context:context().
validate(Context, AccountId) ->
    validate_account(Context, AccountId, cb_context:req_verb(Context)).

-spec validate_account(cb_context:context(), kz_term:ne_binary(), http_method()) -> cb_context:context().
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

-spec validate(cb_context:context(), path_token(), kz_term:ne_binary()) ->
          cb_context:context().
validate(Context, AccountId, PathToken) ->
    validate_account_path(Context, AccountId, PathToken, cb_context:req_verb(Context)).

-spec validate_account_path(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary(), http_method()) ->
          cb_context:context().
validate_account_path(Context, AccountId, ?CHILDREN, ?HTTP_GET) ->
    load_children(AccountId, prepare_context('undefined', Context));
validate_account_path(Context, AccountId, ?DESCENDANTS, ?HTTP_GET) ->
    load_descendants(AccountId, prepare_context('undefined', Context));
validate_account_path(Context, AccountId, ?SIBLINGS, ?HTTP_GET) ->
    load_siblings(AccountId, prepare_context('undefined', Context));
validate_account_path(Context, AccountId, ?PARENTS, ?HTTP_GET) ->
    load_account_tree(AccountId, prepare_context('undefined', Context));
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
            ApiKey = kzd_accounts:api_key(JObj),
            RespJObj = kz_json:from_list([{<<"api_key">>, ApiKey}]),
            cb_context:set_resp_data(Context1, RespJObj);
        _Else -> Context1
    end;
validate_account_path(Context, AccountId, ?API_KEY, ?HTTP_PUT) ->
    case cb_context:is_account_admin(Context) of
        'true' ->
            Context1 = crossbar_doc:load(AccountId, prepare_context('undefined', Context), ?TYPE_CHECK_OPTION(?PVT_TYPE)),
            case cb_context:resp_status(Context1) of
                'success' -> add_pvt_api_key(Context1);
                _Else -> Context1
            end;
        'false' -> cb_context:add_system_error('forbidden', Context)
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
    load_account_tree(AccountId, Context).

-spec add_pvt_api_key(cb_context:context()) -> cb_context:context().
add_pvt_api_key(Context) ->
    JObj = cb_context:doc(Context),
    cb_context:set_doc(Context, kzd_accounts:add_pvt_api_key(JObj)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec post(cb_context:context(), path_token()) -> cb_context:context().
post(Context, AccountId) ->
    {'ok', Existing} = kzd_accounts:fetch(AccountId),
    New = kz_json:merge(kz_doc:private_fields(Existing), maybe_import_enabled(Context, Existing)),
    case kzd_accounts:save(New) of
        {'ok', SavedAccount} ->
            Context1 = crossbar_doc:handle_datamgr_success(SavedAccount, Context),
            _NotifyPid = kz_util:spawn(fun notification_util:maybe_notify_account_change/2, [Existing, Context]),
            _ProvisionerPid = kz_util:spawn(fun provisioner_util:maybe_update_account/1, [Context1]),

            lager:debug("saved account doc, spawned workers for notification in ~p and provisioner in ~p"
                       ,[_NotifyPid, _ProvisionerPid]
                       ),

            leak_pvt_fields(AccountId, Context1);
        {'error', Error} ->
            lager:warning("failed to update account information with error: ~p", [Error]),
            crossbar_doc:handle_datamgr_errors(Error, AccountId, Context)
    end.

-spec post(cb_context:context(), path_token(), path_token()) -> cb_context:context().
post(Context, AccountId, ?MOVE) ->
    case cb_context:resp_status(Context) of
        'success' -> move_account(Context, AccountId);
        _Status -> Context
    end.

-spec patch(cb_context:context(), path_token()) -> cb_context:context().
patch(Context, AccountId) ->
    post(Context, AccountId).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec put(cb_context:context()) -> cb_context:context().
put(Context) ->
    put(Context, 'undefined').

-spec put(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
put(Context, PathAccountId) ->
    JObj = cb_context:doc(Context),
    NewAccountId = kz_doc:id(JObj, kz_datamgr:get_uuid()),

    lager:info("creating new account with id ~s", [NewAccountId]),
    try create_new_account_db(prepare_context(NewAccountId, Context)) of
        C ->
            Tree = kzd_accounts:tree(JObj),
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

-spec put(cb_context:context(), path_token(), path_token()) -> cb_context:context().
put(Context, _AccountId, ?API_KEY) ->
    C1 = crossbar_doc:save(Context),
    case cb_context:resp_status(C1) of
        'success' ->
            JObj = cb_context:doc(C1),
            ApiKey = kzd_accounts:api_key(JObj),
            RespJObj = kz_json:from_list([{<<"api_key">>, ApiKey}]),
            cb_context:set_resp_data(C1, RespJObj);
        _ -> C1
    end;
put(Context, AccountId, ?RESELLER) ->
    case kz_services_reseller:promote(AccountId) of
        {'error', 'master_account'} -> cb_context:add_system_error('forbidden', Context);
        {'error', 'reseller_descendants'} -> cb_context:add_system_error('account_has_descendants', Context);
        'ok' -> load_account(AccountId, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_account(kz_term:ne_binary()) -> 'ok' | 'error'.
delete_account(?MATCH_ACCOUNT_RAW(AccountId)) ->
    Context = prepare_context(AccountId, cb_context:new()),
    lager:info("attempting to delete ~s(~s)", [cb_context:account_id(Context)
                                              ,cb_context:account_db(Context)
                                              ]),
    Context1 = delete(Context, AccountId),
    case cb_context:resp_status(Context1) of
        'success' -> lager:info("deleted account ~s", [AccountId]);
        _Resp ->
            lager:info("failed to delete account ~s", [AccountId]),
            'error'
    end.

-spec delete(cb_context:context(), path_token()) -> cb_context:context().
delete(Context, Account) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    AccountId = kz_util:format_account_id(Account, 'raw'),
    case kapps_util:is_account_db(AccountDb) of
        'false' ->
            cb_context:add_system_error('bad_identifier', kz_json:from_list([{<<"cause">>, AccountId}]),  Context);
        'true' ->
            Context1 = delete_remove_services(prepare_context(Context, AccountId, AccountDb)),
            _ = maybe_update_descendants_count(kzd_accounts:tree(cb_context:doc(Context1))),
            Context1
    end.

-spec delete(cb_context:context(), path_token(), path_token()) -> cb_context:context().
delete(Context, AccountId, ?RESELLER) ->
    case kz_services_reseller:demote(AccountId) of
        {'error', 'master_account'} -> cb_context:add_system_error('forbidden', Context);
        {'error', 'reseller_descendants'} -> cb_context:add_system_error('account_has_descendants', Context);
        'ok' -> load_account(AccountId, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_update_descendants_count(kz_term:ne_binaries()) -> 'ok'.
maybe_update_descendants_count([]) -> 'ok';
maybe_update_descendants_count(Tree) ->
    _CountPid = kz_util:spawn(fun crossbar_util:descendants_count/1, [lists:last(Tree)]),
    lager:debug("descendants count calculation in ~p from last in ~p", [_CountPid, Tree]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_apps_store_doc(kz_term:ne_binary()) -> 'ok'.
create_apps_store_doc(AccountId) ->
    _AppsPid = kz_util:spawn(fun cb_apps_util:create_apps_store_doc/1, [AccountId]),
    lager:debug("creating apps store doc in ~p", [_AppsPid]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_move(kz_term:ne_binary(), cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> boolean().
validate_move(<<"superduper_admin">>, Context, _, _) ->
    lager:debug("using superduper_admin flag to allow move account"),
    AuthId = kz_json:get_value(<<"account_id">>, cb_context:auth_doc(Context)),
    kzd_accounts:is_superduper_admin(AuthId);
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
    lager:error("unknown move type ~p", [_Type]),
    'false'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec move_account(cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
move_account(Context, AccountId) ->
    Data = cb_context:req_data(Context),
    ToAccount = kz_json:get_binary_value(<<"to">>, Data),
    case crossbar_util:move_account(AccountId, ToAccount) of
        {'error', 'forbidden'} -> cb_context:add_system_error('forbidden', Context);
        {'error', _E} -> cb_context:add_system_error('datastore_fault', Context);
        {'ok', _} ->
            load_account(AccountId, prepare_context(AccountId, Context))
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec prepare_context(kz_term:api_ne_binary(), cb_context:context()) -> cb_context:context().
prepare_context('undefined', Context) ->
    cb_context:set_account_db(Context, ?KZ_ACCOUNTS_DB);
prepare_context(Account, Context) ->
    AccountId = kz_util:format_account_id(Account),
    AccountDb = kz_util:format_account_db(Account),
    prepare_context(Context, AccountId, AccountDb).

-spec prepare_context(cb_context:context(), kz_term:ne_binary(), kz_term:ne_binary()) -> cb_context:context().
prepare_context(Context, AccountId, AccountDb) ->
    cb_context:setters(Context, [{fun cb_context:set_account_db/2, AccountDb}
                                ,{fun cb_context:set_account_id/2, AccountId}
                                ]).

%%------------------------------------------------------------------------------
%% @doc Validate the request JObj passes all validation checks and add / alter
%% any required fields.
%% @end
%%------------------------------------------------------------------------------
-spec validate_request(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
validate_request(AccountId, Context) ->
    ReqJObj = cb_context:req_data(Context),
    ParentId = get_parent_id_from_req(Context),
    case kzd_accounts:validate(ParentId, AccountId, ReqJObj) of
        {'true', AccountJObj} ->
            Context1 = cb_context:update_successfully_validated_request(Context, AccountJObj),
            extra_validation(AccountId, Context1);
        {'validation_errors', ValidationErrors} ->
            cb_context:add_doc_validation_errors(Context, ValidationErrors);
        {'system_error', Error} when is_atom(Error) ->
            lager:info("system error validating account: ~p", [Error]),
            cb_context:add_system_error(Error, Context);
        {'system_error', {Error, Message}} ->
            lager:info("system error validating account: ~p, ~p", [Error, Message]),
            cb_context:add_system_error(Error, Message, Context)
    end.

-spec get_parent_id_from_req(cb_context:context()) -> kz_term:api_ne_binary().
get_parent_id_from_req(Context) ->
    case props:get_value(<<"accounts">>, cb_context:req_nouns(Context)) of
        [ParentId] -> ParentId;
        _Nouns ->
            case cb_context:auth_doc(Context) of
                'undefined' -> 'undefined';
                AuthDoc -> kz_json:get_ne_binary_value(<<"account_id">>, AuthDoc)
            end
    end.

-spec extra_validation(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
extra_validation(AccountId, Context) ->
    disallow_direct_clients(AccountId, Context).

-spec maybe_import_enabled(cb_context:context(), kz_json:object()) ->
          kz_json:object().
maybe_import_enabled(Context, ExistingDoc) ->
    case cb_context:auth_account_id(Context) =:= cb_context:account_id(Context) of
        'true' ->
            kz_json:delete_key(<<"enabled">>, cb_context:doc(Context));
        'false' ->
            AuthAccountId = cb_context:auth_account_id(Context),
            Doc = cb_context:doc(Context),
            Enabled = kz_json:is_true(<<"enabled">>, Doc, 'undefined'),
            NewDoc = kz_json:delete_key(<<"enabled">>, Doc),
            case lists:member(AuthAccountId, kzd_accounts:tree(ExistingDoc)) of
                'false' -> NewDoc;
                'true' ->
                    lager:debug("importing enabled from req and it is set to: ~p", [Enabled]),
                    import_enabled(NewDoc, Enabled)
            end
    end.

-spec import_enabled(kz_json:object(), kz_term:api_boolean()) -> kz_json:object().
import_enabled(Doc, 'undefined') -> Doc;
import_enabled(Doc, 'true') -> kzd_accounts:enable(Doc);
import_enabled(Doc, 'false') -> kzd_accounts:disable(Doc).

-spec disallow_direct_clients(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
disallow_direct_clients(AccountId, Context) ->
    ShouldAllow = ?ALLOW_DIRECT_CLIENTS,
    lager:debug("will allow direct clients: ~s", [ShouldAllow]),
    maybe_disallow_direct_clients(AccountId, Context, ShouldAllow).

-spec maybe_disallow_direct_clients(kz_term:api_binary(), cb_context:context(), boolean()) ->
          cb_context:context().
maybe_disallow_direct_clients(_AccountId, Context, 'true') ->
    Context;
maybe_disallow_direct_clients(_AccountId, Context, 'false') ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    AuthAccountId = cb_context:auth_account_id(Context),
    AuthUserReseller = kz_services_reseller:get_id(AuthAccountId),
    case AuthUserReseller =/= MasterAccountId
        orelse kz_services_reseller:is_reseller(AuthAccountId)
    of
        'true' -> Context;
        'false' ->
            lager:debug("direct account ~p is disallowed from creating sub-accounts", [AuthAccountId]),
            Msg = kz_json:from_list(
                    [{<<"message">>, <<"Direct account is not allowed to create sub-accounts">>}
                    ,{<<"cause">>, AuthAccountId}
                    ]),
            cb_context:add_validation_error([<<"account">>], <<"forbidden">>, Msg, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc Load an account document from the database
%% @end
%%------------------------------------------------------------------------------
-spec validate_delete_request(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_delete_request(AccountId, Context) ->
    case kapps_util:account_has_descendants(AccountId) of
        'true' ->  cb_context:add_system_error('account_has_descendants', Context);
        'false' ->
            case knm_port_request:account_has_active_port(AccountId) of
                'false' -> cb_context:set_resp_status(Context, 'success');
                'true' ->
                    lager:debug("prevent deleting account ~s due to has active port request", [AccountId]),
                    Msg = kz_json:from_list(
                            [{<<"message">>, <<"Account has active port request">>}
                            ]),
                    cb_context:add_system_error('account_has_active_port', Msg, Context)
            end
    end.

-spec validate_patch_request(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
validate_patch_request(AccountId, Context) ->
    crossbar_doc:patch_and_validate(AccountId, Context, fun validate_request/2).

%%------------------------------------------------------------------------------
%% @doc Load an account document from the database
%% @end
%%------------------------------------------------------------------------------
-spec load_account(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_account(AccountId, Context) ->
    leak_pvt_fields(AccountId, crossbar_doc:load(AccountId, Context, ?TYPE_CHECK_OPTION(?PVT_TYPE))).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------

-spec leak_pvt_fields(kz_term:api_binary(), cb_context:context()) -> cb_context:context().
leak_pvt_fields(AccountId, Context) ->
    leak_pvt_fields(AccountId, Context, cb_context:resp_status(Context)).

-spec leak_pvt_fields(kz_term:api_binary(), cb_context:context(), crossbar_status()) -> cb_context:context().
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
                                              ,kzd_accounts:allow_number_additions(cb_context:doc(Context))
                                              ,cb_context:resp_data(Context)
                                              )
                            ).

-spec leak_pvt_superduper_admin(cb_context:context()) -> cb_context:context().
leak_pvt_superduper_admin(Context) ->
    cb_context:set_resp_data(Context
                            ,kz_json:set_value(<<"superduper_admin">>
                                              ,kzd_accounts:is_superduper_admin(cb_context:doc(Context))
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
                                                      ,kzd_accounts:api_key(cb_context:doc(Context))
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
    case kzd_accounts:is_enabled(cb_context:doc(Context)) of
        'true' ->
            cb_context:set_resp_data(Context
                                    ,kz_json:set_value(<<"enabled">>, 'true', RespJObj)
                                    );
        'false' ->
            cb_context:set_resp_data(Context
                                    ,kz_json:set_value(<<"enabled">>, 'false', RespJObj)
                                    )
    end.

-spec leak_reseller_id(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
leak_reseller_id(Context, PathAccountId) ->
    cb_context:set_resp_data(Context
                            ,kz_json:set_value(<<"reseller_id">>
                                              ,find_reseller_id(Context, PathAccountId)
                                              ,cb_context:resp_data(Context)
                                              )
                            ).

-spec leak_is_reseller(cb_context:context()) -> cb_context:context().
leak_is_reseller(Context) ->
    IsReseller = kz_services_reseller:is_reseller(cb_context:account_id(Context)),
    cb_context:set_resp_data(Context
                            ,kz_json:set_value(<<"is_reseller">>
                                              ,IsReseller
                                              ,cb_context:resp_data(Context)
                                              )
                            ).

-spec leak_billing_mode(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
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

-spec find_reseller_id(cb_context:context(), kz_term:api_binary()) -> kz_term:api_binary().
find_reseller_id(Context, 'undefined') ->
    %% only when put/1
    cb_context:reseller_id(Context);
find_reseller_id(Context, PathAccountId) ->
    IsNotSelf = PathAccountId =/= cb_context:account_id(Context),
    case kz_services_reseller:is_reseller(PathAccountId) of
        'true' when IsNotSelf -> PathAccountId;
        'true' -> cb_context:reseller_id(Context);
        'false' -> cb_context:reseller_id(Context)
    end.

-spec leak_notification_preference(cb_context:context()) -> cb_context:context().
leak_notification_preference(Context) ->
    leak_notification_preference(Context, kzd_accounts:notification_preference(cb_context:doc(Context))).

-spec leak_notification_preference(cb_context:context(), kz_term:api_binary()) -> cb_context:context().
leak_notification_preference(Context, 'undefined') ->
    Context;
leak_notification_preference(Context, Pref) ->
    UpdatedRespJObj = kz_json:set_value(<<"notification_preference">>, Pref, cb_context:resp_data(Context)),
    cb_context:set_resp_data(Context, UpdatedRespJObj).

-spec leak_trial_time_left(cb_context:context()) ->
          cb_context:context().
leak_trial_time_left(Context) ->
    JObj = cb_context:doc(Context),
    leak_trial_time_left(Context, JObj, kzd_accounts:trial_expiration(JObj)).

-spec leak_trial_time_left(cb_context:context(), kz_json:object(), kz_term:api_integer()) ->
          cb_context:context().
leak_trial_time_left(Context, _JObj, 'undefined') ->
    RespData = kz_json:delete_key(<<"trial_time_left">>
                                 ,cb_context:resp_data(Context)
                                 ),
    cb_context:set_resp_data(Context, RespData);
leak_trial_time_left(Context, JObj, _Expiration) ->
    RespData = kz_json:set_value(<<"trial_time_left">>
                                ,kzd_accounts:trial_time_left(JObj)
                                ,cb_context:resp_data(Context)
                                ),
    cb_context:set_resp_data(Context, RespData).

%%------------------------------------------------------------------------------
%% @doc Load a summary of the children of this account
%% @end
%%------------------------------------------------------------------------------
-spec load_children(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_children(AccountId, Context) ->
    load_children(AccountId, Context, cb_context:api_version(Context)).

-spec load_children(kz_term:ne_binary(), cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_children(AccountId, Context, ?VERSION_1) ->
    load_children_v1(AccountId, Context);
load_children(AccountId, Context, _Version) ->
    load_paginated_children(AccountId, Context).

-spec load_children_v1(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_children_v1(AccountId, Context) ->
    crossbar_doc:load_view(?AGG_VIEW_CHILDREN
                          ,[{'startkey', [AccountId]}
                           ,{'endkey', [AccountId, kz_json:new()]}
                           ]
                          ,Context
                          ,fun normalize_view_results/2
                          ).

-spec load_paginated_children(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
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

%%------------------------------------------------------------------------------
%% @doc Load a summary of the descendants of this account
%% @end
%%------------------------------------------------------------------------------
-spec load_descendants(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_descendants(AccountId, Context) ->
    load_descendants(AccountId, Context, cb_context:api_version(Context)).

load_descendants(AccountId, Context, ?VERSION_1) ->
    load_descendants_v1(AccountId, Context);
load_descendants(AccountId, Context, _Version) ->
    load_paginated_descendants(AccountId, Context).

-spec load_descendants_v1(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_descendants_v1(AccountId, Context) ->
    crossbar_doc:load_view(?AGG_VIEW_DESCENDANTS
                          ,[{'startkey', [AccountId]}
                           ,{'endkey', [AccountId, kz_json:new()]}
                           ]
                          ,Context
                          ,fun normalize_view_results/2
                          ).

-spec load_paginated_descendants(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
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

%%------------------------------------------------------------------------------
%% @doc Load a summary of the siblings of this account
%% @end
%%------------------------------------------------------------------------------
-spec load_siblings(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_siblings(AccountId, Context) ->
    case kzd_accounts:is_superduper_admin(cb_context:auth_account_id(Context))
        orelse
        (AccountId =/= cb_context:auth_account_id(Context)
         andalso kapps_config:get_is_true(?ACCOUNTS_CONFIG_CAT, <<"allow_sibling_listing">>, 'true')
        )
    of
        'true' -> load_siblings(AccountId, Context, cb_context:api_version(Context));
        'false' -> cb_context:add_system_error('forbidden', Context)
    end.

-spec load_siblings(kz_term:ne_binary(), cb_context:context(), kz_term:ne_binary()) -> cb_context:context().
load_siblings(AccountId, Context, ?VERSION_1) ->
    load_siblings_v1(AccountId, Context);
load_siblings(AccountId, Context, _Version) ->
    load_paginated_siblings(AccountId, Context).

-spec load_siblings_v1(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
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

-spec load_paginated_siblings(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
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

-spec load_siblings_results(kz_term:ne_binary(), cb_context:context(), kz_json:objects()) -> cb_context:context().
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
    RespData = lists:reverse(cb_context:resp_data(Context)),
    RespEnv = lists:foldl(fun fix_envelope_fold/2
                         ,cb_context:resp_envelope(Context)
                         ,[<<"start_key">>, <<"next_start_key">>]
                         ),
    cb_context:set_resp_envelope(cb_context:set_resp_data(Context, RespData), RespEnv).

-spec fix_envelope_fold(binary(), kz_json:object()) -> kz_json:object().
fix_envelope_fold(Key, JObj) ->
    case fix_start_key(kz_json:get_value(Key, JObj)) of
        'undefined' -> kz_json:delete_key(Key, JObj);
        V -> kz_json:set_value(Key, V, JObj)
    end.

-spec fix_start_key(kz_term:api_binary() | list()) -> kz_term:api_binary().
fix_start_key('undefined') -> 'undefined';
fix_start_key(<<_/binary>> = StartKey) -> StartKey;
fix_start_key([StartKey]) -> StartKey;
fix_start_key([_AccountId, [_|_]=Keys]) -> lists:last(Keys);
fix_start_key([_AccountId, StartKey]) -> StartKey;
fix_start_key([StartKey|_T]) -> StartKey.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_account_tree(kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
load_account_tree(AccountId, Context) ->
    Context1 = crossbar_doc:load(AccountId, prepare_context('undefined', Context), ?TYPE_CHECK_OPTION(?PVT_TYPE)),
    case cb_context:resp_status(Context1) of
        'success' -> load_account_tree(Context1);
        _Else -> Context1
    end.

-spec load_account_tree(cb_context:context()) -> cb_context:context().
load_account_tree(Context) ->
    Tree = get_authorized_account_tree(Context),
    case kz_datamgr:open_cache_docs(?KZ_ACCOUNTS_DB, Tree) of
        {'error', R} -> crossbar_doc:handle_datamgr_errors(R, ?KZ_ACCOUNTS_DB, Context);
        {'ok', JObjs} -> format_account_tree_results(Context, JObjs)
    end.

-spec get_authorized_account_tree(cb_context:context()) -> kz_term:ne_binaries().
get_authorized_account_tree(Context) ->
    AuthAccountId = cb_context:auth_account_id(Context),
    lists:dropwhile(fun(E) -> E =/= AuthAccountId end
                   ,kzd_accounts:tree(cb_context:doc(Context))
                   ).

-spec format_account_tree_results(cb_context:context(), kz_json:objects()) -> cb_context:context().
format_account_tree_results(Context, JObjs) ->
    RespData =
        [kz_json:from_list(
           [{<<"id">>, kz_doc:id(JObj)}
           ,{<<"name">>, kz_json:get_value([<<"doc">>, <<"name">>], JObj)}
           ,{<<"realm">>, kz_json:get_value([<<"doc">>, <<"realm">>], JObj)}
           ,{<<"flags">>, kz_json:get_value([<<"doc">>, <<"flags">>], JObj, [])}
           ])
         || JObj <- JObjs
        ],
    cb_context:set_resp_data(Context, RespData).

%%------------------------------------------------------------------------------
%% @doc Normalizes the results of a view.
%% @end
%%------------------------------------------------------------------------------
-spec normalize_view_results(kz_json:object(), kz_json:objects()) -> kz_json:objects().
normalize_view_results(JObj, Acc) ->
    [kz_json:get_value(<<"value">>, JObj)|Acc].

%%------------------------------------------------------------------------------
%% @doc This function will attempt to load the context with the db name of
%% for this account
%% @end
%%------------------------------------------------------------------------------
-spec load_account_db(cb_context:context(), kz_term:ne_binary() | kz_term:ne_binaries()) ->
          cb_context:context().
load_account_db(Context, [AccountId|_]) ->
    load_account_db(Context, AccountId);
load_account_db(Context, AccountId) when is_binary(AccountId) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok', JObj} ->
            AccountDb = kz_util:format_account_db(AccountId),
            lager:debug("account ~s db exists, setting operating database as ~s", [AccountId, AccountDb]),
            ResellerId = kz_services_reseller:find_id(AccountId),
            cb_context:setters(Context
                              ,[{fun cb_context:set_resp_status/2, 'success'}
                               ,{fun cb_context:set_account_db/2, AccountDb}
                               ,{fun cb_context:set_account_id/2, AccountId}
                               ,{fun cb_context:set_account_name/2, kzd_accounts:name(JObj)}
                               ,{fun cb_context:set_reseller_id/2, ResellerId}
                               ]);
        {'error', 'not_found'} ->
            Msg = kz_json:from_list([{<<"cause">>, AccountId}]),
            cb_context:add_system_error('bad_identifier', Msg, Context);
        {'error', _R} ->
            crossbar_util:response_db_fatal(Context)
    end.

%%------------------------------------------------------------------------------
%% @doc This function will create a new account and corresponding database
%% then spawn a short initial function
%% @end
%%------------------------------------------------------------------------------
-spec create_new_account_db(cb_context:context()) -> cb_context:context().
create_new_account_db(Context) ->
    AccountDb = cb_context:account_db(Context),
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
            lager:debug("loaded initial views"),

            _ = crossbar_bindings:map(<<"account.created">>, C),
            lager:debug("alerted listeners of new account"),

            _ = create_account_mod(cb_context:account_id(C)),
            lager:debug("created this month's MODb for account"),

            _ = crossbar_services:reconcile(AccountDb),
            lager:debug("performed initial services reconcile"),

            _ = create_first_transaction(cb_context:account_id(C)),
            lager:debug("created first transaction for account"),

            _ = maybe_set_notification_preference(C),
            lager:debug("set notification preference"),

            maybe_notify_new_account(Context),
            C
    end.

-spec maybe_set_notification_preference(cb_context:context()) -> 'ok'.
maybe_set_notification_preference(Context) ->
    AccountId = cb_context:account_id(Context),
    ResellerId = kz_services_reseller:find_id(AccountId),
    case kzd_accounts:fetch(ResellerId) of
        {'error', _E} ->
            lager:error("failed to open reseller '~s': ~p", [ResellerId, _E]);
        {'ok', AccountJObj} ->
            case kzd_accounts:notification_preference(AccountJObj) of
                'undefined' ->
                    lager:debug("notification preference not set on reseller '~s'", [ResellerId]);
                Preference ->
                    set_notification_preference(Context, Preference)
            end
    end.

-spec set_notification_preference(cb_context:context(), kz_term:ne_binary()) -> 'ok'.
set_notification_preference(Context, Preference) ->
    AccountDefinition = kzd_accounts:set_notification_preference(cb_context:doc(Context), Preference),
    case kzd_accounts:save(AccountDefinition) of
        {'error', _R} ->
            lager:error("failed to update account definition: ~p", [_R]);
        {'ok', _AccountDef} ->
            lager:info("notification_preference set to '~s'", [Preference])
    end.

-spec create_account_mod(kz_term:ne_binary()) -> any().
create_account_mod(AccountId) ->
    Db = kz_util:format_account_mod_id(AccountId),
    kazoo_modb:create(Db).

-spec create_first_transaction(kz_term:ne_binary()) -> any().
create_first_transaction(AccountId) ->
    {Year, Month, _} = erlang:date(),
    kz_currency:rollover(AccountId, Year, Month, 0).

-spec create_account_definition(cb_context:context()) -> cb_context:context().
create_account_definition(Context) ->
    Doc = crossbar_doc:update_pvt_parameters(cb_context:doc(Context), Context),
    JObj = maybe_set_trial_expires(kz_doc:set_id(Doc, cb_context:account_id(Context))),

    case kzd_accounts:save(JObj) of
        {'ok', AccountDef} ->
            lager:debug("account definition created: ~s", [kz_doc:revision(AccountDef)]),

            Context1 = crossbar_doc:handle_datamgr_success(AccountDef, Context),
            leak_pvt_fields(kz_doc:id(AccountDef), Context1);
        {'error', _R} ->
            lager:debug("unable to create account definition: ~p", [_R]),
            throw(cb_context:add_system_error('datastore_fault', Context))
    end.

-spec maybe_set_trial_expires(kz_json:object()) -> kz_json:object().
maybe_set_trial_expires(JObj) ->
    case kzd_accounts:is_trial_account(JObj) of
        'false' -> JObj;
        'true' -> set_trial_expires(JObj)
    end.

-spec set_trial_expires(kz_json:object()) -> kz_json:object().
set_trial_expires(JObj) ->
    TrialTime = kapps_config:get_integer(?ACCOUNTS_CONFIG_CAT, <<"trial_time">>, ?SECONDS_IN_DAY * 14),
    Expires = kz_time:now_s() + TrialTime,
    kzd_accounts:set_trial_expiration(JObj, Expires).

-spec load_initial_views(cb_context:context()) -> 'ok'.
load_initial_views(Context)->
    _ = kz_datamgr:refresh_views(cb_context:account_db(Context)),
    'ok'.

-spec maybe_notify_new_account(cb_context:context()) -> 'ok'.
maybe_notify_new_account(Context) ->
    case kz_term:is_true(cb_context:req_value(Context, <<"send_email_on_creation">>, 'true')) of
        'false' -> 'ok';
        'true' -> notify_new_account(Context)
    end.

%%------------------------------------------------------------------------------
%% @doc Send a notification that the account has been created.
%% @end
%%------------------------------------------------------------------------------
-spec notify_new_account(cb_context:context()) -> 'ok'.
notify_new_account(Context) ->
    notify_new_account(Context, cb_context:auth_doc(Context)).

notify_new_account(_Context, 'undefined') -> 'ok';
notify_new_account(Context, _AuthDoc) ->
    _ = cb_context:put_reqid(Context),
    JObj = cb_context:doc(Context),
    lager:debug("triggering new account notification for ~s", [cb_context:account_id(Context)]),
    Notify = [{<<"Account-Name">>, kzd_accounts:name(JObj)}
             ,{<<"Account-Realm">>, kzd_accounts:realm(JObj)}
             ,{<<"Account-API-Key">>, kzd_accounts:api_key(JObj)}
             ,{<<"Account-ID">>, cb_context:account_id(Context)}
             ,{<<"Account-DB">>, cb_context:account_db(Context)}
              | kz_api:default_headers(?APP_VERSION, ?APP_NAME)
             ],
    kapps_notify_publisher:cast(Notify, fun kapi_notifications:publish_new_account/1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec delete_remove_services(cb_context:context()) -> cb_context:context() | boolean().
delete_remove_services(Context) ->
    try kz_services:delete(cb_context:account_id(Context)) of
        _S -> delete_free_numbers(Context)
    catch
        _E:_R ->
            lager:error("failed to delete services: ~s: ~p", [_E, _R]),
            crossbar_util:response('error', <<"unable to cancel services">>, 500, Context)
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
                      lager:debug("failed to open account definition ~s: ~p", [cb_context:account_id(Context), _R]),
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
delete_mod_dbs(Context) ->
    AccountId = cb_context:account_id(Context),
    {Year, Month, _} = erlang:date(),
    delete_mod_dbs(AccountId, Year, Month).

-spec delete_mod_dbs(kz_term:ne_binary(), kz_time:year(), kz_time:month()) -> 'true'.
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
            UpdatedContext = cb_context:setters(Context
                                               ,[{fun cb_context:set_account_db/2, ?KZ_ACCOUNTS_DB}
                                                ,{fun cb_context:set_doc/2, JObj}
                                                ]),
            crossbar_doc:delete(UpdatedContext);

        {'error', 'not_found'} ->
            crossbar_util:response(kz_json:new(), Context);
        {'error', _R} ->
            crossbar_util:response('error', <<"unable to remove account definition">>, 500, Context)
    end.
