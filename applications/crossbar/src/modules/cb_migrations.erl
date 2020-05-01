%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Handle various migrations that can be performed on accounts
%%% @author Mark Magnusson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cb_migrations).

-export([init/0
        ,allowed_methods/0, allowed_methods/1
        ,resource_exists/0, resource_exists/1
        ,validate/1, validate/2
        ]).

-include("crossbar.hrl").

%% {ACCOUNT_ID}.migrations
-define(MIGRATIONS_DOC_ID, <<"migrations">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init() -> 'ok'.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.migrations">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.migrations">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.migrations">>, ?MODULE, 'validate'),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_MigrationId) ->
    [?HTTP_GET, ?HTTP_POST].

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    maybe_create_migration_doc(Context),
    validate(Context, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token()) -> cb_context:context().
validate(Context, ?HTTP_GET) ->
    load_migration_list(Context);

validate(Context, MigId) ->
    validate(Context, MigId, cb_context:req_verb(Context)).

-spec validate(cb_context:context(), path_token(), http_method()) -> cb_context:context().
validate(Context, DocId, ?HTTP_GET) ->
    load_migration_summary(DocId, Context);

validate(Context, DocId, ?HTTP_POST) ->
    maybe_perform_migration(DocId, Context).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_migration_list(cb_context:context()) -> cb_context:context().
load_migration_list(Context) ->
    Setters = [{fun cb_context:set_resp_data/2, 'success'}
              ,{fun cb_context:set_resp_data/2, crossbar_migration:list()}
              ],
    cb_context:setters(Context, Setters).

-spec load_migration_summary(binary(), cb_context:context()) -> cb_context:context().
load_migration_summary(MigId, Context) ->
    case kz_json:get_ne_binary_value(MigId, crossbar_migration:list()) of
        'undefined' ->
            Context1 = cb_context:set_resp_data(Context, <<"migration not found">>),
            cb_context:set_resp_error_code(Context1, 404);

        Desc ->
            render_migration_summary(MigId, Desc, Context)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec render_migration_summary(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) -> cb_context:context().
render_migration_summary(Id, Desc, Context) ->
    Base = [{<<"id">>, Id}
           ,{<<"description">>, Desc}
            | get_migration_performed(Id, Context)
           ],
    Resp = kz_json:from_list(Base),

    Context1 = cb_context:set_resp_data(Context, Resp),
    cb_context:set_resp_status(Context1, 'success').

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_migration_performed(binary(), cb_context:context()) -> kz_term:proplist().
get_migration_performed(Id, Context) ->
    case kz_datamgr:open_cache_doc(cb_context:db_name(Context), ?MIGRATIONS_DOC_ID) of
        {'ok', Doc} ->
            check_migration_performed(Id, Doc);

        _Other ->
            [{<<"performed">>, 'false'}]
    end.

-spec check_migration_performed(binary(), kz_json:object()) -> kz_term:proplist().
check_migration_performed(Id, Doc) ->
    case kz_json:get_value([<<"migrations_performed">>, Id], Doc) of
        'undefined' ->
            [{<<"performed">>, 'false'}];

        Value ->
            [{<<"performed">>, 'true'}, {<<"performed_time">>, kz_json:get_value(<<"performed_time">>, Value)}]
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_perform_migration(path_token(), cb_context:context()) -> cb_context:context().
maybe_perform_migration(MigId, Context) ->
    case kz_json:get_value(<<"perform_migration">>, cb_context:req_data(Context)) of
        <<"now">> ->
            check_migration_valid(MigId, Context);

        _Other ->
            Cause = kz_json:from_list([{<<"error">>, <<"invalid action time">>}]),
            cb_context:add_validation_error(<<"migration">>, <<"failed">>, Cause, Context)
    end.

-spec check_migration_valid(path_token(), cb_context:context()) -> cb_context:context().
check_migration_valid(MigId, Context) ->
    case kz_json:get_ne_binary_value(MigId, crossbar_migration:list()) of
        'undefined' ->
            Cause = kz_json:from_list([{<<"migration">>, <<"not found">>}]),
            cb_context:add_validation_error(<<"migration">>, <<"invalid">>, Cause, Context);

        _ ->
            Context1 = perform_migration(MigId, Context),
            cb_context:set_resp_status(Context1, 'success')
    end.

-spec perform_migration(path_token(), cb_context:context()) -> cb_context:context().
perform_migration(MigId, Context) ->
    case kz_json:is_true(<<"include_descendants">>, cb_context:req_data(Context)) of
        'true' ->
            Descendants = get_account_descendants(Context),
            List = [migrate_on_account(AccountId, MigId, Context) || AccountId <- Descendants],
            cb_context:set_resp_data(Context, kz_json:from_list([{<<"performed_on">>, List}]));

        'false' ->
            _ = migrate_on_account(cb_context:account_id(Context), MigId, Context),
            cb_context:set_resp_data(Context, kz_json:from_list([{<<"performed_on">>, [cb_context:account_id(Context)]}]))
    end.

-spec get_account_descendants(cb_context:context()) -> kz_term:ne_binaries().
get_account_descendants(Context) ->
    AccountId = cb_context:account_id(Context),
    IncludeResellers = kz_json:is_true(<<"include_resellers">>, cb_context:req_data(Context)),

    ViewOptions = [{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    View = <<"accounts/listing_by_descendants">>,
    case kz_datamgr:get_results(?KZ_ACCOUNTS_DB, View, ViewOptions) of
        {'ok', JObjs} ->
            Descendants = [kz_doc:id(JObj) || JObj <- JObjs],
            [AccountId, maybe_filter_resellers(Descendants, IncludeResellers)];
        {'error', _Reason}  ->
            lager:debug("failed to get account ~s descendants: ~p", [AccountId, _Reason]),
            [AccountId]
    end.

-spec maybe_filter_resellers(kz_term:ne_binaries(), boolean()) -> kz_term:ne_binaries().
maybe_filter_resellers(AccountIds, 'false') ->
    [Id
     || Id <- AccountIds,
        not kz_services_reseller:is_reseller(Id)
    ];
maybe_filter_resellers(AccountIds, 'true') ->
    AccountIds.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate_on_account(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) -> binary().
migrate_on_account(Account, MigId, Context) ->
    Result = crossbar_migration:perform(MigId, Account, Context),
    maybe_mark_as_complete(MigId, Account, Result),
    Account.

-spec maybe_mark_as_complete(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) -> 'ok'.
maybe_mark_as_complete(MigId, Account, Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            mark_migration_complete(MigId, Account, Context);
        _Failed ->
            lager:info("migration ~p failed to complete", [MigId])
    end.

-spec mark_migration_complete(kz_term:ne_binary(), kz_term:ne_binary(), cb_context:context()) -> 'ok'.
mark_migration_complete(MigId, AccountId, Context) ->
    lager:info("migration ~p completed successfully on account ~p", [MigId, AccountId]),

    AccountDb = kzs_util:format_account_db(AccountId),
    maybe_create_migration_doc(Context),

    {'ok', Doc} = kz_datamgr:open_cache_doc(AccountDb, ?MIGRATIONS_DOC_ID),
    Migrations  = kz_json:get_value(<<"migrations_performed">>, Doc),

    User = cb_context:auth_user_id(Context),
    AuthAccountId = cb_context:auth_account_id(Context),

    AuthDoc = cb_context:auth_doc(Context),
    Args = kz_json:from_list(
             [{<<"auth_user_id">>, User}
             ,{<<"auth_account_id">>, AuthAccountId}
             ,{<<"auth_account_name">>, kzd_accounts:fetch_name(AuthAccountId)}
             ,{<<"auth_user_name">>, get_user_name(AuthAccountId, User)}
             ,{<<"performed_time">>, kz_time:now_s()}
             ,{<<"original_auth_account_id">>, kz_json:get_value(<<"original_account_id">>, AuthDoc)}
             ,{<<"original_auth_owner_id">>, kz_json:get_value(<<"original_owner_id">>, AuthDoc)}
             ]),

    NewMigs = kz_json:set_value(MigId, Args, Migrations),
    {'ok', _} = kz_datamgr:save_doc(AccountDb, kz_json:set_value(<<"migrations_performed">>, NewMigs, Doc)),
    lager:debug("migrating ~s in ~s complete", [MigId, AccountId]).

-spec get_user_name(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_ne_binary().
get_user_name(AccountId, UserId) ->
    case kzd_users:fetch(AccountId, UserId) of
        {'ok', UserDoc} -> kzd_users:name(UserDoc);
        _ -> 'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_create_migration_doc(cb_context:context()) -> 'ok'.
maybe_create_migration_doc(Context) ->
    DbName = cb_context:db_name(Context),
    case kz_datamgr:open_cache_doc(DbName, ?MIGRATIONS_DOC_ID) of
        {'ok', _} -> 'ok';
        {'error', 'not_found'} ->
            lager:info("creating migrations document for account ~p", [cb_context:account_id(Context)]),
            create_migration_doc(Context)
    end.

-spec create_migration_doc(cb_context:context()) -> 'ok'.
create_migration_doc(Context) ->
    DbName = cb_context:db_name(Context),

    Setters = [{fun kz_doc:set_id/2, ?MIGRATIONS_DOC_ID}
              ,{fun kz_doc:set_type/2, <<"migrations">>}
              ],
    Doc = kz_doc:setters(kz_json:from_list([{<<"migrations_performed">>, []}]), Setters),
    JObj = crossbar_doc:update_pvt_parameters(Doc, Context),

    _ = kz_datamgr:save_doc(DbName, JObj),

    lager:debug("created migration doc for account ~s", [cb_context:account_id(Context)]).
