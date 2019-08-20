%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
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
-define(MIGRATIONS_DOC, <<"migrations">>).

%% Id, Description, Callback Module
-define(MIGRATIONS_LIST, [{<<"notify_to_teletype">>, <<"Migrate account and all sub accounts to Teletype">>, 'cb_migration_disable_notify'}
                         ]).

-spec init() -> ok.
init() ->
    _ = crossbar_bindings:bind(<<"*.allowed_methods.migrations">>, ?MODULE, 'allowed_methods'),
    _ = crossbar_bindings:bind(<<"*.resource_exists.migrations">>, ?MODULE, 'resource_exists'),
    _ = crossbar_bindings:bind(<<"*.validate.migrations">>, ?MODULE, 'validate'),
    ok.

-spec allowed_methods() -> http_methods().
allowed_methods() ->
    [?HTTP_GET].

-spec allowed_methods(path_token()) -> http_methods().
allowed_methods(_MigrationId) ->
    [?HTTP_GET, ?HTTP_POST].

-spec resource_exists() -> 'true'.
resource_exists() -> 'true'.

-spec resource_exists(path_token()) -> 'true'.
resource_exists(_) -> 'true'.

-spec validate(cb_context:context()) -> cb_context:context().
validate(Context) ->
    maybe_create_migration_doc(cb_context:account_db(Context)),
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

-spec load_migration_list(cb_context:context()) -> cb_context:context().
load_migration_list(Context) ->
    Format = fun({I, D, _C}, Acc) ->
                     kz_json:set_value(I, D, Acc)
             end,

    Resp     = lists:foldl(Format, kz_json:new(), ?MIGRATIONS_LIST),
    Context1 = cb_context:set_resp_data(Context, Resp),

    cb_context:set_resp_status(Context1, 'success').

-spec load_migration_summary(binary(), cb_context:context()) -> cb_context:context().
load_migration_summary(MigId, Context) ->
    case lists:keyfind(MigId, 1, ?MIGRATIONS_LIST) of
        'false' ->
            Context1 = cb_context:set_resp_data(Context, <<"migration not found">>),
            cb_context:set_resp_error_code(Context1, 404);

        Migration ->
            render_migration_summary(Migration, Context)
    end.

-spec render_migration_summary({binary(), binary(), atom()}, cb_context:context()) -> cb_context:context().
render_migration_summary({Id, Desc, _Callback}, Context) ->
    Base = [{<<"id">>, Id}
           ,{<<"description">>, Desc}
           ],

    Perf = get_migration_performed(Id, Context),
    Resp = kz_json:from_list(Base ++ Perf),

    Context1 = cb_context:set_resp_data(Context, Resp),
    cb_context:set_resp_status(Context1, 'success').

-spec get_migration_performed(binary(), cb_context:context()) -> list().
get_migration_performed(Id, Context) ->
    case kz_datamgr:open_cache_doc(cb_context:account_db(Context), ?MIGRATIONS_DOC) of
        {'ok', Doc} ->
            check_migration_performed(Id, Doc);

        _Other ->
            [{<<"performed">>, 'false'}]
    end.

-spec check_migration_performed(binary(), kz_json:object()) -> list().
check_migration_performed(Id, Doc) ->
    case kz_json:get_value([<<"migrations_performed">>, Id], Doc) of
        'undefined' ->
            [{<<"performed">>, 'false'}];

        Value ->
            [{<<"performed">>, 'true'}, {<<"performed_time">>, kz_json:get_value(<<"performed_time">>, Value)}]
    end.

-spec maybe_perform_migration(binary(), cb_context:context()) -> cb_context:context().
maybe_perform_migration(MigId, Context) ->
    case kz_json:get_value(<<"perform_migration">>, cb_context:req_data(Context)) of
        <<"now">> ->
            check_migration_valid(MigId, Context);

        _Other ->
            Cause = kz_json:from_list([{<<"error">>, <<"invalid action time">>}]),
            cb_context:add_validation_error(<<"migration">>, <<"failed">>, Cause, Context)
    end.

-spec check_migration_valid(binary(), cb_context:context()) -> cb_context:context().
check_migration_valid(MigId, Context) ->
    case lists:keyfind(MigId, 1, ?MIGRATIONS_LIST) of
        'false' ->
            Cause = kz_json:from_list([{<<"migration">>, <<"not found">>}]),
            cb_context:add_validation_error(<<"migration">>, <<"invalid">>, Cause, Context);

        {_, _, Module} ->
            Context1 = perform_migration(MigId, Module, Context),
            cb_context:set_resp_status(Context1, 'success')
    end.

-spec perform_migration(binary(), atom(), cb_context:context()) -> cb_context:context().
perform_migration(MigId, Module, Context) ->
    {'ok', All} = kz_datamgr:get_all_results(<<"accounts">>, <<"accounts/listing_by_descendants">>),
    Account     = cb_context:account_id(Context),

    Result = case kz_json:is_true(<<"include_descendants">>, cb_context:req_data(Context)) of
                 'true' ->
                     Descendants = filter_account_descendants(Account, All, []),
                     Filtered    = maybe_filter_resellers(Descendants, Context),
                     List = [migrate_on_account(kz_json:get_value(<<"id">>, X), MigId, Module, Context) || X <- Filtered],
                     kz_json:from_list([{<<"performed_on">>, List}]);

                 'false' ->
                     _ = migrate_on_account(cb_context:account_id(Context), MigId, Module, Context),
                     kz_json:from_list([{<<"performed_on">>, [cb_context:account_id(Context)]}])
             end,
    cb_context:set_resp_data(Context, Result).

-spec maybe_filter_resellers(list(), cb_context:context()) -> list(kz_json:object()).
maybe_filter_resellers(Accounts, Context) ->
    case kz_json:is_true(<<"include_resellers">>, cb_context:req_data(Context)) of
        'true'  -> Accounts;
        'false' -> filter_account_resellers(Accounts)
    end.

-spec filter_account_descendants(binary(), list(), list()) -> list().
filter_account_descendants(Account, [H|T], Acc) ->
    case kz_json:get_value(<<"key">>, H) of
        [Account, _] -> filter_account_descendants(Account, T, [H|Acc]);
        _OtherValue  -> filter_account_descendants(Account, T, Acc)
    end;

filter_account_descendants(_Account, [], Acc) ->
    Acc.

-spec filter_account_resellers(list()) -> list().
filter_account_resellers(Accounts) ->
    Resellers = get_resellers(Accounts, []),
    filter_account_resellers(Accounts, Resellers, []).

-spec filter_account_resellers(list(), list(), list()) -> list().
filter_account_resellers([H|T], Res, Acc) ->
    Tree = kz_json:get_value([<<"value">>, <<"tree">>], H),

    case lists:subtract(Tree, Res) of
        New when length(New) < length(Tree) ->
            filter_account_resellers(T, Res, Acc);

        _NotInList ->
            filter_account_resellers(T, Res, [H|Acc])
    end;

filter_account_resellers([], _Res, Acc) ->
    Acc.

-spec get_resellers(list(), list()) -> list().
get_resellers([H|T], Acc) ->
    Account = kz_json:get_value(<<"id">>, H),
    case kz_services_reseller:is_reseller(Account) of
        'true'  -> get_resellers(T, [Account|Acc]);
        'false' -> get_resellers(T, Acc)
    end;

get_resellers([], Acc) ->
    Acc.

-spec migrate_on_account(binary(), binary(), atom(), cb_context:context()) -> binary().
migrate_on_account(Account, MigId, Module, Context) ->
    Result = Module:perform_migration(Account, Context),
    maybe_mark_as_complete(MigId, Account, Result),
    Account.

-spec maybe_mark_as_complete(binary(), binary(), cb_context:context()) -> 'ok'.
maybe_mark_as_complete(MigId, Account, Context) ->
    case cb_context:resp_status(Context) of
        'success' ->
            mark_migration_complete(MigId, Account, Context);
        _Failed ->
            lager:info("migration ~p failed to complete", [MigId])
    end.

-spec mark_migration_complete(binary(), binary(), cb_context:context()) -> 'ok'.
mark_migration_complete(MigId, AccountId, Context) ->
    lager:info("migration ~p completed successfully on account ~p", [MigId, AccountId]),

    AccountDb = kz_util:format_account_db(AccountId),
    maybe_create_migration_doc(AccountDb),

    {'ok', Doc} = kz_datamgr:open_cache_doc(AccountDb, ?MIGRATIONS_DOC),
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

-spec maybe_create_migration_doc(binary()) -> 'ok'.
maybe_create_migration_doc(Account) ->
    case kz_datamgr:open_cache_doc(Account, ?MIGRATIONS_DOC) of
        {'ok', _} -> 'ok';
        {'error', 'not_found'} ->
            lager:info("creating migrations document for account ~p", [Account]),
            create_migration_doc(Account)
    end.

-spec create_migration_doc(binary()) -> 'ok'.
create_migration_doc(Account) ->
    Update = [{<<"pvt_created">>, kz_time:now_s()}
             ,{<<"migrations_performed">>, []}
             ],
    UpdateOptions = [{'update', Update}
                    ,{'ensure_saved', 'true'}
                    ],
    {'ok', _} = kz_datamgr:update_doc(Account, ?MIGRATIONS_DOC, UpdateOptions),
    lager:debug("created migration doc for account ~s", [Account]).
