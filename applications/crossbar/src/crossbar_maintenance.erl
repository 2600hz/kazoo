%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_maintenance).

-export([migrate/0
        ,migrate_accounts_data/0
        ,migrate_account_data/1
        ,refresh/0, refresh/1
        ,register_views/0
        ,flush/0
        ,update_schemas/0
        ,db_init/0
        ]).

-export([start_module/1
        ,stop_module/1
        ,running_modules/0
        ]).

-export([find_account_by_number/1
        ,find_account_by_name/1
        ,find_account_by_realm/1
        ,find_account_by_id/1
        ]).

-export([enable_account/1, disable_account/1
        ,promote_account/1, demote_account/1
        ,allow_account_number_additions/1, disallow_account_number_additions/1
        ,descendants_count/0, descendants_count/1
        ,create_account/4
        ,move_account/2
        ]).

-export([init_apps/1, init_apps/2
        ,init_app/1, init_app/2
        ,refresh_apps/1, refresh_apps/2
        ,refresh_app/1, refresh_app/2
        ,apps/0
        ,app/1
        ,set_app_field/3
        ,set_app_label/2
        ,set_app_description/2
        ,set_app_extended_description/2
        ,set_app_features/2
        ,set_app_icon/2
        ,set_app_screenshots/2
        ]).

-export([does_schema_exist/1]).

-include("crossbar.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").

-type input_term() :: atom() | string() | kz_term:ne_binary().

-define(DEFAULT_REFRESH_APP_KEYS, [<<"name">>
                                  ,<<"i18n">>
                                  ,<<"tags">>
                                  ,<<"author">>
                                  ,<<"version">>
                                  ,<<"license">>
                                  ,<<"price">>
                                  ,<<"icon">>
                                  ,<<"screenshots">>
                                  ,<<"api_url">>
                                  ,<<"phase">>
                                  ]
       ).
-define(REFRESH_APP_KEYS
       ,kapps_config:get_ne_binaries(?CONFIG_CAT, <<"refresh_app_keys">>, ?DEFAULT_REFRESH_APP_KEYS)
       ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate() -> 'no_return'.
migrate() ->
    migrate(kapps_util:get_all_accounts()).

-spec migrate(kz_term:ne_binaries()) -> 'no_return'.
migrate(Accounts) ->
    _ = migrate_accounts_data(Accounts),

    CurrentModules =
        [kz_term:to_atom(Module, 'true')
         || Module <- crossbar_config:autoload_modules()
        ],

    UpdatedModules = remove_deprecated_modules(CurrentModules, ?DEPRECATED_MODULES),

    add_missing_modules(UpdatedModules
                       ,[Module
                         || Module <- ?DEFAULT_MODULES,
                            (not lists:member(Module, CurrentModules))
                        ]).

-spec remove_deprecated_modules(kz_term:atoms(), kz_term:atoms()) -> kz_term:atoms().
remove_deprecated_modules(Modules, Deprecated) ->
    case lists:foldl(fun lists:delete/2, Modules, Deprecated) of
        Modules -> Modules;
        Ms ->
            ?SUP_LOG_INFO(" removed deprecated modules from autoloaded modules: ~p~n", [Deprecated]),
            {'ok', _} = crossbar_config:set_autoload_modules(Ms),
            Ms
    end.

-spec migrate_accounts_data() -> 'no_return'.
migrate_accounts_data() ->
    migrate_accounts_data(kapps_util:get_all_accounts()).

-spec migrate_accounts_data(kz_term:ne_binaries()) -> 'no_return'.
migrate_accounts_data([]) -> 'no_return';
migrate_accounts_data([Account|Accounts]) ->
    _ = migrate_account_data(Account),
    migrate_accounts_data(Accounts).

-spec migrate_account_data(kz_term:ne_binary()) -> 'no_return'.
migrate_account_data(Account) ->
    _ = cb_clicktocall:maybe_migrate_history(Account),
    _ = cb_vmboxes:migrate(Account),
    _ = cb_lists:maybe_migrate(Account),
    _ = cb_apps_maintenance:migrate(Account),
    'no_return'.

-spec add_missing_modules(kz_term:atoms(), kz_term:atoms()) -> 'no_return'.
add_missing_modules(_, []) -> 'no_return';
add_missing_modules(Modules, MissingModules) ->
    ?SUP_LOG_INFO("  saving autoload_modules with missing modules added: ~p~n", [MissingModules]),
    {'ok', _} = crossbar_config:set_autoload_modules(lists:sort(Modules ++ MissingModules)),
    'no_return'.

%%------------------------------------------------------------------------------
%% @doc
%% @deprecated View refresh functionality is moved to {@link kz_datamgr} and
%% reading from database now, please use {@link kapps_maintenance:refresh/0}.
%% @end
%%------------------------------------------------------------------------------
-spec refresh() -> 'ok'.
refresh() ->
    ?SUP_LOG_INFO("please use kapps_maintenance:refresh().").

%%------------------------------------------------------------------------------
%% @doc
%% @deprecated View refresh functionality is moved to {@link kz_datamgr} and
%% reading from database now, please use {@link kapps_maintenance:refresh/1}.
%% @end
%%------------------------------------------------------------------------------
-spec refresh(input_term()) -> 'ok'.
refresh(Value) ->
    ?SUP_LOG_INFO("please use kapps_maintenance:refresh(~p).", [Value]).

-spec flush() -> 'ok'.
flush() ->
    crossbar_config:flush(),
    kz_cache:flush_local(?CACHE_NAME).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start_module(kz_term:text()) -> 'ok'.
start_module(Module) ->
    case crossbar_init:start_mod(Module) of
        'ok' -> maybe_autoload_module(kz_term:to_binary(Module));
        {'error', Error} -> ?SUP_LOG_INFO("failed to start ~s: ~p~n", [Module, Error])
    end.

-spec maybe_autoload_module(kz_term:ne_binary()) -> 'ok'.
maybe_autoload_module(Module) ->
    Mods = crossbar_config:autoload_modules(),
    case lists:member(Module, Mods) of
        'true' ->
            ?SUP_LOG_INFO("module ~s started~n", [Module]);
        'false' ->
            persist_module(Module, Mods),
            ?SUP_LOG_INFO("started and added ~s to autoloaded modules~n", [Module])
    end.

-spec persist_module(kz_term:ne_binary(), kz_term:ne_binaries()) -> 'ok'.
persist_module(Module, Mods) ->
    {'ok', _} = crossbar_config:set_default_autoload_modules(
                  [kz_term:to_binary(Module)
                   | lists:delete(kz_term:to_binary(Module), Mods)
                  ]),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec stop_module(kz_term:text()) -> 'ok'.
stop_module(Module) ->
    'ok' = crossbar_init:stop_mod(Module),
    Mods = crossbar_config:autoload_modules(),
    {'ok', _} = crossbar_config:set_default_autoload_modules(lists:delete(kz_term:to_binary(Module), Mods)),
    ?SUP_LOG_INFO("stopped and removed ~s from autoloaded modules~n", [Module]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec running_modules() -> kz_term:atoms().
running_modules() -> crossbar_bindings:modules_loaded().

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_account_by_number(input_term()) -> {'ok', kz_term:ne_binary()} |
          {'error', any()}.
find_account_by_number(Number) when not is_binary(Number) ->
    find_account_by_number(kz_term:to_binary(Number));
find_account_by_number(Number) ->
    case knm_numbers:lookup_account(Number) of
        {'ok', AccountId, _} ->
            AccountDb = kzs_util:format_account_db(AccountId),
            print_account_info(AccountDb, AccountId);
        {'error', {'not_in_service', AssignedTo}} ->
            AccountDb = kzs_util:format_account_db(AssignedTo),
            print_account_info(AccountDb, AssignedTo);
        {'error', {'account_disabled', AssignedTo}} ->
            AccountDb = kzs_util:format_account_db(AssignedTo),
            print_account_info(AccountDb, AssignedTo);
        {'error', Reason}=E ->
            ?SUP_LOG_INFO("failed to find account assigned to number '~s': ~p~n", [Number, Reason]),
            E
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_account_by_name(input_term()) ->
          {'ok', kz_term:ne_binary()} |
          {'multiples', [kz_term:ne_binary(),...]} |
          {'error', any()}.
find_account_by_name(Name) when not is_binary(Name) ->
    find_account_by_name(kz_term:to_binary(Name));
find_account_by_name(Name) ->
    case kapps_util:get_accounts_by_name(Name) of
        {'ok', AccountDb} ->
            print_account_info(AccountDb);
        {'multiples', AccountDbs} ->
            AccountIds = [begin
                              {'ok', AccountId} = print_account_info(AccountDb),
                              AccountId
                          end || AccountDb <- AccountDbs
                         ],
            {'multiples', AccountIds};
        {'error', Reason}=E ->
            ?SUP_LOG_INFO("failed to find account: ~p~n", [Reason]),
            E
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_account_by_realm(input_term()) ->
          {'ok', kz_term:ne_binary()} |
          {'multiples', [kz_term:ne_binary(),...]} |
          {'error', any()}.
find_account_by_realm(Realm) when not is_binary(Realm) ->
    find_account_by_realm(kz_term:to_binary(Realm));
find_account_by_realm(Realm) ->
    case kapps_util:get_account_by_realm(Realm) of
        {'ok', AccountDb} ->
            print_account_info(AccountDb);
        {'multiples', AccountDbs} ->
            AccountIds = [begin
                              {'ok', AccountId} = print_account_info(AccountDb),
                              AccountId
                          end || AccountDb <- AccountDbs
                         ],
            {'multiples', AccountIds};
        {'error', Reason}=E ->
            ?SUP_LOG_INFO("failed to find account: ~p~n", [Reason]),
            E
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec find_account_by_id(input_term()) ->
          {'ok', kz_term:ne_binary()} |
          {'error', any()}.
find_account_by_id(Id) when is_binary(Id) ->
    print_account_info(kzs_util:format_account_db(Id));
find_account_by_id(Id) ->
    find_account_by_id(kz_term:to_binary(Id)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec allow_account_number_additions(input_term()) -> 'ok' | 'failed'.
allow_account_number_additions(AccountId) ->
    Update = [{kzd_accounts:path_allow_number_additions(), 'true'}],
    case kzd_accounts:update(AccountId, Update) of
        {'ok', _A} ->
            ?SUP_LOG_INFO("  account ~s allowed to add numbers: ~s~n", [AccountId, kz_json:encode(_A)]);
        {'error', _R} ->
            ?SUP_LOG_INFO("  failed to allow account ~s to add numbers: ~p~n", [AccountId, _R]),
            'failed'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec disallow_account_number_additions(input_term()) -> 'ok' | 'failed'.
disallow_account_number_additions(AccountId) ->
    Update = [{kzd_accounts:path_allow_number_additions(), 'false'}],
    case kzd_accounts:update(AccountId, Update) of
        {'ok', _} -> 'ok';
        {'error', _} -> 'failed'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec enable_account(input_term()) -> 'ok' | 'failed'.
enable_account(AccountId) ->
    Update = [{kzd_accounts:path_enabled(), 'true'}],
    case kzd_accounts:update(AccountId, Update) of
        {'ok', _} -> 'ok';
        {'error', _} -> 'failed'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec disable_account(input_term()) -> 'ok' | 'failed'.
disable_account(AccountId) ->
    Update = [{kzd_accounts:path_enabled(), 'false'}],
    case kzd_accounts:update(AccountId, Update) of
        {'ok', _} -> 'ok';
        {'error', _} -> 'failed'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec promote_account(input_term()) -> 'ok' | 'failed'.
promote_account(AccountId) ->
    Update = [{kzd_accounts:path_superduper_admin(), 'true'}],
    case kzd_accounts:update(AccountId, Update) of
        {'ok', _A} ->
            ?SUP_LOG_INFO("  account ~s is admin-ified: ~s~n", [AccountId, kz_json:encode(_A)]);
        {'error', _R} ->
            ?SUP_LOG_INFO("  failed to admin-ify account ~s: ~p~n", [AccountId, _R]),
            'failed'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec demote_account(input_term()) -> 'ok' | 'failed'.
demote_account(AccountId) ->
    Update = [{kzd_accounts:path_superduper_admin(), 'false'}],
    case kzd_accounts:update(AccountId, Update) of
        {'ok', _} -> 'ok';
        {'error', _} -> 'failed'
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_account(input_term(), input_term(), input_term(), input_term()) -> 'ok' | 'failed'.
create_account(AccountName, Realm, Username, Password)
  when is_binary(AccountName),
       is_binary(Realm),
       is_binary(Username),
       is_binary(Password) ->
    Account = kz_json:set_values([{<<"_id">>, kz_datamgr:get_uuid()}
                                 ,{<<"name">>, AccountName}
                                 ,{<<"realm">>, Realm}
                                 ]
                                ,kzd_accounts:new()
                                ),

    User = kz_json:set_values([{<<"_id">>, kz_datamgr:get_uuid()}
                              ,{<<"username">>, Username}
                              ,{<<"password">>, Password}
                              ,{<<"first_name">>, <<"Account">>}
                              ,{<<"last_name">>, <<"Admin">>}
                              ,{<<"priv_level">>, <<"admin">>}
                              ]
                             ,kzd_users:new()
                             ),

    try create_account_and_user(Account, User) of
        {'ok', _Context} -> 'ok'
    catch
        ?STACKTRACE(Type, Reason, ST)
        log_error(Type, Reason, ST, AccountName)
        end;
create_account(AccountName, Realm, Username, Password) ->
    create_account(kz_term:to_binary(AccountName)
                  ,kz_term:to_binary(Realm)
                  ,kz_term:to_binary(Username)
                  ,kz_term:to_binary(Password)
                  ).

log_error(Type, Reason, ST, AccountName) ->
    ?SUP_LOG_ERROR("crashed creating account: ~s: ~p", [Type, Reason]),
    ?SUP_LOG_ERROR("stacktrace: ~p", [ST]),
    ?SUP_LOG_INFO("failed to create '~s': ~p~n", [AccountName, Reason]),
    'failed'.

-spec maybe_promote_account(cb_context:context()) -> {'ok', cb_context:context()}.
maybe_promote_account(Context) ->
    AccountDb = cb_context:db_name(Context),
    AccountId = cb_context:account_id(Context),

    case kapps_util:get_all_accounts() of
        [AccountDb] ->
            ?SUP_LOG_INFO("account ~s is the first, promoting it to sysadmin", [AccountId]),
            'ok' = promote_account(AccountId),
            'ok' = allow_account_number_additions(AccountId),
            'ok' = kz_services_reseller:force_promote(AccountId),
            'ok' = update_system_config(AccountId),
            ?SUP_LOG_INFO("finished promoting account"),
            {'ok', Context};
        _Else ->
            ?SUP_LOG_DEBUG("account ~s is not the first account in the system", [AccountId]),
            {'ok', Context}
    end.

-spec create_account_and_user(kz_json:object(), kz_json:object()) ->
          {'ok', cb_context:context()}.
create_account_and_user(Account, User) ->
    Funs = [fun prechecks/1
           ,{fun validate_account/2, Account}
           ,fun create_account/1
           ,{fun validate_user/2, User}
           ,fun create_user/1
           ,fun maybe_promote_account/1
           ],
    lists:foldl(fun create_fold/2
               ,{'ok', cb_context:new()}
               ,Funs
               ).

-spec create_fold(fun() | {fun(), kz_json:object()}, {'ok', cb_context:context()}) ->
          {'ok', cb_context:context()}.
create_fold({F, V}, {'ok', C}) -> F(V, C);
create_fold(F, {'ok', C}) -> F(C).

-spec update_system_config(kz_term:ne_binary()) -> 'ok'.
update_system_config(AccountId) ->
    {'ok', _} = kapps_config:set(<<"accounts">>, <<"master_account_id">>, AccountId),
    ?SUP_LOG_INFO("updated master account id in system_config.accounts~n").

-spec prechecks(cb_context:context()) -> {'ok', cb_context:context()}.
prechecks(Context) ->
    Funs = [fun is_crossbar_running/0
           ,fun db_accounts_exists/0
           ,fun db_system_config_exists/0
           ,fun db_system_schemas_exists/0
           ,fun do_schemas_exist/0
           ],
    'true' = lists:all(fun(F) -> F() end, Funs),
    ?SUP_LOG_INFO("prechecks passed"),
    {'ok', Context}.

-spec is_crossbar_running() -> boolean().
is_crossbar_running() ->
    case lists:member('crossbar', kapps_controller:running_apps()) of
        'false' -> start_crossbar();
        'true' -> 'true'
    end.

start_crossbar() ->
    case kapps_controller:start_app('crossbar') of
        {'ok', _} -> 'true';
        {'error', _E} ->
            ?SUP_LOG_INFO("failed to start crossbar: ~p~n", [_E]),
            'false'
    end.

%% technically we don't need to check if accounts db exists
%% since kazoo always checks that during startup.
-spec db_accounts_exists() -> 'true'.
db_accounts_exists() ->
    db_exists(?KZ_ACCOUNTS_DB).

-spec db_system_config_exists() -> 'true'.
db_system_config_exists() ->
    db_exists(?KZ_CONFIG_DB).

-spec db_system_schemas_exists() -> 'true'.
db_system_schemas_exists() ->
    db_exists(?KZ_SCHEMA_DB).

-spec db_exists(kz_term:ne_binary()) -> 'true'.
db_exists(Database) ->
    db_exists(Database, 'true').

-spec db_exists(kz_term:ne_binary(), boolean()) -> 'true'.
db_exists(Database, ShouldRetry) ->
    case kz_datamgr:db_exists(Database) of
        'true' -> 'true';
        'false' when ShouldRetry ->
            ?SUP_LOG_INFO("db '~s' doesn't exist~n", [Database]),
            _ = kapps_maintenance:refresh(Database),
            db_exists(Database, 'false');
        'false' ->
            throw(kz_json:from_list([{<<"error">>, <<"database not ready">>}
                                    ,{<<"database">>, Database}
                                    ])
                 )
    end.

-spec do_schemas_exist() -> boolean().
do_schemas_exist() ->
    Schemas = [<<"users">>
              ,<<"accounts">>
              ,<<"profile">>
              ],
    lists:all(fun does_schema_exist/1, Schemas).

-spec does_schema_exist(kz_term:ne_binary()) -> boolean().
does_schema_exist(Schema) ->
    case kz_json_schema:load(Schema) of
        {'ok', SchemaJObj} -> maybe_load_refs(SchemaJObj);
        {'error', 'not_found'} -> maybe_fload(Schema)
    end.

-spec maybe_fload(kz_term:ne_binary()) -> boolean().
maybe_fload(Schema) ->
    case kz_json_schema:fload(Schema) of
        {'ok', SchemaJObj} ->
            ?SUP_LOG_INFO("schema ~s exists on disk, refreshing in db", [Schema]),
            case kz_datamgr:save_doc(?KZ_SCHEMA_DB, SchemaJObj) of
                {'ok', _} -> 'ok';
                {'error', 'conflict'} -> 'ok'
            end,
            maybe_load_refs(SchemaJObj);
        {'error', _E} ->
            ?SUP_LOG_ERROR("schema ~s not in db or on disk: ~p", [Schema, _E]),
            throw(kz_json:from_list([{<<"error">>, <<"schema ", Schema/binary, " not found">>}
                                    ,{<<"schema">>, Schema}
                                    ])
                 )
    end.

maybe_load_refs(SchemaJObj) ->
    kz_json:all(fun maybe_load_ref/1
               ,kz_json:get_json_value(<<"properties">>, SchemaJObj, kz_json:new())
               ).

maybe_load_ref({_Property, Schema}) ->
    case kz_json:get_ne_binary_value(<<"$ref">>, Schema) of
        'undefined' -> maybe_load_refs(Schema);
        Ref -> does_schema_exist(Ref)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_account(kz_json:object(), cb_context:context()) -> {'ok', cb_context:context()}.
validate_account(JObj, Context) ->
    {Nouns, User} = account_nouns_and_user(),
    Payload = [cb_context:setters(Context
                                 ,[{fun cb_context:set_req_data/2, JObj}
                                  ,{fun cb_context:set_req_nouns/2, [{<<"accounts">>, Nouns}]}
                                  ,{fun cb_context:set_req_verb/2, ?HTTP_PUT}
                                  ,{fun cb_context:set_resp_status/2, 'fatal'}
                                  ,{fun cb_context:set_api_version/2, ?VERSION_2}
                                  ,{fun cb_context:set_auth_doc/2, User}
                                   | case Nouns of
                                         [] -> [];
                                         [Id] -> [{fun cb_context:set_auth_account_id/2, Id}]
                                     end
                                  ])
               | Nouns
              ],
    Context1 = apply('cb_accounts', 'validate', Payload),
    case cb_context:resp_status(Context1) of
        'success' ->
            {'ok', cb_context:set_auth_account_id(Context1, cb_context:account_id(Context1))};
        _Status ->
            {'error', {_Code, _Msg, Errors}} = cb_context:response(Context1),
            AccountId = cb_context:account_id(Context1),
            ?SUP_LOG_INFO("failed to validate account ~s: ~p ~s~n", [AccountId, _Code, _Msg]),
            _ = cb_accounts:delete_account(AccountId),
            throw(Errors)
    end.

account_nouns_and_user() ->
    case account_nouns() of
        [] -> {[], 'undefined'};
        [AccountId] -> {[AccountId], master_admin(AccountId)}
    end.

account_nouns() ->
    case kapps_util:get_master_account_id() of
        {'ok', MasterAccountId} -> [MasterAccountId];
        {'error', _} -> []
    end.

master_admin(MasterAccountId) ->
    case kz_datamgr:get_results(MasterAccountId, <<"users/crossbar_listing">>, []) of
        {'ok', Users} -> find_first_admin(Users);
        {'error', _} -> 'undefined'
    end.

find_first_admin([]) -> 'undefined';
find_first_admin([User|Users]) ->
    case kz_json:get_ne_binary_value([<<"value">>, <<"priv_level">>], User) of
        <<"admin">> -> kz_json:get_json_value(<<"value">>, User);
        _ -> find_first_admin(Users)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec validate_user(kz_json:object(), cb_context:context()) -> {'ok', cb_context:context()}.
validate_user(JObj, Context) ->
    Payload = [cb_context:setters(Context
                                 ,[{fun cb_context:set_req_data/2, JObj}
                                  ,{fun cb_context:set_req_nouns/2, [{<<"users">>, []}
                                                                    ,{<<"accounts">>, [cb_context:account_id(Context)]}
                                                                    ]}
                                  ,{fun cb_context:set_req_verb/2, ?HTTP_PUT}
                                  ,{fun cb_context:set_resp_status/2, 'fatal'}
                                  ,{fun cb_context:set_doc/2, 'undefined'}
                                  ,{fun cb_context:set_resp_data/2, 'undefined'}
                                  ,{fun cb_context:set_db_name/2, 'undefined'}
                                  ,{fun cb_context:set_auth_account_id/2, cb_context:account_id(Context)}
                                  ]
                                 )
              ],
    Context1 = crossbar_bindings:fold(<<"v2_resource.validate.users">>, Payload),
    case cb_context:resp_status(Context1) of
        'success' ->
            {'ok', Context1};
        _Status ->
            {'error', {_Code, _Msg, Errors}} = cb_context:response(Context1),
            ?SUP_LOG_INFO("failed to validate user: ~p ~s~n", [_Code, _Msg]),
            throw(Errors)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_account(cb_context:context()) -> {'ok', cb_context:context()}.
create_account(Context) ->
    Context1 = apply('cb_accounts', 'put', [Context | account_nouns()]),
    AccountId = cb_context:account_id(Context1),
    AccountDb = cb_context:db_name(Context1),
    case cb_context:resp_status(Context1) of
        'success' when AccountId =/= 'undefined' ->
            ?SUP_LOG_INFO("created new account '~s' in db '~s'~n"
                         ,[AccountId, AccountDb]
                         ),
            {'ok', cb_context:set_account_id(Context1, AccountId)};
        'success' ->
            AccountIdFromDb = kzs_util:format_account_id(AccountDb),
            ?SUP_LOG_INFO("created new account '~s' in db '~s'~n"
                         ,[AccountIdFromDb, AccountDb]
                         ),
            {'ok', cb_context:set_account_id(Context1, AccountIdFromDb)};
        _Status ->
            {'error', {_Code, _Msg, Errors}} = cb_context:response(Context1),
            DocAccountId = kz_doc:id(cb_context:req_data(Context)),
            kz_datamgr:db_delete(kzs_util:format_account_db(DocAccountId)),

            ?SUP_LOG_ERROR("failed to create the account ~s: ~p ~s", [DocAccountId, _Code, _Msg]),
            throw(Errors)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec create_user(cb_context:context()) -> {'ok', cb_context:context()}.
create_user(Context) ->
    Context1 = crossbar_bindings:fold(<<"v2_resource.execute.put.users">>, [Context]),
    case cb_context:resp_status(Context1) of
        'success' ->
            ?SUP_LOG_INFO("created new account admin user '~s'~n", [kz_doc:id(cb_context:doc(Context1))]),
            {'ok', Context1};
        _Status ->
            {'error', {_Code, _Msg, Errors}} = cb_context:response(Context1),
            ?SUP_LOG_INFO("failed to create the admin user: ~p ~s", [_Code, _Msg]),
            throw(Errors)
    end.

-spec print_account_info(kz_term:ne_binary()) -> {'ok', kz_term:ne_binary()}.
print_account_info(AccountDb) ->
    AccountId = kzs_util:format_account_id(AccountDb),
    print_account_info(AccountDb, AccountId).

-spec print_account_info(kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok', kz_term:ne_binary()}.
print_account_info(AccountDb, AccountId) ->
    case kz_datamgr:open_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            ?SUP_LOG_INFO("Account ID: ~s (~s)~n", [AccountId, AccountDb]),
            ?SUP_LOG_INFO("  Name: ~s~n", [kzd_accounts:name(JObj)]),
            ?SUP_LOG_INFO("  Realm: ~s~n", [kzd_accounts:realm(JObj)]),
            ?SUP_LOG_INFO("  Enabled: ~s~n", [kzd_accounts:is_enabled(JObj)]),
            ?SUP_LOG_INFO("  System Admin: ~s~n", [kzd_accounts:is_superduper_admin(JObj)]);
        {'error', 'not_found'} ->
            ?SUP_LOG_INFO("Account ID: ~s (~s) does not exist~n", [AccountId, AccountDb]);
        {'error', _} ->
            ?SUP_LOG_INFO("Account ID: ~s (~s)~n", [AccountId, AccountDb])
    end,
    {'ok', AccountId}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec move_account(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
move_account(Account, ToAccount) ->
    AccountId = kzs_util:format_account_id(Account),
    ToAccountId = kzs_util:format_account_id(ToAccount),
    maybe_move_account(AccountId, ToAccountId).

-spec maybe_move_account(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
maybe_move_account(AccountId, AccountId) ->
    ?SUP_LOG_INFO("can not move to the same account~n");
maybe_move_account(AccountId, ToAccountId) ->
    case crossbar_util:move_account(AccountId, ToAccountId) of
        {'ok', _} -> ?SUP_LOG_INFO("move complete!~n");
        {'error', Reason} ->
            ?SUP_LOG_INFO("unable to complete move: ~p~n", [Reason])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec descendants_count() -> 'ok'.
descendants_count() ->
    crossbar_util:descendants_count().

-spec descendants_count(kz_term:ne_binary()) -> 'ok'.
descendants_count(AccountId) ->
    crossbar_util:descendants_count(AccountId).

-spec init_apps(file:name()) -> 'ok'.
init_apps(AppsPath) ->
    init_apps(AppsPath, 'undefined').

-spec init_apps(file:name(), kz_term:api_binary()) -> 'ok'.
init_apps(AppsPath, AppUrl) ->
    Apps = find_apps(AppsPath),
    InitApp = fun(App) -> init_app(App, AppUrl) end,
    lists:foreach(InitApp, Apps).

-spec init_app(file:filename_all()) -> 'ok'.
init_app(AppPath) ->
    init_app(AppPath, 'undefined').

-spec init_app(file:filename_all(), kz_term:api_binary()) -> 'ok'.
init_app(AppPath, AppUrl) ->
    Keys = [<<"api_url">>],
    ?SUP_LOG_INFO("trying to init app from ~s~n", [AppPath]),
    maybe_update_app(AppPath, AppUrl, Keys).

-spec refresh_apps(file:name()) -> 'ok'.
refresh_apps(AppsPath) ->
    refresh_apps(AppsPath, 'undefined').

-spec refresh_apps(file:name(), kz_term:api_binary()) -> 'ok'.
refresh_apps(AppsPath, AppUrl) ->
    Apps = find_apps(AppsPath),
    InitApp = fun(App) -> refresh_app(App, AppUrl) end,
    lists:foreach(InitApp, Apps).

-spec refresh_app(file:filename_all()) -> 'ok'.
refresh_app(AppPath) ->
    refresh_app(AppPath, 'undefined').

-spec refresh_app(file:filename_all(), kz_term:api_binary()) -> 'ok'.
refresh_app(AppPath, AppUrl) ->
    ?SUP_LOG_INFO("trying to refresh app from ~s~n", [AppPath]),
    maybe_update_app(AppPath, AppUrl, ?REFRESH_APP_KEYS).

-spec find_apps(file:name()) -> [file:name()].
find_apps(AppsPath) ->
    AccFun =
        fun(AppJSONPath, Acc) ->
                try
                    ?SUP_LOG_DEBUG("find...~p", [AppJSONPath]),
                    App = AppJSONPath, %%filename:absname(AppJSONPath),
                    %% /.../App/metadata/app.json --> App
                    [filename:dirname(filename:dirname(App)) | Acc]
                catch
                    _Ex:_Er:_ST ->
                        kz_log:log_stacktrace(_ST),
                        Acc
                end
        end,
    filelib:fold_files(AppsPath, "app\\.json", 'true', AccFun, []).

-spec maybe_update_app(file:filename_all(), kz_term:api_binary(), kz_term:ne_binaries()) -> 'ok'.
maybe_update_app(AppPath, AppUrl, Keys) ->
    try find_metadata(AppPath) of
        {'ok', MetaData} ->
            maybe_create_app(AppPath, maybe_set_api_url(AppUrl, MetaData), Keys);
        {'invalid_data', _E} ->
            ?SUP_LOG_INFO("  failed to validate app data ~s: ~p~n", [AppPath, _E])
    catch
        'error':{'badmatch', {'error', 'enoent'}} ->
            ?SUP_LOG_INFO("  failed to incorporate app because there was no app.json in ~s~n"
                         ,[filename:join([AppPath, <<"metadata">>])]);
        'error':_E ->
            ?SUP_LOG_INFO("  failed to find metadata in ~s: ~p~n", [AppPath, _E])
    end.

-spec maybe_set_api_url(kz_term:api_binary(), kz_json:object()) -> kz_json:object().
maybe_set_api_url('undefined', MetaData) ->
    kz_json:delete_key(<<"api_url">>, MetaData);
maybe_set_api_url(AppUrl, MetaData) ->
    kz_json:set_value(<<"api_url">>, AppUrl, MetaData).

-spec maybe_create_app(file:filename_all(), kz_json:object(), kz_term:ne_binaries()) -> 'ok'.
maybe_create_app(AppPath, MetaData, Keys) ->
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
    maybe_create_app(AppPath, MetaData, Keys, MasterAccountDb).

-spec maybe_create_app(file:filename_all(), kz_json:object(), kz_term:ne_binaries(), kz_term:ne_binary()) -> 'ok'.
maybe_create_app(AppPath, MetaData, Keys, MasterAccountDb) ->
    AppName = kzd_app:name(MetaData),
    case find_app(MasterAccountDb, AppName) of
        {'ok', JObj} ->
            ?SUP_LOG_INFO(" app ~s already loaded in system~n", [AppName]),
            AppJObj = kz_json:get_json_value(<<"doc">>, JObj),
            maybe_update_app(AppPath, MetaData, Keys, MasterAccountDb, AppJObj);
        {'error', 'not_found'} -> create_app(AppPath, MetaData, MasterAccountDb);
        {'error', _E} -> ?SUP_LOG_INFO(" failed to find app ~s: ~p", [AppName, _E])
    end.

-spec create_app(file:filename_all(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
create_app(AppPath, MetaData, MasterAccountDb) ->
    Doc0 = kz_doc:update_pvt_parameters(MetaData, MasterAccountDb, [{'type', <<"app">>}]),
    Doc = kz_json:delete_key(<<"source_url">>, Doc0),
    case kz_datamgr:save_doc(MasterAccountDb, Doc) of
        {'ok', AppJObj} ->
            AppId = kz_doc:id(AppJObj),
            ?SUP_LOG_INFO(" saved app ~s as doc ~s~n", [kzd_app:name(AppJObj), AppId]),
            maybe_add_images(AppPath, AppId, MetaData, MasterAccountDb);
        {'error', _E} ->
            ?SUP_LOG_INFO(" failed to save app ~s to ~s: ~p~n"
                         ,[kzd_app:name(MetaData), MasterAccountDb, _E])
    end.

-spec maybe_update_app(file:filename_all(), kz_json:object(), kz_term:ne_binaries(), kz_term:ne_binary(), kz_json:object()) -> 'no_return'.
maybe_update_app(AppPath, MetaData, Keys, MasterAccountDb, AppJObj) ->
    CurrentDocId = kz_doc:id(AppJObj),
    case update_app_updates(Keys, MetaData, AppJObj) of
        [] -> ?SUP_LOG_INFO(" no metadata changes for app ~s~n", [CurrentDocId]);
        Updates -> save_app_updates(MasterAccountDb, CurrentDocId, AppJObj, Updates)
    end,
    'ok' = delete_old_images(CurrentDocId, MetaData, MasterAccountDb),
    maybe_add_images(AppPath, CurrentDocId, MetaData, MasterAccountDb).

-spec save_app_updates(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), kz_term:proplist()) -> 'ok'.
save_app_updates(MasterAccountDb, CurrentDocId, AppJObj, Updates) ->
    case kz_datamgr:save_doc(MasterAccountDb, kz_json:set_values(Updates, AppJObj)) of
        {'ok', NewAppJObj} ->
            AppName = kzd_app:name(NewAppJObj),
            ?SUP_LOG_INFO(" updated ~s app ~s metadata~n", [AppName, CurrentDocId]);
        {'error', Err} ->
            ?SUP_LOG_INFO(" error updating app ~s metadata: ~p~n", [CurrentDocId, Err])
    end.

-spec update_app_updates(kz_term:ne_binaries(), kz_json:object(), kz_json:object()) -> kz_term:proplist().
update_app_updates(Keys, MetaData, AppJObj) ->
    update_app_updates(Keys, MetaData, AppJObj, []).

-spec update_app_updates(kz_term:ne_binaries(), kz_json:object(), kz_json:object(), kz_term:proplist()) -> kz_term:proplist().
update_app_updates([], _, _, Props) -> Props;
update_app_updates([Key|Keys], MetaData, AppJObj, Props) ->
    CurrentValue = kz_json:get_ne_value(Key, AppJObj),
    NewValue = kz_json:get_ne_value(Key, MetaData),
    case update_app_values_differ(CurrentValue, NewValue) of
        'false' ->
            ?SUP_LOG_INFO(" not updating ~s, it is unchanged~n", [Key]),
            update_app_updates(Keys, MetaData, AppJObj, Props);
        'true' ->
            Updates = [{Key, NewValue} | Props],
            ?SUP_LOG_INFO(" preparing to update ~s~n", [Key]),
            update_app_updates(Keys, MetaData, AppJObj, Updates)
    end.

-spec update_app_values_differ(kz_json:json_term(), kz_json:json_term()) -> boolean().
update_app_values_differ(_, 'undefined') -> 'false';
update_app_values_differ('undefined', _) -> 'true';
update_app_values_differ(CurrentValue, NewValue) ->
    case kz_json:are_json_objects([CurrentValue, NewValue]) of
        'true' -> not kz_json:are_equal(CurrentValue, NewValue);
        'false' -> CurrentValue =/= NewValue
    end.

-spec delete_old_images(kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
delete_old_images(AppId, MetaData, MasterAccountDb) ->
    F = fun (X) -> safe_delete_image(MasterAccountDb, AppId, X) end,
    lists:foreach(F, [kzd_app:icon(MetaData)]),
    lists:foreach(F, kzd_app:screenshots(MetaData)).

-spec safe_delete_image(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_binary()) -> 'ok'.
safe_delete_image(_AccountDb, _AppId, 'undefined') -> 'ok';
safe_delete_image(AccountDb, AppId, Image) ->
    case kz_datamgr:fetch_attachment(AccountDb, AppId, Image) of
        {'error', _} -> 'ok';
        {'ok', _} ->
            kz_datamgr:delete_attachment(AccountDb, AppId, Image)
    end.

-spec maybe_add_images(file:filename_all(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> no_return.
maybe_add_images(AppPath, ?NE_BINARY=AppId, MetaData, MasterAccountDb) ->
    Icon = kzd_app:icon(MetaData),
    Screenshots = kzd_app:screenshots(MetaData),
    IconPath = {Icon, filename:join([AppPath, <<"metadata">>, <<"icon">>, Icon])},
    SShotPaths = [{SShot, filename:join([AppPath, <<"metadata">>, <<"screenshots">>, SShot])}
                  || SShot <- Screenshots
                 ],
    update_icon(AppId, MasterAccountDb, IconPath),
    update_screenshots(AppId, MasterAccountDb, SShotPaths).

update_icon(AppId, MA, IconPath) ->
    update_images(AppId, MA, [IconPath], <<"icon">>),
    'no_return'.

update_screenshots(AppId, MA, SShotPaths) ->
    update_images(AppId, MA, SShotPaths, <<"screenshots">>),
    'no_return'.

-type image_path() :: {file:filename_all(), file:filename_all()}.
-type image_paths() :: [image_path()].

-spec update_images(kz_term:ne_binary(), kz_term:ne_binary(), image_paths(), kz_term:ne_binary()) -> 'ok'.
update_images(AppId, MasterAccountDb, ImagePaths, Type) ->
    try read_images(ImagePaths) of
        {'ok', Images} -> add_images(AppId, MasterAccountDb, Images)
    catch
        'error':{'badmatch', {'error', 'enoent'}} ->
            ?SUP_LOG_INFO("  failed to find ~s in ~s~n", [Type, AppId]);
        'error':_E ->
            ?SUP_LOG_INFO("  failed to load ~s in ~s: ~p~n", [Type, AppId, _E])
    end.

-spec add_images(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
add_images(AppId, MasterAccountDb, Images) ->
    _ = [add_image(AppId, MasterAccountDb, ImageId, ImageData)
         || {ImageId, ImageData} <- Images
        ],
    'ok'.

-spec add_image(kz_term:ne_binary(), kz_term:ne_binary(), file:filename_all(), binary()) -> 'ok'.
add_image(AppId, MasterAccountDb, ImageId, ImageData) ->
    case kz_datamgr:put_attachment(MasterAccountDb, AppId, ImageId, ImageData) of
        {'ok', _} ->     ?SUP_LOG_INFO("   saved ~s to ~s~n", [ImageId, AppId]);
        {'error', _E} -> ?SUP_LOG_INFO("   failed to save ~s to ~s: ~p~n", [ImageId, AppId, _E])
    end.

-spec read_images(image_paths()) ->
          {'ok', [{file:filename_all(), binary()}]}.
read_images(Images) ->
    {'ok', [{Image, read_image(ImagePath)}
            || {Image, ImagePath} <- Images
           ]}.

-spec read_image(file:filename_all()) -> binary().
read_image(File) ->
    {'ok', ImageData} = file:read_file(File),
    ImageData.

-spec find_metadata(file:filename_all()) ->
          {'ok', kz_json:object()} |
          {'invalid_data', kz_term:proplist()}.
find_metadata(AppPath) ->
    {'ok', Bin} = file:read_file(filename:join([AppPath, <<"metadata">>, <<"app.json">>])),
    JSON = kz_json:decode(Bin),
    case kz_json_schema:validate(<<"app">>, kz_doc:public_fields(JSON)) of
        {'ok', _} -> {'ok', JSON};
        {'error', Errors} ->
            {'invalid_data', [Error || {'data_invalid', _, Error, _, _} <- Errors]}
    end.

-spec apps() -> 'no_return'.
apps() ->
    {'ok', MA} = kapps_util:get_master_account_db(),
    case kz_datamgr:get_results(MA, ?CB_APPS_STORE_LIST) of
        {'error', _R} -> ?SUP_LOG_DEBUG("failed to read apps in ~s: ~p", [MA, _R]);
        {'ok', JObjs} -> lists:foreach(fun print_app/1, JObjs)
    end,
    'no_return'.

-spec app(kz_term:ne_binary()) -> 'no_return'.
app(AppNameOrId) ->
    {'ok', MA} = kapps_util:get_master_account_db(),
    case find_app(MA, AppNameOrId) of
        {'ok', AppJObj} -> print_app(AppJObj);
        {'error', _} ->
            case kz_datamgr:open_doc(MA, AppNameOrId) of
                {'ok', AppJObj} -> print_app(view_app(AppJObj));
                _ -> ?SUP_LOG_INFO("unknown app\n"), 'no_return'
            end
    end.

-spec find_app(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          kz_datamgr:data_error() |
          {'error', 'multiple_results'}.
find_app(Db, Name) ->
    ViewOptions = [{'key', Name}
                  ,'include_docs'
                  ],
    kz_datamgr:get_single_result(Db, ?CB_APPS_STORE_LIST, ViewOptions).

view_app(AppJObj) ->
    M = maps:with([<<"name">>, <<"id">>, <<"phase">>, <<"i18n">>
                  ,<<"tags">>, <<"api_url">>, <<"source_url">>, <<"published">>
                  ]
                 ,kz_json:to_map(AppJObj)
                 ),
    kz_json:from_list([{<<"key">>, kzd_app:name(AppJObj)}
                      ,{<<"value">>, kz_json:from_map(M)}
                      ]).

print_app(AppJObj) ->
    ?SUP_LOG_INFO("App ~s\n", [kz_json:get_ne_value(<<"key">>, AppJObj)]),
    _ = put('pp_lvl', 1),
    _ = print_kvs(kz_json:get_json_value(<<"value">>, AppJObj)),
    ?SUP_LOG_INFO("\n"),
    'no_return'.

print_kvs(JObj) -> kz_json:foreach(fun print_k_v/1, JObj).

print_k_v({K, V}) ->
    Lvl = get('pp_lvl'),
    Indent = string:copies("  ", Lvl),
    case kz_json:is_json_object(V) of
        'false' -> ?SUP_LOG_INFO("~s~s: ~s\n", [Indent, K, kz_json:encode(V)]);
        'true' ->
            ?SUP_LOG_INFO("~s~s:\n", [Indent, K]),
            _ = put('pp_lvl', Lvl + 1),
            _ = print_kvs(V),
            _ = put('pp_lvl', Lvl)
    end.

update_app(AppId, Path, Value) ->
    case lists:last(Path) of
        <<"_id">> -> 'ok';
        <<"pvt_", _/binary>> -> 'ok';
        ?NE_BINARY ->
            {'ok', MA} = kapps_util:get_master_account_db(),
            Updates = [{Path, Value}],
            UpdateOptions = [{'update', Updates}],
            case kz_datamgr:update_doc(MA, AppId, UpdateOptions) of
                {'ok', _} -> app(AppId);
                {'error', _R} -> ?SUP_LOG_INFO("updating ~s failed: ~p\n", [AppId, _R])
            end
    end,
    'no_return'.

-spec set_app_field(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
set_app_field(AppId, Field, Value) ->
    update_app(AppId, [Field], Value).

-spec set_app_label(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
set_app_label(AppId, Value) ->
    update_app(AppId, [<<"i18n">>, <<"en-US">>, <<"label">>], Value).

-spec set_app_description(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
set_app_description(AppId, Value) ->
    update_app(AppId, [<<"i18n">>, <<"en-US">>, <<"description">>], Value).

-spec set_app_extended_description(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
set_app_extended_description(AppId, Value) ->
    update_app(AppId, [<<"i18n">>, <<"en-US">>, <<"extended_description">>], Value).

-spec set_app_features(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
set_app_features(AppId, Value) ->
    Values = [V || V <- binary:split(Value, <<$@>>, ['global']),
                   V =/= <<>>
             ],
    update_app(AppId, [<<"i18n">>, <<"en-US">>, <<"features">>], Values).

-spec set_app_icon(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
set_app_icon(AppId, PathToPNGIcon) ->
    {'ok', MA} = kapps_util:get_master_account_db(),
    ?SUP_LOG_INFO("Processing...\n"),
    Icon = {filename:basename(PathToPNGIcon), PathToPNGIcon},
    update_icon(AppId, MA, Icon).

-spec set_app_screenshots(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
set_app_screenshots(AppId, PathToScreenshotsFolder) ->
    {'ok', MA} = kapps_util:get_master_account_db(),
    ?SUP_LOG_INFO("processing...\n"),
    SShots = [{filename:basename(SShot), SShot}
              || SShot <- filelib:wildcard(kz_term:to_list(PathToScreenshotsFolder) ++ "/*.png")
             ],
    update_screenshots(AppId, MA, SShots).

%%------------------------------------------------------------------------------
%% @doc Updates system schemas using files in Crossbar `priv' folder during
%% start up.
%%
%% This is called by {@link db_init/0} during Crossbar start up.
%% @end
%%------------------------------------------------------------------------------
-spec update_schemas() -> 'ok'.
update_schemas() ->
    ?SUP_LOG_WARNING("starting system schemas update"),
    kz_datamgr:revise_docs_from_folder(?KZ_SCHEMA_DB, ?APP, <<"schemas">>),
    ?SUP_LOG_WARNING("finished system schemas update").

%%------------------------------------------------------------------------------
%% @doc Updates system schemas using files in Crossbar `priv' folder during
%% start up, and validate them.
%%
%% This is called by {@link db_init/0} during Crossbar start up.
%%
%% @see update_schemas/0
%% @see check_system_configs/0
%% @end
%%------------------------------------------------------------------------------
-spec db_init_schemas() -> 'ok'.
db_init_schemas() ->
    kz_datamgr:suppress_change_notice(),
    update_schemas(),
    kz_datamgr:enable_change_notice(),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Updating system schemas.
%%
%% This is called as part of system/crossbar start up to update system_schema,
%% if any system schema is different from their counterpart in
%% `crossbar/priv/couchdb/schemas/*' it will be updated.
%%
%% Keep in mind that this function is only read from view definitions from
%% `system_data' database only
%%
%% If you only need to update system schemas in runtime, e.g. during developing,
%% use {@link update_schemas/0}.
%%
%% @see update_schemas/0
%% @see check_system_configs/0
%% @end
%%------------------------------------------------------------------------------
-spec db_init() -> 'ok'.
db_init() ->
    _ = kz_process:spawn(fun db_init_schemas/0),
    'ok'.

-spec register_views() -> 'ok'.
register_views() ->
    ?SUP_LOG_WARNING("crossbar register_views()"),
    kz_datamgr:register_views_from_folder('crossbar').
