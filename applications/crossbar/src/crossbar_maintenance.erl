%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(crossbar_maintenance).

-export([migrate/0
        ,migrate/1
        ,migrate_accounts_data/0
        ,migrate_account_data/1
        ]).

-export([start_module/1]).
-export([stop_module/1]).
-export([running_modules/0]).
-export([refresh/0, refresh/1
        ,flush/0
        ]).
-export([find_account_by_number/1]).
-export([find_account_by_name/1]).
-export([find_account_by_realm/1]).
-export([find_account_by_id/1]).
-export([enable_account/1, disable_account/1]).
-export([promote_account/1, demote_account/1]).
-export([allow_account_number_additions/1, disallow_account_number_additions/1]).
-export([create_account/4]).
-export([create_account/1]).
-export([move_account/2]).
-export([descendants_count/0, descendants_count/1]).
-export([migrate_ring_group_callflow/1]).

-export([init_apps/2, init_app/2]).
-export([init_apps/1, init_app/1]).
-export([apps/0, app/1
        ,set_app_field/3%, set_app_field/4, set_app_field/5
        ,set_app_label/2
        ,set_app_description/2
        ,set_app_extended_description/2
        ,set_app_features/2
        ,set_app_icon/2
        ,set_app_screenshots/2
        ]).

-include("crossbar.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").

-type input_term() :: atom() | string() | ne_binary().

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec migrate() -> 'no_return'.
migrate() ->
    migrate(kapps_util:get_all_accounts()).

-spec migrate(ne_binaries()) -> 'no_return'.
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

-spec remove_deprecated_modules(atoms(), atoms()) -> atoms().
remove_deprecated_modules(Modules, Deprecated) ->
    case lists:foldl(fun lists:delete/2, Modules, Deprecated) of
        Modules -> Modules;
        Ms ->
            io:format(" removed deprecated modules from autoloaded modules: ~p~n", [Deprecated]),
            crossbar_config:set_autoload_modules(Ms),
            Ms
    end.

-spec migrate_accounts_data() -> 'no_return'.
migrate_accounts_data() ->
    migrate_accounts_data(kapps_util:get_all_accounts()).

-spec migrate_accounts_data(ne_binaries()) -> 'no_return'.
migrate_accounts_data([]) -> 'no_return';
migrate_accounts_data([Account|Accounts]) ->
    _ = migrate_account_data(Account),
    migrate_accounts_data(Accounts).

-spec migrate_account_data(ne_binary()) -> 'no_return'.
migrate_account_data(Account) ->
    _ = cb_clicktocall:maybe_migrate_history(Account),
    _ = migrate_ring_group_callflow(Account),
    _ = cb_vmboxes:migrate(Account),
    _ = cb_lists_v2:maybe_migrate(Account),
    _ = cb_apps_maintenance:migrate(Account),
    'no_return'.

-spec add_missing_modules(atoms(), atoms()) -> 'no_return'.
add_missing_modules(_, []) -> 'no_return';
add_missing_modules(Modules, MissingModules) ->
    io:format("  saving autoload_modules with missing modules added: ~p~n", [MissingModules]),
    crossbar_config:set_autoload_modules(lists:sort(Modules ++ MissingModules)),
    'no_return'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec refresh() -> 'ok'.
-spec refresh(input_term()) -> 'ok'.

refresh() ->
    io:format("please use kapps_maintenance:refresh().").

refresh(Value) ->
    io:format("please use kapps_maintenance:refresh(~p).", [Value]).

-spec flush() -> 'ok'.
flush() ->
    crossbar_config:flush(),
    kz_cache:flush_local(?CACHE_NAME).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec start_module(text()) -> 'ok'.
start_module(Module) ->
    case crossbar_init:start_mod(Module) of
        'ok' -> maybe_autoload_module(kz_term:to_binary(Module));
        {'error', Error} -> io:format("failed to start ~s: ~p~n", [Module, Error])
    end.

-spec maybe_autoload_module(ne_binary()) -> 'ok'.
maybe_autoload_module(Module) ->
    Mods = crossbar_config:autoload_modules(),
    case lists:member(Module, Mods) of
        'true' ->
            io:format("module ~s started~n", [Module]);
        'false' ->
            persist_module(Module, Mods),
            io:format("started and added ~s to autoloaded modules~n", [Module])
    end.

-spec persist_module(ne_binary(), ne_binaries()) -> 'ok'.
persist_module(Module, Mods) ->
    crossbar_config:set_default_autoload_modules(
      [kz_term:to_binary(Module)
       | lists:delete(kz_term:to_binary(Module), Mods)
      ]),
    'ok'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_module(text()) -> 'ok'.
stop_module(Module) ->
    'ok' = crossbar_init:stop_mod(Module),
    Mods = crossbar_config:autoload_modules(),
    crossbar_config:set_default_autoload_modules(lists:delete(kz_term:to_binary(Module), Mods)),
    io:format("stopped and removed ~s from autoloaded modules~n", [Module]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec running_modules() -> atoms().
running_modules() -> crossbar_bindings:modules_loaded().

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_account_by_number(input_term()) -> {'ok', ne_binary()} |
                                              {'error', any()}.
find_account_by_number(Number) when not is_binary(Number) ->
    find_account_by_number(kz_term:to_binary(Number));
find_account_by_number(Number) ->
    case knm_number:lookup_account(Number) of
        {'ok', AccountId, _} ->
            AccountDb = kz_util:format_account_db(AccountId),
            print_account_info(AccountDb, AccountId);
        {'error', {'not_in_service', AssignedTo}} ->
            AccountDb = kz_util:format_account_db(AssignedTo),
            print_account_info(AccountDb, AssignedTo);
        {'error', {'account_disabled', AssignedTo}} ->
            AccountDb = kz_util:format_account_db(AssignedTo),
            print_account_info(AccountDb, AssignedTo);
        {'error', Reason}=E ->
            io:format("failed to find account assigned to number '~s': ~p~n", [Number, Reason]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_account_by_name(input_term()) ->
                                  {'ok', ne_binary()} |
                                  {'multiples', [ne_binary(),...]} |
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
            io:format("failed to find account: ~p~n", [Reason]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_account_by_realm(input_term()) ->
                                   {'ok', ne_binary()} |
                                   {'multiples', [ne_binary(),...]} |
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
            io:format("failed to find account: ~p~n", [Reason]),
            E
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec find_account_by_id(input_term()) ->
                                {'ok', ne_binary()} |
                                {'error', any()}.
find_account_by_id(Id) when is_binary(Id) ->
    print_account_info(kz_util:format_account_id(Id, 'encoded'));
find_account_by_id(Id) ->
    find_account_by_id(kz_term:to_binary(Id)).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec allow_account_number_additions(input_term()) -> 'ok' | 'failed'.
allow_account_number_additions(AccountId) ->
    case kz_util:set_allow_number_additions(AccountId, 'true') of
        {'ok', _} -> 'ok';
        {'error', _} -> 'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec disallow_account_number_additions(input_term()) -> 'ok' | 'failed'.
disallow_account_number_additions(AccountId) ->
    case kz_util:set_allow_number_additions(AccountId, 'false') of
        {'ok', _} -> 'ok';
        {'error', _} -> 'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec enable_account(input_term()) -> 'ok' | 'failed'.
enable_account(AccountId) ->
    case kz_util:enable_account(AccountId) of
        {'ok', _} -> 'ok';
        {'error', _} -> 'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec disable_account(input_term()) -> 'ok' | 'failed'.
disable_account(AccountId) ->
    case kz_util:disable_account(AccountId) of
        {'ok', _} -> 'ok';
        {'error', _} -> 'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec promote_account(input_term()) -> 'ok' | 'failed'.
promote_account(AccountId) ->
    case kz_util:set_superduper_admin(AccountId, 'true') of
        {'ok', _} -> 'ok';
        {'error', _} -> 'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec demote_account(input_term()) -> 'ok' | 'failed'.
demote_account(AccountId) ->
    case kz_util:set_superduper_admin(AccountId, 'false') of
        {'ok', _} -> 'ok';
        {'error', _} -> 'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_account(input_term(), input_term(), input_term(), input_term()) -> 'ok' | 'failed'.
create_account(AccountName, Realm, Username, Password)
  when is_binary(AccountName), is_binary(Realm), is_binary(Username), is_binary(Password) ->
    Account = kz_json:from_list([{<<"_id">>, kz_datamgr:get_uuid()}
                                ,{<<"name">>, AccountName}
                                ,{<<"realm">>, Realm}
                                ]),

    User = kz_json:from_list([{<<"_id">>, kz_datamgr:get_uuid()}
                             ,{<<"username">>, Username}
                             ,{<<"password">>, Password}
                             ,{<<"first_name">>, <<"Account">>}
                             ,{<<"last_name">>, <<"Admin">>}
                             ,{<<"priv_level">>, <<"admin">>}
                             ]),

    try create_account_and_user(Account, User) of
        {'ok', _Context} -> 'ok'
    catch
        'throw':Errors ->
            io:format("failed to create '~s': ~s~n", [AccountName, kz_json:encode(Errors)]),
            lager:error("errors thrown when creating account: ~s", [kz_json:encode(Errors)]),
            'failed';
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:error("crashed creating account: ~s: ~p", [_E, _R]),
            kz_util:log_stacktrace(ST),

            io:format("failed to create '~s': ~p~n", [AccountName, _R]),
            'failed'
    end;
create_account(AccountName, Realm, Username, Password) ->
    create_account(kz_term:to_binary(AccountName)
                  ,kz_term:to_binary(Realm)
                  ,kz_term:to_binary(Username)
                  ,kz_term:to_binary(Password)
                  ).

-spec maybe_promote_account(cb_context:context()) -> {'ok', cb_context:context()}.
maybe_promote_account(Context) ->
    AccountDb = cb_context:account_db(Context),
    AccountId = cb_context:account_id(Context),

    case kapps_util:get_all_accounts() of
        [AccountDb] ->
            'ok' = promote_account(AccountId),
            'ok' = allow_account_number_additions(AccountId),
            'ok' = whs_account_conversion:force_promote(AccountId),
            'ok' = update_system_config(AccountId),
            {'ok', Context};
        _Else -> {'ok', Context}
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

-spec update_system_config(ne_binary()) -> 'ok'.
update_system_config(AccountId) ->
    kapps_config:set(?KZ_SYSTEM_CONFIG_ACCOUNT, <<"master_account_id">>, AccountId),
    io:format("updated master account id in system_config.~s~n", [?KZ_SYSTEM_CONFIG_ACCOUNT]).

-spec prechecks(cb_context:context()) -> {'ok', cb_context:context()}.
prechecks(Context) ->
    Funs = [fun db_accounts_exists/0
           ,fun db_system_config_exists/0
           ,fun db_system_schemas_exists/0
           ],
    'true' = lists:all(fun(F) -> F() end, Funs),
    {'ok', Context}.

-spec db_accounts_exists() -> 'true'.
db_accounts_exists() ->
    db_exists(?KZ_ACCOUNTS_DB).

-spec db_system_config_exists() -> 'true'.
db_system_config_exists() ->
    db_exists(?KZ_CONFIG_DB).

-spec db_system_schemas_exists() -> 'true'.
db_system_schemas_exists() ->
    db_exists(?KZ_SCHEMA_DB).

-spec db_exists(ne_binary()) -> 'true'.
-spec db_exists(ne_binary(), boolean()) -> 'true'.
db_exists(Database) ->
    db_exists(Database, 'true').

db_exists(Database, ShouldRetry) ->
    case kz_datamgr:db_exists(Database) of
        'true' -> 'true';
        'false' when ShouldRetry ->
            io:format("db '~s' doesn't exist~n", [Database]),
            kapps_maintenance:refresh(Database),
            db_exists(Database, 'false');
        'false' ->
            throw(kz_json:from_list([{<<"error">>, <<"database not ready">>}
                                    ,{<<"database">>, Database}
                                    ])
                 )
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_account(kz_json:object(), cb_context:context()) -> {'ok', cb_context:context()}.
validate_account(JObj, Context) ->
    Payload = [cb_context:setters(Context
                                 ,[{fun cb_context:set_req_data/2, JObj}
                                  ,{fun cb_context:set_req_nouns/2, [{<<"accounts">>, []}]}
                                  ,{fun cb_context:set_req_verb/2, ?HTTP_PUT}
                                  ,{fun cb_context:set_resp_status/2, 'fatal'}
                                  ,{fun cb_context:set_api_version/2, ?VERSION_2}
                                  ])
              ],
    Context1 = crossbar_bindings:fold(<<"v2_resource.validate.accounts">>, Payload),
    case cb_context:resp_status(Context1) of
        'success' ->
            {'ok', Context1};
        _Status ->
            {'error', {_Code, _Msg, Errors}} = cb_context:response(Context1),
            io:format("failed to validate account: ~p ~s~n", [_Code, _Msg]),
            throw(Errors)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
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
                                  ]
                                 )
              ],
    Context1 = crossbar_bindings:fold(<<"v2_resource.validate.users">>, Payload),
    case cb_context:resp_status(Context1) of
        'success' -> {'ok', Context1};
        _Status ->
            {'error', {_Code, _Msg, Errors}} = cb_context:response(Context1),
            io:format("failed to validate user: ~p ~s~n", [_Code, _Msg]),
            throw(Errors)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_account(cb_context:context()) -> {'ok', cb_context:context()}.
create_account(Context) ->
    Context1 = crossbar_bindings:fold(<<"v2_resource.execute.put.accounts">>, [Context]),
    case cb_context:resp_status(Context1) of
        'success' ->
            io:format("created new account '~s' in db '~s'~n"
                     ,[cb_context:account_id(Context1)
                      ,cb_context:account_db(Context1)
                      ]
                     ),
            {'ok', Context1};
        _Status ->
            {'error', {_Code, _Msg, Errors}} = cb_context:response(Context1),
            AccountId = kz_doc:id(cb_context:req_data(Context)),
            kz_datamgr:db_delete(kz_util:format_account_db(AccountId)),

            io:format("failed to create the account: ~p ~s", [_Code, _Msg]),
            throw(Errors)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_user(cb_context:context()) -> {'ok', cb_context:context()}.
create_user(Context) ->
    Context1 = crossbar_bindings:fold(<<"v2_resource.execute.put.users">>, [Context]),
    case cb_context:resp_status(Context1) of
        'success' ->
            io:format("created new account admin user '~s'~n", [kz_doc:id(cb_context:doc(Context1))]),
            {'ok', Context1};
        _Status ->
            {'error', {_Code, _Msg, Errors}} = cb_context:response(Context1),
            io:format("failed to create the admin user: ~p ~s", [_Code, _Msg]),
            throw(Errors)
    end.

-spec print_account_info(ne_binary()) -> {'ok', ne_binary()}.
-spec print_account_info(ne_binary(), ne_binary()) -> {'ok', ne_binary()}.
print_account_info(AccountDb) ->
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),
    print_account_info(AccountDb, AccountId).
print_account_info(AccountDb, AccountId) ->
    case kz_datamgr:open_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            io:format("Account ID: ~s (~s)~n", [AccountId, AccountDb]),
            io:format("  Name: ~s~n", [kz_account:name(JObj)]),
            io:format("  Realm: ~s~n", [kz_account:realm(JObj)]),
            io:format("  Enabled: ~s~n", [kz_account:is_enabled(JObj)]),
            io:format("  System Admin: ~s~n", [kz_account:is_superduper_admin(JObj)]);
        {'error', 'not_found'} ->
            io:format("Account ID: ~s (~s) does not exist~n", [AccountId, AccountDb]);
        {'error', _} ->
            io:format("Account ID: ~s (~s)~n", [AccountId, AccountDb])
    end,
    {'ok', AccountId}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec move_account(ne_binary(), ne_binary()) -> 'ok'.
move_account(Account, ToAccount) ->
    AccountId = kz_util:format_account_id(Account, 'raw'),
    ToAccountId = kz_util:format_account_id(ToAccount, 'raw'),
    maybe_move_account(AccountId, ToAccountId).

-spec maybe_move_account(ne_binary(), ne_binary()) -> 'ok'.
maybe_move_account(AccountId, AccountId) ->
    io:format("can not move to the same account~n");
maybe_move_account(AccountId, ToAccountId) ->
    case crossbar_util:move_account(AccountId, ToAccountId) of
        {'ok', _} -> io:format("move complete!~n");
        {'error', Reason} ->
            io:format("unable to complete move: ~p~n", [Reason])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec descendants_count() -> 'ok'.
-spec descendants_count(ne_binary()) -> 'ok'.
descendants_count() ->
    crossbar_util:descendants_count().

descendants_count(AccountId) ->
    crossbar_util:descendants_count(AccountId).

-spec migrate_ring_group_callflow(ne_binary()) -> 'ok'.
migrate_ring_group_callflow(Account) ->
    lists:foreach(fun create_new_ring_group_callflow/1
                 ,get_migrateable_ring_group_callflows(Account)
                 ).

-spec get_migrateable_ring_group_callflows(ne_binary()) -> kz_json:objects().
get_migrateable_ring_group_callflows(Account) ->
    AccountDb = kz_util:format_account_id(Account, 'encoded'),
    case kz_datamgr:get_all_results(AccountDb, <<"callflows/crossbar_listing">>) of
        {'error', _M} ->
            io:format("error fetching callflows in ~p ~p~n", [AccountDb, _M]),
            [];
        {'ok', JObjs} ->
            get_migrateable_ring_group_callflows(AccountDb, JObjs)
    end.

-spec get_migrateable_ring_group_callflows(ne_binary(), kz_json:objects()) -> kz_json:objects().
get_migrateable_ring_group_callflows(AccountDb, JObjs) ->
    lists:foldl(fun(JObj, Acc) -> get_migrateable_ring_group_callflow(JObj, Acc, AccountDb) end
               ,[]
               ,JObjs
               ).

-spec get_migrateable_ring_group_callflow(kz_json:object(), kz_json:objects(), ne_binary()) ->
                                                 kz_json:objects().
get_migrateable_ring_group_callflow(JObj, Acc, AccountDb) ->
    case {kz_json:get_ne_binary_value([<<"value">>, <<"group_id">>], JObj)
         ,kz_json:get_ne_binary_value([<<"value">>, <<"type">>], JObj)
         }
    of
        {'undefined', _} -> Acc;
        {_, 'undefined'} ->
            Id = kz_doc:id(JObj),
            case kz_datamgr:open_cache_doc(AccountDb, Id) of
                {'ok', CallflowJObj} -> check_callflow_eligibility(CallflowJObj, Acc);
                {'error', _M} ->
                    io:format("  error fetching callflow ~p in ~p ~p~n", [Id, AccountDb, _M]),
                    Acc
            end;
        {_, _} -> Acc
    end.

-spec check_callflow_eligibility(kz_json:object(), kz_json:objects()) -> kz_json:objects().
check_callflow_eligibility(CallflowJObj, Acc) ->
    case kz_json:get_value([<<"flow">>, <<"module">>], CallflowJObj) of
        <<"ring_group">> -> [CallflowJObj|Acc];
        <<"record_call">> -> [CallflowJObj|Acc];
        _Module -> Acc
    end.

-spec create_new_ring_group_callflow(kz_json:object()) -> 'ok'.
create_new_ring_group_callflow(JObj) ->
    BaseGroup = base_group_ring_group(JObj),
    save_new_ring_group_callflow(JObj, BaseGroup).

-spec base_group_ring_group(kz_json:object()) -> kz_json:object().
base_group_ring_group(JObj) ->
    io:format("migrating callflow ~s: ~s~n", [kz_doc:id(JObj), kz_json:encode(JObj)]),
    BaseGroup = kz_json:from_list(
                  [{<<"pvt_vsn">>, <<"1">>}
                  ,{<<"pvt_type">>, <<"callflow">>}
                  ,{<<"pvt_modified">>, kz_time:current_tstamp()}
                  ,{<<"pvt_created">>, kz_time:current_tstamp()}
                  ,{<<"pvt_account_db">>, kz_doc:account_db(JObj)}
                  ,{<<"pvt_account_id">>, kz_doc:account_id(JObj)}
                  ,{<<"flow">>, kz_json:from_list([{<<"children">>, kz_json:new()}
                                                  ,{<<"module">>, <<"ring_group">>}
                                                  ])
                   }
                  ,{<<"group_id">>, kz_json:get_value(<<"group_id">>, JObj)}
                  ,{<<"type">>, <<"baseGroup">>}
                  ]),
    set_data_for_callflow(JObj, BaseGroup).

-spec set_data_for_callflow(kz_json:object(), kz_json:object()) -> kz_json:object().
set_data_for_callflow(JObj, BaseGroup) ->
    Flow = kz_json:get_value(<<"flow">>, BaseGroup),
    case kz_json:get_value([<<"flow">>, <<"module">>], JObj) of
        <<"ring_group">> ->
            Data = kz_json:get_value([<<"flow">>, <<"data">>], JObj),
            NewFlow = kz_json:set_value(<<"data">>, Data, Flow),
            set_number_for_callflow(JObj, kz_json:set_value(<<"flow">>, NewFlow, BaseGroup));
        <<"record_call">> ->
            Data = kz_json:get_value([<<"flow">>, <<"children">>, <<"_">>, <<"data">>], JObj),
            NewFlow = kz_json:set_value(<<"data">>, Data, Flow),
            set_number_for_callflow(JObj, kz_json:set_value(<<"flow">>, NewFlow, BaseGroup))
    end.

-spec set_number_for_callflow(kz_json:object(), kz_json:object()) -> kz_json:object().
set_number_for_callflow(JObj, BaseGroup) ->
    Number = <<"group_", (kz_term:to_binary(kz_time:now_ms()))/binary>>,
    Numbers = [Number],
    set_name_for_callflow(JObj, kz_json:set_value(<<"numbers">>, Numbers, BaseGroup)).

-spec set_name_for_callflow(kz_json:object(), kz_json:object()) -> kz_json:object().
set_name_for_callflow(JObj, BaseGroup) ->
    Name = kz_json:get_value(<<"name">>, JObj),
    NewName = binary:replace(Name, <<"Ring Group">>, <<"Base Group">>),
    set_ui_metadata(JObj, kz_json:set_value(<<"name">>, NewName, BaseGroup)).

-spec set_ui_metadata(kz_json:object(), kz_json:object()) -> kz_json:object().
set_ui_metadata(JObj, BaseGroup) ->
    MetaData = kz_json:get_value(<<"ui_metadata">>, JObj),
    NewMetaData = kz_json:set_value(<<"version">>, <<"v3.19">>, MetaData),
    kz_json:set_value(<<"ui_metadata">>, NewMetaData, BaseGroup).

-spec save_new_ring_group_callflow(kz_json:object(), kz_json:object()) -> 'ok'.
save_new_ring_group_callflow(JObj, NewCallflow) ->
    AccountDb = kz_doc:account_db(JObj),
    Name = kz_json:get_value(<<"name">>, NewCallflow),
    case check_if_callflow_exist(AccountDb, Name) of
        'true' ->
            io:format("unable to save new callflow '~s' in '~s'; already exists~n", [Name, AccountDb]);
        'false' ->
            save_new_ring_group_callflow(JObj, NewCallflow, AccountDb)
    end.

save_new_ring_group_callflow(JObj, NewCallflow, AccountDb) ->
    case kz_datamgr:save_doc(AccountDb, NewCallflow) of
        {'error', _M} ->
            io:format("unable to save new callflow (old:~p) in ~p aborting...~n"
                     ,[kz_doc:id(JObj), AccountDb]);
        {'ok', NewJObj} ->
            io:format("  saved base group callflow: ~s~n", [kz_json:encode(NewJObj)]),
            update_old_ring_group_callflow(JObj, NewJObj)
    end.

-spec check_if_callflow_exist(ne_binary(), ne_binary()) -> boolean().
check_if_callflow_exist(AccountDb, Name) ->
    case kz_datamgr:get_all_results(AccountDb, <<"callflows/crossbar_listing">>) of
        {'error', _M} ->
            io:format("error fetching callflows in ~p ~p~n", [AccountDb, _M]),
            'true';
        {'ok', JObjs} ->
            lists:any(fun(JObj) ->
                              kz_json:get_value([<<"value">>, <<"name">>], JObj) =:= Name
                      end
                     ,JObjs
                     )
    end.

-spec update_old_ring_group_callflow(kz_json:object(), kz_json:object()) -> 'ok'.
update_old_ring_group_callflow(JObj, NewCallflow) ->
    Routines = [fun update_old_ring_group_type/2
               ,fun update_old_ring_group_metadata/2
               ,fun update_old_ring_group_flow/2
               ,fun save_old_ring_group/2
               ],
    lists:foldl(fun(F, J) -> F(J, NewCallflow) end, JObj, Routines).

-spec update_old_ring_group_type(kz_json:object(), kz_json:object()) -> kz_json:object().
update_old_ring_group_type(JObj, _NewCallflow) ->
    kz_json:set_value(<<"type">>, <<"userGroup">>, JObj).

-spec update_old_ring_group_metadata(kz_json:object(), kz_json:object()) -> kz_json:object().
update_old_ring_group_metadata(JObj, _NewCallflow) ->
    MetaData = kz_json:get_value(<<"ui_metadata">>, JObj),
    NewMetaData = kz_json:set_value(<<"version">>, <<"v3.19">>, MetaData),
    kz_json:set_value(<<"ui_metadata">>, NewMetaData, JObj).

-spec update_old_ring_group_flow(kz_json:object(), kz_json:object()) -> kz_json:object().
update_old_ring_group_flow(JObj, NewCallflow) ->
    Data = kz_json:from_list([{<<"id">>, kz_doc:id(NewCallflow)}]),
    case kz_json:get_value([<<"flow">>, <<"module">>], JObj) of
        <<"ring_group">> ->
            Flow = kz_json:get_value(<<"flow">>, JObj),
            NewFlow = kz_json:set_values([{<<"data">>, Data}, {<<"module">>, <<"callflow">>}], Flow),
            kz_json:set_value(<<"flow">>, NewFlow, JObj);
        <<"record_call">> ->
            ChFlow = kz_json:get_value([<<"flow">>, <<"children">>, <<"_">>], JObj),
            ChNewFlow = kz_json:set_values([{<<"data">>, Data}, {<<"module">>, <<"callflow">>}], ChFlow),
            Children = kz_json:set_value(<<"_">>, ChNewFlow, kz_json:get_value([<<"flow">>, <<"children">>], JObj)),
            Flow = kz_json:set_value(<<"children">>, Children, kz_json:get_value(<<"flow">>, JObj)),
            kz_json:set_value(<<"flow">>, Flow, JObj)
    end.

-spec save_old_ring_group(kz_json:object(), kz_json:object()) -> 'ok'.
save_old_ring_group(JObj, NewCallflow) ->
    AccountDb = kz_doc:account_db(JObj),
    case kz_datamgr:save_doc(AccountDb, JObj) of
        {'error', _M} ->
            io:format("unable to save callflow ~p in ~p, removing new one (~p)~n"
                     ,[kz_doc:id(JObj), AccountDb, kz_doc:id(NewCallflow)]),
            {'ok', _} = kz_datamgr:del_doc(AccountDb, NewCallflow),
            'ok';
        {'ok', _OldJObj} ->
            io:format("  saved ring group callflow: ~s~n", [kz_json:encode(_OldJObj)])
    end.


-spec init_apps(file:name()) -> 'ok'.
init_apps(AppsPath) ->
    init_apps(AppsPath, 'undefined').

-spec init_apps(file:name(), api_binary()) -> 'ok'.
init_apps(AppsPath, AppUrl) ->
    Apps = find_apps(AppsPath),
    InitApp = fun(App) -> init_app(App, AppUrl) end,
    lists:foreach(InitApp, Apps).

-spec find_apps(file:name()) -> [file:name()].
find_apps(AppsPath) ->
    AccFun =
        fun(AppJSONPath, Acc) ->
                App = filename:absname(AppJSONPath),
                %% /.../App/metadata/app.json --> App
                [filename:dirname(filename:dirname(App)) | Acc]
        end,
    filelib:fold_files(AppsPath, "app\\.json", 'true', AccFun, []).

-spec init_app(file:filename_all()) -> 'ok'.
init_app(AppPath) ->
    init_app(AppPath, 'undefined').

-spec init_app(file:filename_all(), api_binary()) -> 'ok'.
init_app(AppPath, AppUrl) ->
    io:format("trying to init app from ~s~n", [AppPath]),
    try find_metadata(AppPath) of
        {'ok', MetaData} ->
            maybe_create_app(AppPath, maybe_set_api_url(AppUrl, MetaData));
        {'invalid_data', _E} ->
            io:format("  failed to validate app data ~s: ~p~n", [AppPath, _E])
    catch
        'error':{'badmatch', {'error', 'enoent'}} ->
            io:format("  failed to incorporate app because there was no app.json in ~s~n"
                     ,[filename:join([AppPath, <<"metadata">>])]);
        'error':_E ->
            io:format("  failed to find metadata in ~s: ~p~n", [AppPath, _E])
    end.

-spec maybe_set_api_url(api_binary(), kz_json:object()) -> kz_json:object().
maybe_set_api_url('undefined', MetaData) ->
    kz_json:delete_key(<<"api_url">>, MetaData);
maybe_set_api_url(AppUrl, MetaData) ->
    kz_json:set_value(<<"api_url">>, AppUrl, MetaData).

-spec maybe_create_app(file:filename_all(), kz_json:object()) -> 'ok'.
-spec maybe_create_app(file:filename_all(), kz_json:object(), ne_binary()) -> 'ok'.
maybe_create_app(AppPath, MetaData) ->
    {'ok', MasterAccountDb} = kapps_util:get_master_account_db(),
    maybe_create_app(AppPath, MetaData, MasterAccountDb).

maybe_create_app(AppPath, MetaData, MasterAccountDb) ->
    AppName = kzd_app:name(MetaData),
    case find_app(MasterAccountDb, AppName) of
        {'ok', JObj} ->
            io:format(" app ~s already loaded in system~n", [AppName]),
            maybe_update_app(AppPath, MetaData, MasterAccountDb, JObj);
        {'error', 'not_found'} -> create_app(AppPath, MetaData, MasterAccountDb);
        {'error', _E} -> io:format(" failed to find app ~s: ~p", [AppName, _E])
    end.

-spec maybe_update_app(file:filename_all(), kz_json:object(), ne_binary(), kz_json:object()) -> no_return.
maybe_update_app(AppPath, MetaData, MasterAccountDb, AppJObj) ->
    ApiUrlKey = <<"api_url">>,
    CurrentDocId = kzd_app:id(AppJObj),
    CurrentApiUrl = kzd_app:api_url(kz_json:get_value(<<"value">>, AppJObj)),
    case kzd_app:api_url(MetaData) of
        'undefined'   -> io:format(" not updating ~s, it is undefined~n", [ApiUrlKey]);
        CurrentApiUrl -> io:format(" not updating ~s, it is unchanged~n", [ApiUrlKey]);
        NewApiUrl ->
            Update = [{ApiUrlKey, NewApiUrl}],
            case kz_datamgr:update_doc(MasterAccountDb, CurrentDocId, Update) of
                {'ok', _NJObj} -> io:format(" updated ~s to ~s~n", [ApiUrlKey, NewApiUrl]);
                {'error', Err} -> io:format(" error updating ~s: ~p~n", [ApiUrlKey, Err])
            end
    end,
    'ok' = delete_old_images(CurrentDocId, MetaData, MasterAccountDb),
    maybe_add_images(AppPath, CurrentDocId, MetaData, MasterAccountDb).

-spec find_app(ne_binary(), ne_binary()) -> {'ok', kz_json:object()} |
                                            {'error', any()}.
find_app(Db, Name) ->
    ViewOptions = [{'key', Name}],
    kz_datamgr:get_single_result(Db, ?CB_APPS_STORE_LIST, ViewOptions).

-spec create_app(file:filename_all(), kz_json:object(), ne_binary()) -> 'ok'.
create_app(AppPath, MetaData, MasterAccountDb) ->
    Doc0 = kz_doc:update_pvt_parameters(MetaData, MasterAccountDb, [{'type', <<"app">>}]),
    Doc = kz_json:delete_key(<<"source_url">>, Doc0),
    case kz_datamgr:save_doc(MasterAccountDb, Doc) of
        {'ok', AppJObj} ->
            AppId = kzd_app:id(AppJObj),
            io:format(" saved app ~s as doc ~s~n", [kzd_app:name(AppJObj), AppId]),
            maybe_add_images(AppPath, AppId, MetaData, MasterAccountDb);
        {'error', _E} ->
            io:format(" failed to save app ~s to ~s: ~p~n"
                     ,[kzd_app:name(MetaData), MasterAccountDb, _E])
    end.

-spec delete_old_images(ne_binary(), kz_json:object(), ne_binary()) -> 'ok'.
delete_old_images(AppId, MetaData, MasterAccountDb) ->
    F = fun (X) -> safe_delete_image(MasterAccountDb, AppId, X) end,
    lists:foreach(F, [kzd_app:icon(MetaData)]),
    lists:foreach(F, kzd_app:screenshots(MetaData)).

-spec safe_delete_image(ne_binary(), ne_binary(), api_binary()) -> 'ok'.
safe_delete_image(_AccountDb, _AppId, 'undefined') -> 'ok';
safe_delete_image(AccountDb, AppId, Image) ->
    case kz_datamgr:fetch_attachment(AccountDb, AppId, Image) of
        {'error', _} -> 'ok';
        {'ok', _} ->
            kz_datamgr:delete_attachment(AccountDb, AppId, Image)
    end.

-spec maybe_add_images(file:filename_all(), ne_binary(), kz_json:object(), ne_binary()) -> no_return.
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
    no_return.

update_screenshots(AppId, MA, SShotPaths) ->
    update_images(AppId, MA, SShotPaths, <<"screenshots">>),
    no_return.

-type image_path() :: {file:filename_all(), file:filename_all()}.
-type image_paths() :: [image_path()].

-spec update_images(ne_binary(), ne_binary(), image_paths(), ne_binary()) -> 'ok'.
update_images(AppId, MasterAccountDb, ImagePaths, Type) ->
    try read_images(ImagePaths) of
        {'ok', Images} -> add_images(AppId, MasterAccountDb, Images)
    catch
        'error':{'badmatch', {'error', 'enoent'}} ->
            io:format("  failed to find ~s in ~s~n", [Type, AppId]);
        'error':_E ->
            io:format("  failed to load ~s in ~s: ~p~n", [Type, AppId, _E])
    end.

-spec add_images(ne_binary(), ne_binary(), kz_proplist()) -> 'ok'.
add_images(AppId, MasterAccountDb, Images) ->
    _ = [add_image(AppId, MasterAccountDb, ImageId, ImageData)
         || {ImageId, ImageData} <- Images
        ],
    'ok'.

-spec add_image(ne_binary(), ne_binary(), file:filename_all(), binary()) -> 'ok'.
add_image(AppId, MasterAccountDb, ImageId, ImageData) ->
    case kz_datamgr:put_attachment(MasterAccountDb, AppId, ImageId, ImageData) of
        {'ok', _} ->     io:format("   saved ~s to ~s~n", [ImageId, AppId]);
        {'error', _E} -> io:format("   failed to save ~s to ~s: ~p~n", [ImageId, AppId, _E])
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
                           {'invalid_data', kz_proplist()}.
find_metadata(AppPath) ->
    {'ok', JSON} = file:read_file(filename:join([AppPath, <<"metadata">>, <<"app.json">>])),
    {'ok', Schema} = kz_json_schema:load(<<"app">>),
    case jesse:validate_with_schema(Schema, kz_doc:public_fields(kz_json:decode(JSON))) of
        {'ok', _}=OK -> OK;
        {'error', Errors} ->
            {'invalid_data', [Error || {'data_invalid', _, Error, _, _} <- Errors]}
    end.

-spec apps() -> no_return.
apps() ->
    {ok, MA} = kapps_util:get_master_account_db(),
    case kz_datamgr:get_results(MA, ?CB_APPS_STORE_LIST) of
        {error, _R} -> lager:debug("failed to read apps in ~s: ~p", [MA, _R]);
        {ok, JObjs} -> lists:foreach(fun print_app/1, JObjs)
    end,
    no_return.

-spec app(ne_binary()) -> no_return.
app(AppNameOrId) ->
    {ok, MA} = kapps_util:get_master_account_db(),
    case find_app(MA, AppNameOrId) of
        {ok, AppJObj} -> print_app(AppJObj);
        {error, _} ->
            case kz_datamgr:open_doc(MA, AppNameOrId) of
                {ok, AppJObj} -> print_app(view_app(AppJObj));
                _ -> io:format("unknown app\n"), no_return
            end
    end.

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
    io:format("App ~s\n", [kz_json:get_ne_value(<<"key">>, AppJObj)]),
    _ = put(pp_lvl, 1),
    _ = print_k_v(kz_json:get_value(<<"value">>, AppJObj)),
    io:format("\n"),
    no_return.

print_k_v(JObj) -> kz_json:map(fun print_k_v/2, JObj).
print_k_v(K, V) ->
    Lvl = get(pp_lvl),
    Indent = string:copies("  ", Lvl),
    case kz_json:is_json_object(V) of
        false -> io:format("~s~s: ~s\n", [Indent, K, kz_json:encode(V)]);
        true ->
            io:format("~s~s:\n", [Indent, K]),
            _ = put(pp_lvl, Lvl + 1),
            print_k_v(V),
            _ = put(pp_lvl, Lvl)
    end.

update_app(AppId, Path, Value) ->
    case lists:last(Path) of
        <<"_id">> -> ok;
        <<"pvt_", _/binary>> -> ok;
        ?NE_BINARY ->
            {ok, MA} = kapps_util:get_master_account_db(),
            Update = [{Path, Value}],
            case kz_datamgr:update_doc(MA, AppId, Update) of
                {ok, _} -> app(AppId);
                {error, _R} -> io:format("updating ~s failed: ~p\n", [AppId, _R])
            end
    end,
    no_return.

-spec set_app_field(ne_binary(), ne_binary(), ne_binary()) -> no_return.
set_app_field(AppId, Field, Value) ->
    update_app(AppId, [Field], Value).

-spec set_app_label(ne_binary(), ne_binary()) -> no_return.
-spec set_app_description(ne_binary(), ne_binary()) -> no_return.
-spec set_app_extended_description(ne_binary(), ne_binary()) -> no_return.
-spec set_app_features(ne_binary(), ne_binary()) -> no_return.
set_app_label(AppId, Value) ->
    update_app(AppId, [<<"i18n">>, <<"en-US">>, <<"label">>], Value).
set_app_description(AppId, Value) ->
    update_app(AppId, [<<"i18n">>, <<"en-US">>, <<"description">>], Value).
set_app_extended_description(AppId, Value) ->
    update_app(AppId, [<<"i18n">>, <<"en-US">>, <<"extended_description">>], Value).
set_app_features(AppId, Value) ->
    Values = [V || V <- binary:split(Value, <<$@>>, [global]),
                   V =/= <<>>
             ],
    update_app(AppId, [<<"i18n">>, <<"en-US">>, <<"features">>], Values).

-spec set_app_icon(ne_binary(), ne_binary()) -> no_return.
set_app_icon(AppId, PathToPNGIcon) ->
    {ok, MA} = kapps_util:get_master_account_db(),
    io:format("Processing...\n"),
    Icon = {filename:basename(PathToPNGIcon), PathToPNGIcon},
    update_icon(AppId, MA, Icon).

-spec set_app_screenshots(ne_binary(), ne_binary()) -> no_return.
set_app_screenshots(AppId, PathToScreenshotsFolder) ->
    {ok, MA} = kapps_util:get_master_account_db(),
    io:format("Processing...\n"),
    SShots = [{filename:basename(SShot), SShot}
              || SShot <- filelib:wildcard(kz_term:to_list(PathToScreenshotsFolder) ++ "/*.png")
             ],
    update_screenshots(AppId, MA, SShots).
