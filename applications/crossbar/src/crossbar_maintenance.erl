%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
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
-export([enable_account/1, disable_account/1]).
-export([promote_account/1, demote_account/1]).
-export([allow_account_number_additions/1, disallow_account_number_additions/1]).
-export([create_account/4]).
-export([create_account/1]).
-export([move_account/2]).
-export([descendants_count/0, descendants_count/1]).
-export([migrate_ring_group_callflow/1]).

-export([init_apps/2, init_app/2]).

-include_lib("crossbar.hrl").
-include_lib("whistle/include/wh_system_config.hrl").

-type input_term() :: atom() | string() | ne_binary().

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec migrate() -> 'no_return'.
migrate() ->
    migrate(whapps_util:get_all_accounts()).

-spec migrate(ne_binaries()) -> 'no_return'.
migrate(Accounts) ->
    _ = migrate_accounts_data(Accounts),

    CurrentModules =
        [wh_util:to_atom(Module, 'true')
         || Module <- crossbar_config:autoload_modules()
        ],

    UpdatedModules = remove_deprecated_modules(CurrentModules, ?DEPRECATED_MODULES),

    add_missing_modules(
      UpdatedModules
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
    migrate_accounts_data(whapps_util:get_all_accounts()).

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
    io:format("please use whapps_maintenance:refresh().~n").

refresh(Value) ->
    io:format("please use whapps_maintenance:refresh(~p).~n", [Value]).

-spec flush() -> 'ok'.
flush() ->
    crossbar_config:flush(),
    wh_cache:flush_local(?CROSSBAR_CACHE).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec start_module(text()) -> 'ok'.
start_module(Module) ->
    try crossbar:start_mod(Module) of
        _ -> maybe_autoload_module(wh_util:to_binary(Module))
    catch
        _E:_R ->
            io:format("failed to start ~s: ~s: ~p~n", [Module, _E, _R])
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
      [wh_util:to_binary(Module)
       | lists:delete(wh_util:to_binary(Module), Mods)
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
    try crossbar:stop_mod(Module) of
        _ ->
            Mods = crossbar_config:autoload_modules(),
            crossbar_config:set_default_autoload_modules(lists:delete(wh_util:to_binary(Module), Mods)),
            io:format("stopped and removed ~s from autoloaded modules~n", [Module])
    catch
        _E:_R ->
            io:format("failed to stop ~s: ~s: ~p~n", [Module, _E, _R])
    end.

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
-spec find_account_by_number(input_term()) ->
                                    {'ok', ne_binary()} |
                                    {'error', _}.
find_account_by_number(Number) when not is_binary(Number) ->
    find_account_by_number(wh_util:to_binary(Number));
find_account_by_number(Number) ->
    case wh_number_manager:lookup_account_by_number(Number) of
        {'ok', AccountId, _} ->
            AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
            print_account_info(AccountDb, AccountId);
        {'error', {'not_in_service', AssignedTo}} ->
            AccountDb = wh_util:format_account_id(AssignedTo, 'encoded'),
            print_account_info(AccountDb, AssignedTo);
        {'error', {'account_disabled', AssignedTo}} ->
            AccountDb = wh_util:format_account_id(AssignedTo, 'encoded'),
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
                                  {'error', _}.
find_account_by_name(Name) when not is_binary(Name) ->
    find_account_by_name(wh_util:to_binary(Name));
find_account_by_name(Name) ->
    case whapps_util:get_accounts_by_name(Name) of
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
                                   {'error', _}.
find_account_by_realm(Realm) when not is_binary(Realm) ->
    find_account_by_realm(wh_util:to_binary(Realm));
find_account_by_realm(Realm) ->
    case whapps_util:get_account_by_realm(Realm) of
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
-spec allow_account_number_additions(input_term()) -> 'ok' | 'failed'.
allow_account_number_additions(AccountId) ->
    case update_account(AccountId, <<"pvt_wnm_allow_additions">>, 'true') of
        {'ok', _} ->
            io:format("allowing account '~s' to add numbers~n", [AccountId]);
        {'error', Reason} ->
            io:format("failed to find account: ~p~n", [Reason]),
            'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec disallow_account_number_additions(input_term()) -> 'ok' | 'failed'.
disallow_account_number_additions(AccountId) ->
    case update_account(AccountId, <<"pvt_wnm_allow_additions">>, 'false') of
        {'ok', _} ->
            io:format("disallowed account '~s' to added numbers~n", [AccountId]);
        {'error', Reason} ->
            io:format("failed to find account: ~p~n", [Reason]),
            'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec enable_account(input_term()) -> 'ok' | 'failed'.
enable_account(AccountId) ->
    case update_account(AccountId, <<"pvt_enabled">>, 'true') of
        {'ok', _} ->
            io:format("enabled account '~s'~n", [AccountId]);
        {'error', Reason} ->
            io:format("failed to enable account: ~p~n", [Reason]),
            'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec disable_account(input_term()) -> 'ok' | 'failed'.
disable_account(AccountId) ->
    case update_account(AccountId, <<"pvt_enabled">>, 'false') of
        {'ok', _} ->
            io:format("disabled account '~s'~n", [AccountId]);
        {'error', Reason} ->
            io:format("failed to disable account: ~p~n", [Reason]),
            'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec promote_account(input_term()) -> 'ok' | 'failed'.
promote_account(AccountId) ->
    case update_account(AccountId, <<"pvt_superduper_admin">>, 'true') of
        {'ok', _} ->
            io:format("promoted account '~s', this account now has permission to change system settings~n", [AccountId]);
        {'error', Reason} ->
            io:format("failed to promote account: ~p~n", [Reason]),
            'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec demote_account(input_term()) -> 'ok' | 'failed'.
demote_account(AccountId) ->
    case update_account(AccountId, <<"pvt_superduper_admin">>, 'false') of
        {'ok', _} ->
            io:format("promoted account '~s', this account can no longer change system settings~n", [AccountId]);
        {'error', Reason} ->
            io:format("failed to demote account: ~p~n", [Reason]),
            'failed'
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_account(input_term(), input_term(), input_term(), input_term()) -> 'ok' | 'failed'.
create_account(AccountName, Realm, Username, Password) when not is_binary(AccountName) ->
    create_account(wh_util:to_binary(AccountName), Realm, Username, Password);
create_account(AccountName, Realm, Username, Password) when not is_binary(Realm) ->
    create_account(AccountName, wh_util:to_binary(Realm), Username, Password);
create_account(AccountName, Realm, Username, Password) when not is_binary(Username) ->
    create_account(AccountName, Realm, wh_util:to_binary(Username), Password);
create_account(AccountName, Realm, Username, Password) when not is_binary(Password) ->
    create_account(AccountName, Realm, Username, wh_util:to_binary(Password));
create_account(AccountName, Realm, Username, Password) ->
    Account = wh_json:from_list([{<<"_id">>, couch_mgr:get_uuid()}
                                 ,{<<"name">>, AccountName}
                                 ,{<<"realm">>, Realm}
                                ]),
    User = wh_json:from_list([{<<"_id">>, couch_mgr:get_uuid()}
                              ,{<<"username">>, Username}
                              ,{<<"password">>, Password}
                              ,{<<"first_name">>, <<"Account">>}
                              ,{<<"last_name">>, <<"Admin">>}
                              ,{<<"priv_level">>, <<"admin">>}
                             ]),
    try
        {'ok', C1} = validate_account(Account, cb_context:new()),
        {'ok', C2} = create_account(C1),
        {'ok', C3} = validate_user(User, C2),
        {'ok', _} = create_user(C3),

        AccountDb = cb_context:account_db(C3),
        AccountId = cb_context:account_id(C3),

        case whapps_util:get_all_accounts() of
            [AccountDb] ->
                _ = promote_account(AccountId),
                _ = allow_account_number_additions(AccountId),
                _ = whistle_services_maintenance:make_reseller(AccountId),
                _ = update_system_config(AccountId),
                'ok';
            _Else -> 'ok'
        end,
        'ok'
    catch
        _E:_R ->
            ST = erlang:get_stacktrace(),
            lager:error("crashed creating account: ~s: ~p", [_E, _R]),
            wh_util:log_stacktrace(ST),
            'failed'
    end.

-spec update_system_config(ne_binary()) -> 'ok'.
update_system_config(AccountId) ->
    whapps_config:set(?WH_SYSTEM_CONFIG_ACCOUNT, <<"master_account_id">>, AccountId),
    io:format("updating master account id in system_config.~s~n", [?WH_SYSTEM_CONFIG_ACCOUNT]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_account(wh_json:object(), cb_context:context()) ->
                              {'ok', cb_context:context()} |
                              {'error', wh_json:object()}.
validate_account(JObj, Context) ->
    Payload = [cb_context:setters(Context
                                  ,[{fun cb_context:set_req_data/2, JObj}
                                    ,{fun cb_context:set_req_nouns/2, [{?WH_ACCOUNTS_DB, []}]}
                                    ,{fun cb_context:set_req_verb/2, ?HTTP_PUT}
                                    ,{fun cb_context:set_resp_status/2, 'fatal'}
                                   ])
              ],
    Context1 = crossbar_bindings:fold(<<"v1_resource.validate.accounts">>, Payload),
    case cb_context:resp_status(Context1) of
        'success' -> {'ok', Context1};
        _Status ->
            Errors = cb_context:resp_data(Context1),
            io:format("failed to validate account properties(~p): '~s'~n", [_Status, wh_json:encode(Errors)]),
            {'error', Errors}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec validate_user(wh_json:object(), cb_context:context()) ->
                           {'ok', cb_context:context()} |
                           {'error', wh_json:object()}.
validate_user(JObj, Context) ->
    Payload = [cb_context:setters(Context
                                  ,[{fun cb_context:set_req_data/2, JObj}
                                    ,{fun cb_context:set_req_nouns/2, [{?WH_ACCOUNTS_DB, []}]}
                                    ,{fun cb_context:set_req_verb/2, ?HTTP_PUT}
                                    ,{fun cb_context:set_resp_status/2, 'fatal'}
                                   ]
                                 )
              ],
    Context1 = crossbar_bindings:fold(<<"v1_resource.validate.users">>, Payload),
    case cb_context:resp_status(Context1) of
        'success' -> {'ok', Context1};
        _Status ->
            Errors = cb_context:resp_data(Context1),
            io:format("failed to validate user properties: '~s'~n", [wh_json:encode(Errors)]),
            {'error', Errors}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_account(cb_context:context()) ->
                            {'ok', cb_context:context()} |
                            {'error', wh_json:object()}.
create_account(Context) ->
    Context1 = crossbar_bindings:fold(<<"v1_resource.execute.put.accounts">>, [Context]),
    case cb_context:resp_status(Context1) of
        'success' ->
            io:format("created new account '~s' in db '~s'~n", [cb_context:account_id(Context1)
                                                                ,cb_context:account_db(Context1)
                                                               ]),
            {'ok', Context1};
        _Status ->
            Errors = cb_context:resp_data(Context1),
            io:format("failed to create account: '~s'~n", [wh_json:encode(Errors)]),
            AccountId = wh_doc:id(cb_context:req_data(Context)),
            couch_mgr:db_delete(wh_util:format_account_id(AccountId, 'encoded')),
            {'error', Errors}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec create_user(cb_context:context()) ->
                         {'ok', cb_context:context()} |
                         {'error', wh_json:object()}.
create_user(Context) ->
    Context1 = crossbar_bindings:fold(<<"v1_resource.execute.put.users">>, [Context]),
    case cb_context:resp_status(Context1) of
        'success' ->
            io:format("created new account admin user '~s'~n"
                      ,[wh_doc:id(cb_context:doc(Context1))]
                     ),
            {'ok', Context1};
        _Status ->
            Errors = cb_context:resp_data(Context1),
            io:format("failed to create account admin user: '~s'~n", [wh_json:encode(Errors)]),
            {'error', Errors}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec update_account(input_term(), ne_binary(), _) ->
                            {'ok', wh_json:object()} |
                            {'error', _}.
update_account(AccountId, Key, Value) when not is_binary(AccountId) ->
    update_account(wh_util:to_binary(AccountId), Key, Value);
update_account(AccountId, Key, Value) ->
    Updaters = [fun({'error', _}=E) -> E;
                   ({'ok', J}) ->
                        AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
                        couch_mgr:save_doc(AccountDb, wh_json:set_value(Key, Value, J))
                end
                ,fun({'error', _}=E) -> E;
                    ({'ok', J}) ->
                         case couch_mgr:lookup_doc_rev(?WH_ACCOUNTS_DB, AccountId) of
                             {'ok', Rev} ->
                                 couch_mgr:save_doc(?WH_ACCOUNTS_DB, wh_doc:set_revision(J, Rev));
                             {'error', 'not_found'} ->
                                 couch_mgr:save_doc(?WH_ACCOUNTS_DB, wh_doc:delete_revision(J))
                         end
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end
                ,kz_account:fetch(AccountId)
                ,Updaters
               ).

-spec print_account_info(ne_binary()) -> {'ok', ne_binary()}.
-spec print_account_info(ne_binary(), ne_binary()) -> {'ok', ne_binary()}.
print_account_info(AccountDb) ->
    AccountId = wh_util:format_account_id(AccountDb, 'raw'),
    print_account_info(AccountDb, AccountId).
print_account_info(AccountDb, AccountId) ->
    case couch_mgr:open_doc(AccountDb, AccountId) of
        {'ok', JObj} ->
            io:format("Account ID: ~s (~s)~n", [AccountId, AccountDb]),
            io:format("  Name: ~s~n", [kz_account:name(JObj)]),
            io:format("  Realm: ~s~n", [kz_account:realm(JObj)]),
            io:format("  Enabled: ~s~n", [kz_account:is_enabled(JObj)]),
            io:format("  System Admin: ~s~n", [kz_account:is_superduper_admin(JObj)]);
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
    AccountId = wh_util:format_account_id(Account, 'raw'),
    ToAccountId = wh_util:format_account_id(ToAccount, 'raw'),
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

-spec get_migrateable_ring_group_callflows(ne_binary()) -> wh_json:objects().
get_migrateable_ring_group_callflows(Account) ->
    AccountDb = wh_util:format_account_id(Account, 'encoded'),
    case couch_mgr:get_all_results(AccountDb, <<"callflows/crossbar_listing">>) of
        {'error', _M} ->
            io:format("error fetching callflows in ~p ~p~n", [AccountDb, _M]),
            [];
        {'ok', JObjs} ->
            get_migrateable_ring_group_callflows(AccountDb, JObjs)
    end.

-spec get_migrateable_ring_group_callflows(ne_binary(), wh_json:objects()) -> wh_json:objects().
get_migrateable_ring_group_callflows(AccountDb, JObjs) ->
    lists:foldl(fun(JObj, Acc) -> get_migrateable_ring_group_callflow(JObj, Acc, AccountDb) end
                ,[]
                ,JObjs
               ).

-spec get_migrateable_ring_group_callflow(wh_json:object(), wh_json:objects(), ne_binary()) ->
                                                 wh_json:objects().
get_migrateable_ring_group_callflow(JObj, Acc, AccountDb) ->
    case {wh_json:get_ne_binary_value([<<"value">>, <<"group_id">>], JObj)
          ,wh_json:get_ne_binary_value([<<"value">>, <<"type">>], JObj)
         }
    of
        {'undefined', _} -> Acc;
        {_, 'undefined'} ->
            Id = wh_doc:id(JObj),
            case couch_mgr:open_cache_doc(AccountDb, Id) of
                {'ok', CallflowJObj} -> check_callflow_eligibility(CallflowJObj, Acc);
                {'error', _M} ->
                    io:format("  error fetching callflow ~p in ~p ~p~n", [Id, AccountDb, _M]),
                    Acc
            end;
        {_, _} -> Acc
    end.

-spec check_callflow_eligibility(wh_json:object(), wh_json:objects()) -> wh_json:objects().
check_callflow_eligibility(CallflowJObj, Acc) ->
    case wh_json:get_value([<<"flow">>, <<"module">>], CallflowJObj) of
        <<"ring_group">> -> [CallflowJObj|Acc];
        <<"record_call">> -> [CallflowJObj|Acc];
        _Module -> Acc
    end.

-spec create_new_ring_group_callflow(wh_json:object()) -> 'ok'.
create_new_ring_group_callflow(JObj) ->
    BaseGroup = base_group_ring_group(JObj),
    save_new_ring_group_callflow(JObj, BaseGroup).

-spec base_group_ring_group(wh_json:object()) -> wh_json:object().
base_group_ring_group(JObj) ->
    io:format("migrating callflow ~s: ~s~n", [wh_doc:id(JObj), wh_json:encode(JObj)]),
    BaseGroup = wh_json:from_list(
                  props:filter_undefined(
                    [{<<"pvt_vsn">>, <<"1">>}
                     ,{<<"pvt_type">>, <<"callflow">>}
                     ,{<<"pvt_modified">>, wh_util:current_tstamp()}
                     ,{<<"pvt_created">>, wh_util:current_tstamp()}
                     ,{<<"pvt_account_db">>, wh_doc:account_db(JObj)}
                     ,{<<"pvt_account_id">>, wh_doc:account_id(JObj)}
                     ,{<<"flow">>, wh_json:from_list([{<<"children">>, wh_json:new()}
                                                      ,{<<"module">>, <<"ring_group">>}
                                                     ])
                      }
                     ,{<<"group_id">>, wh_json:get_value(<<"group_id">>, JObj)}
                     ,{<<"type">>, <<"baseGroup">>}
                    ])),
    set_data_for_callflow(JObj, BaseGroup).

-spec set_data_for_callflow(wh_json:object(), wh_json:object()) -> wh_json:object().
set_data_for_callflow(JObj, BaseGroup) ->
    Flow = wh_json:get_value(<<"flow">>, BaseGroup),
    case wh_json:get_value([<<"flow">>, <<"module">>], JObj) of
        <<"ring_group">> ->
            Data = wh_json:get_value([<<"flow">>, <<"data">>], JObj),
            NewFlow = wh_json:set_value(<<"data">>, Data, Flow),
            set_number_for_callflow(JObj, wh_json:set_value(<<"flow">>, NewFlow, BaseGroup));
        <<"record_call">> ->
            Data = wh_json:get_value([<<"flow">>, <<"children">>, <<"_">>, <<"data">>], JObj),
            NewFlow = wh_json:set_value(<<"data">>, Data, Flow),
            set_number_for_callflow(JObj, wh_json:set_value(<<"flow">>, NewFlow, BaseGroup))
    end.

-spec set_number_for_callflow(wh_json:object(), wh_json:object()) -> wh_json:object().
set_number_for_callflow(JObj, BaseGroup) ->
    Number = <<"group_", (wh_util:to_binary(wh_util:now_ms(os:timestamp())))/binary>>,
    Numbers = [Number],
    set_name_for_callflow(JObj, wh_json:set_value(<<"numbers">>, Numbers, BaseGroup)).

-spec set_name_for_callflow(wh_json:object(), wh_json:object()) -> wh_json:object().
set_name_for_callflow(JObj, BaseGroup) ->
    Name = wh_json:get_value(<<"name">>, JObj),
    NewName = binary:replace(Name, <<"Ring Group">>, <<"Base Group">>),
    set_ui_metadata(JObj, wh_json:set_value(<<"name">>, NewName, BaseGroup)).

-spec set_ui_metadata(wh_json:object(), wh_json:object()) -> wh_json:object().
set_ui_metadata(JObj, BaseGroup) ->
    MetaData = wh_json:get_value(<<"ui_metadata">>, JObj),
    NewMetaData = wh_json:set_value(<<"version">>, <<"v3.19">>, MetaData),
    wh_json:set_value(<<"ui_metadata">>, NewMetaData, BaseGroup).

-spec save_new_ring_group_callflow(wh_json:object(), wh_json:object()) -> 'ok'.
save_new_ring_group_callflow(JObj, NewCallflow) ->
    AccountDb = wh_doc:account_db(JObj),
    Name = wh_json:get_value(<<"name">>, NewCallflow),
    case check_if_callflow_exist(AccountDb, Name) of
        'true' ->
            io:format("unable to save new callflow '~s' in '~s'; already exists~n", [Name, AccountDb]);
        'false' ->
            save_new_ring_group_callflow(JObj, NewCallflow, AccountDb)
    end.

save_new_ring_group_callflow(JObj, NewCallflow, AccountDb) ->
    case couch_mgr:save_doc(AccountDb, NewCallflow) of
        {'error', _M} ->
            io:format("unable to save new callflow (old:~p) in ~p aborting...~n"
                      ,[wh_doc:id(JObj), AccountDb]
                     );
        {'ok', NewJObj} ->
            io:format("  saved base group callflow: ~s~n", [wh_json:encode(NewJObj)]),
            update_old_ring_group_callflow(JObj, NewJObj)
    end.

-spec check_if_callflow_exist(ne_binary(), ne_binary()) -> boolean().
check_if_callflow_exist(AccountDb, Name) ->
    case couch_mgr:get_all_results(AccountDb, <<"callflows/crossbar_listing">>) of
        {'error', _M} ->
            io:format("error fetching callflows in ~p ~p~n", [AccountDb, _M]),
            'true';
        {'ok', JObjs} ->
            lists:any(
              fun(JObj) ->
                      wh_json:get_value([<<"value">>, <<"name">>], JObj) =:= Name
              end
              ,JObjs
            )
    end.

-spec update_old_ring_group_callflow(wh_json:object(), wh_json:object()) -> 'ok'.
update_old_ring_group_callflow(JObj, NewCallflow) ->
    Routines = [fun update_old_ring_group_type/2
                ,fun update_old_ring_group_metadata/2
                ,fun update_old_ring_group_flow/2
                ,fun save_old_ring_group/2
               ],
    lists:foldl(fun(F, J) -> F(J, NewCallflow) end, JObj, Routines).

-spec update_old_ring_group_type(wh_json:object(), wh_json:object()) -> wh_json:object().
update_old_ring_group_type(JObj, _NewCallflow) ->
    wh_json:set_value(<<"type">>, <<"userGroup">>, JObj).

-spec update_old_ring_group_metadata(wh_json:object(), wh_json:object()) -> wh_json:object().
update_old_ring_group_metadata(JObj, _NewCallflow) ->
    MetaData = wh_json:get_value(<<"ui_metadata">>, JObj),
    NewMetaData = wh_json:set_value(<<"version">>, <<"v3.19">>, MetaData),
    wh_json:set_value(<<"ui_metadata">>, NewMetaData, JObj).

-spec update_old_ring_group_flow(wh_json:object(), wh_json:object()) -> wh_json:object().
update_old_ring_group_flow(JObj, NewCallflow) ->
    Data = wh_json:from_list([{<<"id">>, wh_doc:id(NewCallflow)}]),
    case wh_json:get_value([<<"flow">>, <<"module">>], JObj) of
        <<"ring_group">> ->
            Flow = wh_json:get_value(<<"flow">>, JObj),
            NewFlow = wh_json:set_values([{<<"data">>, Data}, {<<"module">>, <<"callflow">>}], Flow),
            wh_json:set_value(<<"flow">>, NewFlow, JObj);
        <<"record_call">> ->
            ChFlow = wh_json:get_value([<<"flow">>, <<"children">>, <<"_">>], JObj),
            ChNewFlow = wh_json:set_values([{<<"data">>, Data}, {<<"module">>, <<"callflow">>}], ChFlow),
            Children = wh_json:set_value(<<"_">>, ChNewFlow, wh_json:get_value([<<"flow">>, <<"children">>], JObj)),
            Flow = wh_json:set_value(<<"children">>, Children, wh_json:get_value(<<"flow">>, JObj)),
            wh_json:set_value(<<"flow">>, Flow, JObj)
    end.

-spec save_old_ring_group(wh_json:object(), wh_json:object()) -> 'ok'.
save_old_ring_group(JObj, NewCallflow) ->
    AccountDb = wh_doc:account_db(JObj),
    case couch_mgr:save_doc(AccountDb, JObj) of
        {'error', _M} ->
            io:format("unable to save callflow ~p in ~p, removing new one (~p)~n"
                      ,[wh_doc:id(JObj), AccountDb, wh_doc:id(NewCallflow)]
                     ),
            {'ok', _} = couch_mgr:del_doc(AccountDb, NewCallflow),
            'ok';
        {'ok', _OldJObj} ->
            io:format("  saved ring group callflow: ~s~n", [wh_json:encode(_OldJObj)])
    end.

-spec init_apps(filelib:dirname(), ne_binary()) -> 'ok'.
init_apps(AppsPath, AppUrl) ->
    Apps = find_apps(AppsPath),
    InitApp = fun(App) -> init_app(App, AppUrl) end,
    lists:foreach(InitApp, Apps).

-spec find_apps(filelib:dirname()) -> [file:name()].
find_apps(AppsPath) ->
    AccFun =
        fun (AppJSONPath, Acc) ->
                App = filename:absname(AppJSONPath),
                %% App/metadata/app.json --> App
                [filename:dirname(filename:dirname(App)) | Acc]
        end,
    filelib:fold_files(AppsPath, "app\\.json", 'true', AccFun, []).

-spec init_app(file:filename(), ne_binary()) -> 'ok'.
init_app(AppPath, AppUrl) ->
    io:format("trying to init app from ~s~n", [AppPath]),
    try find_metadata(AppPath) of
        {'ok', MetaData} ->
            maybe_create_app(AppPath, wh_json:set_value(<<"api_url">>, AppUrl, MetaData));
        {'invalid_data', _E} ->
            io:format("  failed to validate app data ~s: ~p~n", [AppPath, _E])
    catch
        'error':{'badmatch', {'error', 'enoent'}} ->
            io:format("  failed to incorporate app because there was no app.json in ~s~n"
                      ,[filename:join([AppPath, <<"metadata">>])]
                     );
        'error':_E ->
            io:format("  failed to find metadata in ~s: ~p~n", [AppPath, _E])
    end.

-spec maybe_create_app(file:filename(), wh_json:object()) -> 'ok'.
-spec maybe_create_app(file:filename(), wh_json:object(), ne_binary()) -> 'ok'.
maybe_create_app(AppPath, MetaData) ->
    {'ok', MasterAccountDb} = whapps_util:get_master_account_db(),
    maybe_create_app(AppPath, MetaData, MasterAccountDb).

maybe_create_app(AppPath, MetaData, MasterAccountDb) ->
    AppName = wh_json:get_value(<<"name">>, MetaData),
    case find_app(MasterAccountDb, AppName) of
        {'ok', _JObj} -> io:format(" app ~s already loaded in system~n", [AppName]);
        {'error', 'not_found'} -> create_app(AppPath, MetaData, MasterAccountDb);
        {'error', _E} -> io:format(" failed to find app ~s: ~p", [AppName, _E])
    end.

-spec find_app(ne_binary(), ne_binary()) -> {'ok', wh_json:object()} |
                                            {'error', _}.
find_app(Db, Name) ->
    case couch_mgr:get_results(Db, ?CB_APPS_STORE_LIST, [{'key', Name}]) of
        {'ok', []} -> {'error', 'not_found'};
        {'ok', [View]} -> {'ok', View};
        {'error', _}=E -> E
    end.

-spec create_app(file:filename(), wh_json:object(), ne_binary()) -> 'ok'.
create_app(AppPath, MetaData, MasterAccountDb) ->
    Doc = wh_json:delete_keys([<<"source_url">>]
                              ,wh_doc:update_pvt_parameters(MetaData, MasterAccountDb, [{'type', <<"app">>}])
                             ),
    case couch_mgr:save_doc(MasterAccountDb, Doc) of
        {'ok', JObj} ->
            io:format(" saved app ~s as doc ~s~n", [wh_json:get_value(<<"name">>, JObj)
                                                    ,wh_doc:id(JObj)
                                                   ]),
            maybe_add_icons(AppPath, wh_doc:id(JObj), MasterAccountDb);
        {'error', _E} ->
            io:format(" failed to save app ~s to ~s: ~p~n", [wh_json:get_value(<<"name">>, MetaData), MasterAccountDb, _E])
    end.

-spec maybe_add_icons(file:filename(), ne_binary(), ne_binary()) -> 'ok'.
maybe_add_icons(AppPath, AppId, MasterAccountDb) ->
    try find_icons(AppPath) of
        {'ok', Icons} -> add_icons(AppId, MasterAccountDb, Icons)
    catch
        'error':{'badmatch', {'error', 'enoent'}} ->
            io:format("  failed to find icons in ~s~n", [AppPath]);
        'error':_E ->
            io:format("  failed to load icons from ~s: ~p~n", [AppPath, _E])
    end.

-spec add_icons(ne_binary(), ne_binary(), wh_proplist()) -> 'ok'.
add_icons(AppId, MasterAccountDb, Icons) ->
    _ = [add_icon(AppId, MasterAccountDb, IconId, IconData) || {IconId, IconData} <- Icons],
    'ok'.

-spec add_icon(ne_binary(), ne_binary(), file:filename(), binary()) -> 'ok'.
add_icon(AppId, MasterAccountDb, IconId, IconData) ->
    case couch_mgr:put_attachment(MasterAccountDb, AppId, IconId, IconData) of
        {'ok', _} ->     io:format("   saved ~s to ~s~n", [IconId, AppId]);
        {'error', _E} -> io:format("   failed to save ~s to ~s: ~p~n", [IconId, AppId, _E])
    end.

-spec find_icons(file:filename()) -> {'ok', [{file:filename(), binary()}]}.
find_icons(AppPath) ->
    {'ok', Dirs} = file:list_dir(AppPath),
    case lists:member("icon", Dirs) of
        'true' ->  read_icons(filename:join([AppPath, <<"icon">>]));
        'false' -> read_icons(filename:join([AppPath, <<"metadata">>, <<"icon">>]))
    end.

-spec read_icons(file:filename()) -> {'ok', [{file:filename(), binary()}]}.
read_icons(IconPath) ->
    {'ok', Icons} = file:list_dir(IconPath),
    {'ok', [{Icon, read_icon(IconPath, Icon)} || Icon <- Icons]}.

-spec read_icon(file:filename(), file:filename()) -> binary().
read_icon(Path, File) ->
    {'ok', IconData} = file:read_file(filename:join([Path, File])),
    IconData.

-spec find_metadata(file:filename()) -> {'ok', wh_json:object()} | {'invalid_data', wh_proplist()}.
find_metadata(AppPath) ->
    AppJSONPath = filename:join([AppPath, <<"metadata">>, <<"app.json">>]),
    {'ok', JSON} = file:read_file(AppJSONPath),
    JObj = wh_json:decode(JSON),
    {'ok', Schema} = wh_json_schema:load(<<"app">>),
    case jesse:validate_with_schema(Schema, wh_json:public_fields(JObj)) of
        {'ok', _}=OK -> OK;
        {'error', Errors} ->
            {'invalid_data', [Error || {'data_invalid', _, Error, _, _} <- Errors]}
    end.
