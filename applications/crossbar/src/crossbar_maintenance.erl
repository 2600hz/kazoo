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
-export([refresh/0, refresh/1]).
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

-include_lib("crossbar.hrl").

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
    io:format("updating default crossbar modules~n", []),
    _ = migrate_accounts_data(Accounts),
    Modules =
        [wh_util:to_atom(Module, 'true')
         || Module <- whapps_config:get(<<"crossbar">>, <<"autoload_modules">>, [])
        ],
    add_missing_modules(
      Modules,
      [Module
       || Module <- ?DEFAULT_MODULES
              ,(not lists:member(Module, Modules))
      ]).

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
    cb_clicktocall:maybe_migrate_history(Account),
    'no_return'.

-spec add_missing_modules(atoms(), atoms()) -> 'no_return'.
add_missing_modules(_, []) -> 'no_return';
add_missing_modules(Modules, MissingModules) ->
    _ = whapps_config:set(<<"crossbar">>, <<"autoload_modules">>, Modules ++ MissingModules),
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
    io:format("please use whapps_maintenance:refresh().", []).

refresh(Value) ->
    io:format("please use whapps_maintenance:refresh(~p).", [Value]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec start_module(text()) -> 'ok' | {'error', _}.
start_module(Module) ->
    try crossbar:start_mod(Module) of
        _ ->
            Mods = whapps_config:get(?CONFIG_CAT, <<"autoload_modules">>, []),
            whapps_config:set_default(?CONFIG_CAT, <<"autoload_modules">>, [wh_util:to_binary(Module)
                                                                            | lists:delete(wh_util:to_binary(Module), Mods)
                                                                           ]),
            io:format("started and added ~s to autoloaded modules~n", [Module])
    catch
        _E:_R ->
            io:format("failed to start ~s: ~s: ~p~n", [Module, _E, _R])
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec stop_module(text()) -> 'ok' | {'error', _}.
stop_module(Module) ->
    try crossbar:stop_mod(Module) of
        _ ->
            Mods = whapps_config:get(?CONFIG_CAT, <<"autoload_modules">>, []),
            whapps_config:set_default(?CONFIG_CAT, <<"autoload_modules">>, lists:delete(wh_util:to_binary(Module), Mods)),
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
                                    {'error', term()}.
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
                                  {'error', term()}.
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
                                   {'error', term()}.
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

        AccountDb = cb_context:account_db(C1),
        AccountId = cb_context:account_id(C1),

        case whapps_util:get_all_accounts() of
            [AccountDb] ->
                _ = promote_account(AccountId),
                _ = allow_account_number_additions(AccountId),
                _ = whistle_services_maintenance:make_reseller(AccountId),
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
    Payload = [Context#cb_context{req_data=JObj
                                  ,req_nouns=[{?WH_ACCOUNTS_DB, []}]
                                  ,req_verb = ?HTTP_PUT
                                  ,resp_status = 'fatal'
                                 }
              ],
    case crossbar_bindings:fold(<<"v1_resource.validate.users">>, Payload) of
        #cb_context{resp_status='success'}=Context1 -> {'ok', Context1};
        #cb_context{resp_data=Errors} ->
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
            AccountId = wh_json:get_value(<<"_id">>, cb_context:req_data(Context)),
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
                      ,[wh_json:get_value(<<"_id">>, cb_context:doc(Context1))]
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
-spec update_account(input_term(), ne_binary(), term()) ->
                            {'ok', wh_json:object()} |
                            {'error', term()}.
update_account(AccountId, Key, Value) when not is_binary(AccountId) ->
    update_account(wh_util:to_binary(AccountId), Key, Value);
update_account(AccountId, Key, Value) ->
    AccountDb = wh_util:format_account_id(AccountId, 'encoded'),
    Updaters = [fun({'error', _}=E) -> E;
                   ({'ok', J}) ->
                        couch_mgr:save_doc(AccountDb, wh_json:set_value(Key, Value, J))
                end
                ,fun({'error', _}=E) -> E;
                    ({'ok', J}) ->
                         case couch_mgr:lookup_doc_rev(?WH_ACCOUNTS_DB, AccountId) of
                             {'ok', Rev} ->
                                 couch_mgr:save_doc(?WH_ACCOUNTS_DB, wh_json:set_value(<<"_rev">>, Rev, J));
                             {'error', 'not_found'} ->
                                 couch_mgr:save_doc(?WH_ACCOUNTS_DB, wh_json:delete_key(<<"_rev">>, J))
                         end
                 end
               ],
    lists:foldl(fun(F, J) -> F(J) end
                ,couch_mgr:open_cache_doc(AccountDb, AccountId)
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
            io:format("  Name: ~s~n", [wh_json:get_value(<<"name">>, JObj)]),
            io:format("  Realm: ~s~n", [wh_json:get_value(<<"realm">>, JObj)]),
            io:format("  Enabled: ~s~n", [not wh_json:is_false(<<"pvt_enabled">>, JObj)]),
            io:format("  System Admin: ~s~n", [wh_json:is_true(<<"pvt_superduper_admin">>, JObj)]);
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
    io:format("can not move to the same account~n", []);
maybe_move_account(AccountId, ToAccountId) ->
    case crossbar_util:move_account(AccountId, ToAccountId) of
        {'ok', _} -> io:format("move complete!~n", []);
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
