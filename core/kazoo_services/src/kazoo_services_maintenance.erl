%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_services_maintenance).

-export([flush/0]).
-export([credit/2]).
-export([debit/2]).
-export([refresh/0]).
-export([reconcile/0, reconcile/1
        ,remove_orphaned_services/0
        ]).
-export([sync/1]).
-export([sync_descendants/1]).
-export([make_reseller/1]).
-export([demote_reseller/1]).
-export([cascade_reseller_id/2]).
-export([set_reseller_id/2]).
-export([rebuild_services_db/0
        ,attempt_services_recovery/0
        ]).

-include("services.hrl").

-define(KZ_SERVICES_DB_TMP, <<"services_backup">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec flush() -> 'ok'.
flush() ->
    kz_services:flush_services().

%%------------------------------------------------------------------------------
%% @doc Add arbitrary credit to an account, without charging the accounts
%% credit card
%% @end
%%------------------------------------------------------------------------------
-spec credit(kz_term:ne_binary(), kz_term:text()) -> 'no_return'.
credit(AccountId, Amount) ->
    Units = wht_util:dollars_to_units(Amount),

    case create_transaction(kz_transaction:credit(AccountId, Units)) of
        'ok' ->
            lager:info("credited account ~p $~p", [AccountId, Amount]);
        {'error', _R} ->
            lager:info("failed to credit account: ~s: ~p", [AccountId, _R])
    end,
    'no_return'.

%%------------------------------------------------------------------------------
%% @doc Add arbitrary debit to an account, without charging the accounts
%% debit card
%% @end
%%------------------------------------------------------------------------------
-spec debit(kz_term:ne_binary(), kz_term:text()) -> 'no_return'.
debit(AccountId, Amount) ->
    Units = wht_util:dollars_to_units(Amount),

    case create_transaction(kz_transaction:debit(AccountId, Units)) of
        'ok' -> lager:info("debited account ~s $~p", [AccountId, Amount]);
        {'error', _R} ->
            lager:info("failed to debit account ~s: ~p"
                      ,[AccountId, _R]
                      )
    end,
    'no_return'.

-spec create_transaction(kz_transaction:kz_transaction()) ->
                                'ok' |
                                {'error', any()}.
create_transaction(Transaction) ->
    Routines = [fun admin_discretion/1
               ,fun admin_description/1
               ],
    T = lists:foldl(fun(F, T) -> F(T) end
                   ,Transaction
                   ,Routines
                   ),
    case kz_transaction:save(T) of
        {'ok', _} -> 'ok';
        {'error', _R}=E -> E
    end.

-spec admin_discretion(kz_transaction:kz_transaction()) ->
                              kz_transaction:kz_transaction().
admin_discretion(T) ->
    kz_transaction:set_reason(wht_util:admin_discretion(), T).

-spec admin_description(kz_transaction:kz_transaction()) ->
                               kz_transaction:kz_transaction().
admin_description(T) ->
    kz_transaction:set_description(<<"system administrator credit modification">>, T).

%%------------------------------------------------------------------------------
%% @doc maintenance function for the services db
%% @end
%%------------------------------------------------------------------------------
-spec refresh() -> 'ok'.
refresh() ->
    kz_datamgr:db_create(?KZ_SERVICES_DB),
    kz_datamgr:revise_docs_from_folder(?KZ_SERVICES_DB, 'kazoo_services', "views").

%%------------------------------------------------------------------------------
%% @doc schedules an eventual sync with the bookkeeper and will dirty the
%% full reseller tree (as it normally does when changes occur)
%% @end
%%------------------------------------------------------------------------------

-spec reconcile() -> 'no_return'.
reconcile() ->
    reconcile('all').

-spec reconcile(kz_term:text()) -> 'no_return'.
reconcile('all') ->
    Accounts = kapps_util:get_all_accounts('raw'),
    Total = length(Accounts),
    _ = lists:foldr(fun(Account, Current) ->
                            io:format("reconcile services (~p/~p) '~s'~n", [Current, Total, Account]),
                            _ = reconcile(Account),
                            Current + 1
                    end, 1, Accounts),
    'no_return';
reconcile(Account) when not is_binary(Account) ->
    reconcile(kz_term:to_binary(Account));
reconcile(Account) ->
    try kz_services:reconcile(Account)
    catch
        _E:_R ->
            io:format("failed to reconcile account ~s(~p):~n  ~p~n", [Account, _E, _R])
    end.

%%------------------------------------------------------------------------------
%% @doc runs an immediate sync with a bookkeeper without dirtying the
%% reseller tree (only the one account is affected)
%% @end
%%------------------------------------------------------------------------------
-spec sync(kz_term:text()) -> 'ok'.
sync(Account) when not is_binary(Account) ->
    sync(kz_term:to_binary(Account));
sync(Account) ->
    _ = kz_services:sync(Account),
    'ok'.

-spec sync_descendants(kz_term:text()) -> 'ok'.
sync_descendants(Account) when not is_binary(Account) ->
    sync_descendants(kz_term:to_binary(Account));
sync_descendants(Account) ->
    Descendants = kapps_util:account_descendants(Account),
    io:format("syncing ~p descendants of ~s", [length(Descendants), Account]),
    do_sync_descendants(Descendants).

-spec do_sync_descendants(kz_term:ne_binaries()) -> 'ok'.
do_sync_descendants([]) -> 'ok';
do_sync_descendants([Descendant|Descendants]) ->
    io:format("  syncing ~s, ~p accounts remaining", [Descendant, length(Descendants)]),
    _ = sync(Descendant),
    do_sync_descendants(Descendants).

%%------------------------------------------------------------------------------
%% @doc Set the reseller_id to the provided value on the provided account
%% @end
%%------------------------------------------------------------------------------
-spec set_reseller_id(kz_term:text(), kz_term:text()) -> 'ok'.
set_reseller_id(Reseller, Account) when not is_binary(Account) ->
    set_reseller_id(Reseller, kz_term:to_binary(Account));
set_reseller_id(Reseller, Account) when not is_binary(Reseller) ->
    set_reseller_id(kz_term:to_binary(Reseller), Account);
set_reseller_id(Reseller, Account) ->
    whs_account_conversion:set_reseller_id(Reseller, Account).

%%------------------------------------------------------------------------------
%% @doc Set the reseller_id to the provided value on all the sub-accounts
%% of the provided account
%% @end
%%------------------------------------------------------------------------------
-spec cascade_reseller_id(kz_term:text(), kz_term:text()) -> 'ok'.
cascade_reseller_id(Reseller, Account) when not is_binary(Account) ->
    cascade_reseller_id(Reseller, kz_term:to_binary(Account));
cascade_reseller_id(Reseller, Account) when not is_binary(Reseller) ->
    cascade_reseller_id(kz_term:to_binary(Reseller), Account);
cascade_reseller_id(Reseller, Account) ->
    whs_account_conversion:cascade_reseller_id(Reseller, Account).

%%------------------------------------------------------------------------------
%% @doc Remove reseller status from an account and set all its sub accounts
%% to the next higher reseller
%% @end
%%------------------------------------------------------------------------------
-spec demote_reseller(kz_term:text()) -> 'ok'.
demote_reseller(Account) when not is_binary(Account) ->
    demote_reseller(kz_term:to_binary(Account));
demote_reseller(Account) ->
    whs_account_conversion:demote(Account).

%%------------------------------------------------------------------------------
%% @doc Set the reseller status on the provided account and update all
%% sub accounts
%% @end
%%------------------------------------------------------------------------------
-spec make_reseller(kz_term:text()) -> 'ok'.
make_reseller(Account) when not is_binary(Account) ->
    make_reseller(kz_term:to_binary(Account));
make_reseller(Account) ->
    whs_account_conversion:promote(Account).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec attempt_services_recovery() -> 'ok'.
attempt_services_recovery() ->
    JObjs = fetch_all_service_docs(?KZ_SERVICES_DB_TMP),
    _ = kapps_maintenance:refresh(?KZ_SERVICES_DB),
    {'ok', Results} = kz_datamgr:save_docs(?KZ_SERVICES_DB, JObjs),
    log_services_recovery_results(Results).

-spec log_services_recovery_results(kz_json:objects()) -> 'ok'.
log_services_recovery_results([]) -> 'ok';
log_services_recovery_results([JObj|JObjs]) ->
    Id = kz_doc:id(JObj),
    _ = case kz_json:get_value(<<"reason">>, JObj) of
            'undefined' -> io:format("  restored document ~s~n", [Id]);
            Reason -> io:format("  failed to restore ~s: ~s~n", [Id, Reason])
        end,
    log_services_recovery_results(JObjs).

-spec rebuild_services_db() -> 'ok'.
rebuild_services_db() ->
    JObjs = fetch_all_service_docs(?KZ_SERVICES_DB),
    'ok' = backup_service_docs(JObjs),
    'true' = compare_services_backup(),
    _ = rebuild_services_db(JObjs),
    case compare_services_backup() of
        'true' -> io:format("rebuild completed successfully~n", []);
        'false' ->
            io:format("discrepancies found between restored and backed up service documents!~n", []),
            io:format("try: sup kazoo_services_maintenance attempt_services_recovery~n", [])
    end.

-spec rebuild_services_db(kz_json:objects()) -> 'ok'.
rebuild_services_db(JObjs) ->
    io:format("removing services database~n", []),
    kz_datamgr:db_delete(?KZ_SERVICES_DB),
    timer:sleep(5000),
    io:format("rebuilding services database views~n", []),
    kapps_maintenance:refresh(?KZ_SERVICES_DB),
    io:format("restoring service documents~n", []),
    kz_datamgr:save_docs(?KZ_SERVICES_DB, JObjs),
    io:format("rebuild complete~n", []).

-spec compare_services_backup() -> boolean().
compare_services_backup() ->
    _ = io:format("comparing original and backed up service documents~n", []),
    Originals = [{kz_doc:id(JObj), JObj} || JObj <- fetch_all_service_docs(?KZ_SERVICES_DB)],
    Backups = [{kz_doc:id(JObj), JObj} || JObj <- fetch_all_service_docs(?KZ_SERVICES_DB_TMP)],
    lists:all(fun({Id, Original}) -> kz_json:are_equal(props:get_value(Id, Backups), Original) end, Originals).

-spec backup_service_docs(kz_json:objects()) -> 'ok' | 'error'.
backup_service_docs(JObjs) ->
    _ = io:format("saving all service documents to ~s~n", [?KZ_SERVICES_DB_TMP]),
    'true' = kz_datamgr:db_create(?KZ_SERVICES_DB_TMP),
    {'ok', Results} = kz_datamgr:save_docs(?KZ_SERVICES_DB_TMP, JObjs),
    case find_services_backup_failures(Results) of
        [] -> 'ok';
        Errors -> log_services_backup_failures(Errors)
    end.

-spec log_services_backup_failures(kz_json:objects()) -> 'error'.
log_services_backup_failures([]) -> 'error';
log_services_backup_failures([JObj|JObjs]) ->
    _ = io:format("  failed to backup ~s: ~s~n"
                 ,[kz_doc:id(JObj)
                  ,kz_json:get_value(<<"reason">>, JObj)
                  ]
                 ),
    log_services_backup_failures(JObjs).

-spec find_services_backup_failures(kz_json:objects()) -> kz_json:objects().
find_services_backup_failures(JObjs) ->
    [JObj || JObj <- JObjs, kz_json:get_ne_value(<<"error">>, JObj) =/= 'undefined'].

-spec fetch_all_service_docs(kz_term:ne_binary()) -> kz_json:objects().
fetch_all_service_docs(Database) ->
    _ = io:format("fetching all service docs from '~s'~n", [Database]),
    {'ok', JObjs} = kz_datamgr:all_docs(Database, ['include_docs']),
    [prepare_service_doc(JObj) || JObj <- JObjs, not_design_doc(JObj)].

-spec not_design_doc(kz_json:object()) -> boolean().
not_design_doc(JObj) ->
    case kz_doc:id(JObj) of
        <<"_design", _/binary>> -> 'false';
        _Else -> 'true'
    end.

-spec prepare_service_doc(kz_json:object()) -> kz_json:object().
prepare_service_doc(JObj) ->
    Doc = kz_json:get_value(<<"doc">>, JObj),
    kz_json:delete_key(<<"_rev">>, Doc).

-spec remove_orphaned_services() -> 'no_return'.
remove_orphaned_services() ->
    {'ok', ServiceDocs} = kz_datamgr:all_docs(?KZ_SERVICES_DB),
    Count = lists:foldl(fun maybe_remove_orphan/2, 0, ServiceDocs),
    Count > 0
        andalso io:format("removed ~p service docs~n", [Count]),
    'no_return'.

-spec maybe_remove_orphan(kz_json:object() | kz_term:ne_binary(), non_neg_integer()) ->
                                 non_neg_integer().
maybe_remove_orphan(<<"_design/", _/binary>>, Count) -> Count;
maybe_remove_orphan(<<_/binary>> = AccountId, Count) ->
    case kzd_accounts:fetch(AccountId) of
        {'ok', _AccountDoc} -> Count;
        {'error', 'not_found'} ->
            {'ok', _} = kz_datamgr:del_doc(?KZ_SERVICES_DB, AccountId),
            io:format("account ~s not found, removing services~n", [AccountId]),
            Count+1
    end;
maybe_remove_orphan(ViewResult, Count) ->
    maybe_remove_orphan(kz_doc:id(ViewResult), Count).
