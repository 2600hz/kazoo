%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2016-2019, 2600Hz
%%% @doc
%%% @author Pierre Fenoll
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_number_manager_maintenance).

-include("knm.hrl").

-export([app_using/2]).
-export([generate_js_classifiers/1]).

-export([carrier_module_usage/0
        ,carrier_module_usage/1
        ]).
-export([convert_carrier_module/2
        ,convert_carrier_module/3
        ,convert_carrier_module_number/2
        ]).
-export([refresh_numbers_dbs/0
        ,refresh_numbers_db/1
        ]).
-export([init_dbs/0
        ,register_views/0
        ]).


-export([copy_accounts_to_number_dbs/0
        ,copy_accounts_to_number_dbs/1
        ,copy_single_account_to_number_dbs/1
        ]).
-export([copy_number_dbs_to_accounts/0
        ,copy_number_dbs_to_accounts/1
        ,copy_single_number_db_to_accounts/1
        ]).
-export([copy_assigned_number_dbs_to_account/1
        ,copy_single_assigned_number_db_to_account/2
        ]).

-export([copy_single_number_to_account_db/2]).

-export([fix_apps_for_account_dbs/0
        ,fix_apps_for_account_dbs/1
        ,fix_apps_for_single_account_db/1
        ]).
-export([fix_apps_for_number_dbs/0
        ,fix_apps_for_number_dbs/1
        ,fix_apps_for_single_number_db/1
        ]).
-export([fix_apps_in_number_dbs_for_accounts/1
        ,fix_apps_in_number_dbs_for_single_account/1
        ]).

-export([remove_wrong_assigned_from_accounts/0
        ,remove_wrong_assigned_from_single_accountdb/1
        ]).


-export([fix_account_db_numbers/1]).


-export([migrate/0
        ,migrate_unassigned_numbers/0, migrate_unassigned_numbers/1
        ]).
-export([generate_numbers/4]).
-export([delete/1]).
-export([purge_discovery/0, purge_discovery/1]).
-export([purge_deleted/0, purge_deleted/1]).
-export([update_number_services_view/1]).

-export([all_features/0]).
-export([feature_permissions_on_number/1]).
-export([add_allowed_feature_on_number/2
        ,remove_allowed_feature_on_number/2
        ,add_denied_feature_on_number/2
        ,remove_denied_feature_on_number/2
        ]).
-export([feature_permissions_on_reseller_of/1]).
-export([add_allowed_feature_on_reseller_of/2
        ,remove_allowed_feature_on_reseller_of/2
        ,add_denied_feature_on_reseller_of/2
        ,remove_denied_feature_on_reseller_of/2
        ]).
-export([feature_permissions_on_system_config/0]).
-export([add_allowed_feature_on_system_config/1
        ,remove_allowed_feature_on_system_config/1
        ,reset_allowed_features_to_defaults_on_system_config/0
        ]).
-export([ensure_adminonly_features_are_reachable/0]).

-export([migrate_port_requests/0]).

-define(TIME_BETWEEN_ACCOUNTS_MS
       ,kapps_config:get_pos_integer(?KNM_CONFIG_CAT, <<"time_between_accounts_ms">>, ?MILLISECONDS_IN_SECOND)).

-define(TIME_BETWEEN_NUMBERS_MS
       ,kapps_config:get_pos_integer(?KNM_CONFIG_CAT, <<"time_between_numbers_ms">>, ?MILLISECONDS_IN_SECOND)).

-define(TIME_BETWEEN_DBS_MS
       ,kapps_config:get_pos_integer(?KNM_CONFIG_CAT, <<"time_between_db_ms">>, 50)
       ).

-define(PARALLEL_JOBS_COUNT
       ,kapps_config:get_pos_integer(?KNM_CONFIG_CAT, <<"parallel_jobs_count">>, 1)
       ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec carrier_module_usage() -> 'ok'.
carrier_module_usage() ->
    carrier_module_usage(knm_util:get_all_number_dbs(), dict:new()).

-spec carrier_module_usage(kz_term:text()) -> 'ok'.
carrier_module_usage(Prefix) ->
    Database = knm_converters:to_db(Prefix),
    carrier_module_usage([Database], dict:new()).

-spec carrier_module_usage(kz_term:ne_binaries(), dict:dict()) -> 'ok'.
carrier_module_usage([], Totals) ->
    io:format("Totals:~n", []),
    F = fun (Module, Count) -> io:format("    ~s: ~p~n", [Module, Count]) end,
    _ = dict:map(F, Totals),
    'ok';
carrier_module_usage([Database|Databases], Totals0) ->
    Totals1 = get_carrier_module_usage(Database, Totals0),
    carrier_module_usage(Databases, Totals1).

-spec get_carrier_module_usage(kz_term:ne_binary(), dict:dict()) -> dict:dict().
get_carrier_module_usage(Database, Totals) ->
    io:format("~s:~n", [Database]),
    ViewOptions = ['reduce', 'group'],
    {'ok', JObjs} = kz_datamgr:get_results(Database, <<"numbers/module_name">>, ViewOptions),
    log_carrier_module_usage(JObjs, Database, Totals).

-spec log_carrier_module_usage(kz_json:objects(), kz_term:ne_binary(), dict:dict()) -> dict:dict().
log_carrier_module_usage([], _, Totals) -> Totals;
log_carrier_module_usage([JObj|JObjs], Database, Totals0) ->
    Module = kz_json:get_value(<<"key">>, JObj),
    Count = kz_json:get_value(<<"value">>, JObj),
    io:format("    ~s: ~p~n", [Module, Count]),
    Totals1 = dict:update_counter(Module, Count, Totals0),
    log_carrier_module_usage(JObjs, Database, Totals1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec convert_carrier_module(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
convert_carrier_module(Source, Target) ->
    convert_carrier_module_database(Source, Target, knm_util:get_all_number_dbs()).

-spec convert_carrier_module(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
convert_carrier_module(Source, Target, Prefix) ->
    Database = knm_converters:to_db(Prefix),
    convert_carrier_module_database(Source, Target, [Database]).

-spec convert_carrier_module_database(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binaries()) -> 'ok'.
convert_carrier_module_database(_, _, []) -> 'ok';
convert_carrier_module_database(Source, Target, [Database|Databases]) ->
    io:format("attempt to convert numbers with carrier module ~s to ~s in database ~s~n"
             ,[Source, Target, Database]
             ),
    ViewOptions = [{'reduce', 'false'}
                  ,{'key', Source}
                  ],
    {'ok', JObjs} = kz_datamgr:get_results(Database, <<"numbers/module_name">>, ViewOptions),
    convert_carrier_module_numbers([kz_doc:id(JObj) || JObj <- JObjs], Target),
    convert_carrier_module_database(Source, Target, Databases).

-spec convert_carrier_module_numbers(kz_term:ne_binaries(), kz_term:ne_binary()) -> 'ok'.
convert_carrier_module_numbers(Nums, Target) ->
    case lists:member(Target, knm_carriers:all_modules()) of
        'false' -> io:format("Bad carrier module: ~s\n", [Target]);
        'true' ->
            Routines = [{fun knm_phone_number:set_module_name/2, Target}],
            #{ok := Ns, ko := KOs} = knm_numbers:update(Nums, Routines),
            TotalLength = length(Nums),
            print_convert_carrier_success(Target, TotalLength, Ns),
            print_convert_carrier_failure(Target, TotalLength, KOs)
    end.

-spec print_convert_carrier_success(kz_term:ne_binary(), non_neg_integer(), kz_term:ne_binaries()) -> 'ok'.
print_convert_carrier_success(_, _, []) -> 'ok';
print_convert_carrier_success(Target, TotalLength, Converted) ->
    io:format("updated carrier module to ~s for ~b/~b number(s):\n", [Target, length(Converted), TotalLength]),
    F = fun (N) -> io:format("\t~s\n", [knm_phone_number:number(knm_number:phone_number(N))]) end,
    lists:foreach(F, Converted).

-spec print_convert_carrier_failure(kz_term:ne_binary(), non_neg_integer(), map()) -> 'ok'.
print_convert_carrier_failure(Target, TotalLength, KOs) ->
    case maps:size(KOs) of
        0 -> 'ok';
        KOsSize ->
            io:format("updating carrier module to ~s failed for ~b/~b number(s):\n", [Target, KOsSize, TotalLength]),
            F = fun (Num, R) -> io:format("\t~s: ~p\n", [Num, R]) end,
            _ = maps:map(F, KOs),
            'ok'
    end.

-spec convert_carrier_module_number(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
convert_carrier_module_number(Num, Target) ->
    convert_carrier_module_numbers([Num], Target).

-spec init_dbs() -> 'ok'.
init_dbs() ->
    init_dbs([?KZ_MANAGED_DB
             ,?KZ_INUM_DB
             ,?KZ_PORT_REQUESTS_DB
             ]).

init_dbs([]) -> 'ok';
init_dbs([Db | Dbs]) ->
    case kz_datamgr:db_exists(Db) of
        'false' ->
            Result = kz_datamgr:db_create(Db),
            lager:debug("~s is created: ~p", [Db, Result]);
        'true' -> 'ok'
    end,
    _ = kapps_maintenance:refresh(Db),
    init_dbs(Dbs).

-spec register_views() -> 'ok'.
register_views() ->
    kz_datamgr:register_views_from_folder(?APP).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec refresh_numbers_dbs() -> 'ok'.
refresh_numbers_dbs() ->
    NumberDbs = knm_util:get_all_number_dbs(),
    refresh_numbers_dbs(NumberDbs, length(NumberDbs)).

-spec refresh_numbers_dbs(kz_term:ne_binaries(), non_neg_integer()) -> 'ok'.
refresh_numbers_dbs([], _) -> 'ok';
refresh_numbers_dbs([NumberDb|NumberDbs], Total) ->
    ?SUP_LOG_DEBUG("(~p/~p) updating number db ~s", [length(NumberDbs) + 1, Total, NumberDb]),
    _ = refresh_numbers_db(NumberDb),
    refresh_numbers_dbs(NumberDbs, Total).

-spec refresh_numbers_db(kz_term:ne_binary()) -> 'ok'.
refresh_numbers_db(<<?KNM_DB_PREFIX_ENCODED, _/binary>> = NumberDb) ->
    _ = kapps_maintenance:refresh(NumberDb),
    'ok';
refresh_numbers_db(<<?KNM_DB_PREFIX, Suffix/binary>>) ->
    NumberDb = <<?KNM_DB_PREFIX_ENCODED, Suffix/binary>>,
    refresh_numbers_db(NumberDb);
refresh_numbers_db(<<"+", _/binary>> = Num) ->
    refresh_numbers_db(knm_converters:to_db(Num));
refresh_numbers_db(_Thing) ->
    ?SUP_LOG_DEBUG("skipping badly formed ~s", [_Thing]).

-spec update_number_services_view(kz_term:ne_binary()) -> 'no_return'.
update_number_services_view(?MATCH_ACCOUNT_RAW(AccountId)) ->
    update_number_services_view(kz_util:format_account_db(AccountId));
update_number_services_view(?MATCH_ACCOUNT_ENCODED(_)=AccountDb) ->
    FunMatchBlock = fun(Class) -> ["    resC['", Class, "'] = resM;"] end,
    MapView = number_services_map(FunMatchBlock),
    RedView = number_services_red(),
    ViewName = <<"_design/numbers">>,
    View = case kz_datamgr:open_doc(AccountDb, ViewName) of
               {'ok', JObj} -> JObj;
               {'error', _R} ->
                   lager:debug("reading account view ~s from disk (~p)", [ViewName, _R]),
                   {ViewName,JObj} = kapps_util:get_view_json('crossbar', <<"account/numbers.json">>),
                   JObj
           end,
    PathMap = [<<"views">>, <<"reconcile_services">>, <<"map">>],
    PathRed = [<<"views">>, <<"reconcile_services">>, <<"reduce">>],
    case MapView =:= kz_json:get_ne_binary_value(PathMap, View)
        andalso RedView =:= kz_json:get_ne_binary_value(PathRed, View)
    of
        'true' -> 'no_return';
        'false' ->
            NewView = kz_json:set_values([{PathMap, MapView}
                                         ,{PathRed, RedView}
                                         ]
                                        ,View
                                        ),
            _Updated = kz_datamgr:db_view_update(AccountDb, [{ViewName, NewView}]),
            ?SUP_LOG_DEBUG("view updated for '~s': ~p", [AccountDb, _Updated])
    end.

%%------------------------------------------------------------------------------
%% @doc Copy number docs from all accounts to their number dbs.
%% @end
%%------------------------------------------------------------------------------
-type loop_state() :: map().

-spec copy_accounts_to_number_dbs() -> 'ok'.
copy_accounts_to_number_dbs() ->
    copy_accounts_to_number_dbs(kapps_util:get_all_accounts()).

-spec copy_accounts_to_number_dbs(kz_term:ne_binaries()) -> 'ok'.
copy_accounts_to_number_dbs(AccountIds) ->
    ?SUP_LOG_DEBUG("::: start copying numbers doc from ~b account dbs to number dbs"
                  ,[length(AccountIds)]
                  ),
    State = accounts_to_numdbs_state([kz_util:format_account_db(A) || A <- AccountIds]),
    loop_dbs(State, fun copy_account_to_numdbs/2).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec copy_single_account_to_number_dbs(kz_term:ne_binary()) -> 'ok'.
copy_single_account_to_number_dbs(AccountId) ->
    _ = copy_accounts_to_number_dbs([AccountId]),
    'ok'.

%% @private
-spec accounts_to_numdbs_state(kz_term:ne_binaries()) -> loop_state().
accounts_to_numdbs_state(Dbs) ->
    maps:merge(loop_dbs_init()
              ,#{dbs => Dbs
                ,delete_revision => 'true'
                }
              ).

%% @private
-spec copy_account_to_numdbs(loop_state(), kz_term:ne_binary()) -> loop_state().
copy_account_to_numdbs(State, AccountDb) ->
    ViewOptions = [{'limit', kz_datamgr:max_bulk_read()}
                  ,'include_docs'
                  ],
    View = <<"numbers/list_by_number">>,
    Funs = [fun get_docs_from_result/2
           ,fun delete_revision/2
           ,fun split_docs_by_number_dbs/2
           ,fun save_to_dbs/2
           ],
    get_results_loop(State, AccountDb, View, ViewOptions, Funs).

%%------------------------------------------------------------------------------
%% @doc Copy number docs from all number dbs to their assigned account db.
%% @end
%%------------------------------------------------------------------------------
-spec copy_number_dbs_to_accounts() -> 'ok'.
copy_number_dbs_to_accounts() ->
    copy_number_dbs_to_accounts(knm_util:get_all_number_dbs()).

-spec copy_number_dbs_to_accounts(kz_term:ne_binaries()) -> 'ok'.
copy_number_dbs_to_accounts(NumberDbs) ->
    ?SUP_LOG_DEBUG("::: start copying numbers doc from ~b number dbs to account dbs"
                  ,[length(NumberDbs)]
                  ),
    loop_dbs(numdbs_to_accounts_state(NumberDbs), fun copy_numdb_to_accounts/2).

%%------------------------------------------------------------------------------
%% @doc Copy number docs from a single number db to their assigned account dbs.
%% @end
%%------------------------------------------------------------------------------
-spec copy_single_number_db_to_accounts(kz_term:ne_binary()) -> 'ok'.
copy_single_number_db_to_accounts(NumberDb) ->
    _ = copy_numdb_to_accounts(numdbs_to_accounts_state([NumberDb]), NumberDb),
    'ok'.

%% @private
-spec numdbs_to_accounts_state(kz_term:ne_binaries()) -> loop_state().
numdbs_to_accounts_state(NumberDbs) ->
    maps:merge(loop_dbs_init()
              ,#{dbs => NumberDbs
                ,delete_revision => 'true'
                ,retry_conflict => 'true'
                }
              ).

%% @private
-spec copy_numdb_to_accounts(loop_state(), kz_term:ne_binary()) -> loop_state().
copy_numdb_to_accounts(State, NumberDb) ->
    ViewOptions = [{'limit', kz_datamgr:max_bulk_read()}
                  ,'include_docs'
                  ],
    copy_numdb_to_accounts(State, NumberDb, ViewOptions).

%% @private
-spec copy_numdb_to_accounts(loop_state(), kz_term:ne_binary(), kz_term:proplist()) -> loop_state().
copy_numdb_to_accounts(State, NumberDb, ViewOptions) ->
    View = <<"numbers/assigned_to">>,
    Funs = [fun get_docs_from_result/2
           ,fun delete_revision/2
           ,fun split_docs_by_assigned_to/2
           ,fun save_to_dbs/2
           ],
    get_results_loop(State, NumberDb, View, ViewOptions, Funs).

%%------------------------------------------------------------------------------
%% @doc Copy all assigned number docs to an account from across all number dbs
%% to the account's db.
%% @end
%%------------------------------------------------------------------------------
-spec copy_assigned_number_dbs_to_account(kz_term:ne_binary()) -> 'ok'.
copy_assigned_number_dbs_to_account(Account) ->
    NumberDbs = knm_util:get_all_number_dbs(),
    ?SUP_LOG_DEBUG("::: start copying assigned numbers doc from ~b number dbs to account ~s"
                  ,[length(NumberDbs), Account]
                  ),
    AccountId = kz_util:format_account_id(Account),
    ViewOptions = [{'limit', kz_datamgr:max_bulk_read()}
                  ,{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ,'include_docs'
                  ],
    State = numdbs_to_accounts_state(NumberDbs),
    loop_dbs(State, fun(StateAcc, Db) -> copy_numdb_to_accounts(StateAcc, Db, ViewOptions) end).

-spec copy_single_assigned_number_db_to_account(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
copy_single_assigned_number_db_to_account(Account, NumberDb) ->
    AccountId = kz_util:format_account_id(Account),
    ViewOptions = [{'limit', kz_datamgr:max_bulk_read()}
                  ,{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ,'include_docs'
                  ],
    State = numdbs_to_accounts_state([NumberDb]),
    _ = copy_numdb_to_accounts(State, NumberDb, ViewOptions),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec copy_single_number_to_account_db(kz_term:ne_binary(), kz_term:ne_binary()) -> {'ok' | 'error', kz_term:ne_binary()}.
copy_single_number_to_account_db(Num, Account) ->
    AccountId = kz_util:format_account_id(Account),
    AccountDb = kz_util:format_account_db(AccountId),
    Number = knm_converters:normalize(Num),
    NumberDb = knm_converters:to_db(Number),
    case kz_datamgr:open_doc(NumberDb, Number) of
        {'ok', JObj} ->
            NewJObj = case kz_datamgr:lookup_doc_rev(AccountDb, kz_doc:id(JObj)) of
                          {'ok', Rev} -> kz_doc:set_revision(JObj, Rev);
                          {'error', _} -> kz_doc:delete_revision(JObj)
                      end,
            case kz_datamgr:save_doc(AccountDb, NewJObj) of
                {'ok', _} -> {'ok', kz_doc:id(JObj)};
                {'error', Reason} ->
                    Error = kz_term:to_binary(
                              io_lib:format("can save number '~s' to account db: ~p"
                                           ,[kz_doc:id(JObj), Reason]
                                           )
                             ),
                    {'error', Error}
            end;
        {'error', Reason} ->
            Error = kz_term:to_binary(
                      io_lib:format("can read number '~s' from '~s': ~p"
                                   ,[Number, NumberDb, Reason]
                                   )
                     ),
            {'error', Error}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fix_apps_for_account_dbs() -> 'ok'.
fix_apps_for_account_dbs() ->
    fix_apps_for_account_dbs(kapps_util:get_all_accounts()).

-spec fix_apps_for_account_dbs(kz_term:ne_binaries()) -> 'ok'.
fix_apps_for_account_dbs(AccountDbs) ->
    ?SUP_LOG_DEBUG("::: start fixing numbers doc used_by field for ~b account dbs"
                  ,[length(AccountDbs)]
                  ),
    State = apps_fix_state([kz_util:format_account_db(A) || A <- AccountDbs]),
    loop_dbs(State, fun fix_apps_for_account/2).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fix_apps_for_single_account_db(kz_term:ne_binary()) -> 'ok'.
fix_apps_for_single_account_db(Account) ->
    fix_apps_for_account_dbs([Account]).

%% @private
-spec apps_fix_state(kz_term:ne_binaries()) -> loop_state().
apps_fix_state(Dbs) ->
    maps:merge(loop_dbs_init()
              ,#{dbs => Dbs
                ,apps_fixers => #{}
                }
              ).

%% @private
-spec fix_apps_for_account(loop_state(), kz_term:ne_binary()) -> loop_state().
fix_apps_for_account(State, AccountDb) ->
    ViewOptions = [{'limit', kz_datamgr:max_bulk_read()}
                  ],
    View = <<"numbers/list_by_app">>,
    Funs = [fun check_app_usage/2
           ,fun open_docs_from_dbs/2
           ,fun apply_apps_fixers/2
           ,fun save_to_dbs/2
           ],
    NewState = get_results_loop(State, AccountDb, View, ViewOptions, Funs),
    NewState#{apps_fixers => #{}}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fix_apps_for_number_dbs() -> 'ok'.
fix_apps_for_number_dbs() ->
    fix_apps_for_number_dbs(knm_util:get_all_number_dbs()).

-spec fix_apps_for_number_dbs(kz_term:ne_binaries()) -> 'ok'.
fix_apps_for_number_dbs(NumberDbs) ->
    ?SUP_LOG_DEBUG("::: start fixing numbers doc used_by field for ~b number dbs"
                  ,[length(NumberDbs)]
                  ),
    State = apps_fix_state(NumberDbs),
    loop_dbs(State, fun fix_apps_for_number_db/2).

-spec fix_apps_for_single_number_db(kz_term:ne_binary()) -> 'ok'.
fix_apps_for_single_number_db(NumberDb) ->
    _ = fix_apps_for_number_dbs([NumberDb]),
    'ok'.

%% @private
-spec fix_apps_for_number_db(loop_state(), kz_term:ne_binary()) -> loop_state().
fix_apps_for_number_db(State, NumberDb) ->
    ViewOptions = [{'limit', kz_datamgr:max_bulk_read()}
                  ],
    View = <<"numbers/list_assigned_and_app">>,
    Funs = [fun split_by_list_assigned_and_app/2
           ,fun check_app_usage/2
           ,fun open_docs_from_dbs/2
           ,fun apply_apps_fixers/2
           ,fun save_to_dbs/2
           ],
    NewState = get_results_loop(State, NumberDb, View, ViewOptions, Funs),
    NewState#{apps_fixers => #{}}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fix_apps_in_number_dbs_for_accounts(kz_term:ne_binaries()) -> 'ok'.
fix_apps_in_number_dbs_for_accounts(Accounts) ->
    NumberDbs = knm_util:get_all_number_dbs(),
    ?SUP_LOG_DEBUG("::: start fixing numbers doc used_by field in ~b number dbs for ~b accounts"
                  ,[length(NumberDbs), length(Accounts)]
                  ),
    AccountIds = [kz_util:format_account_id(A) || A <- Accounts],
    State = apps_fix_state(NumberDbs),
    Fun = fun(StateAcc, NumberDb) ->
                  fix_apps_in_number_dbs_for_account(StateAcc, NumberDb, length(AccountIds), AccountIds)
          end,
    loop_dbs(State, Fun).

-spec fix_apps_in_number_dbs_for_single_account(kz_term:ne_binary()) -> 'ok'.
fix_apps_in_number_dbs_for_single_account(Account) ->
    fix_apps_in_number_dbs_for_accounts([Account]).

%% @private
-spec fix_apps_in_number_dbs_for_account(loop_state(), kz_term:ne_binary(), non_neg_integer(), kz_term:ne_binaries()) ->
                                                loop_state().
fix_apps_in_number_dbs_for_account(State, _, _, []) ->
    State;
fix_apps_in_number_dbs_for_account(State, NumberDb, Total, [AccountId|AccountIds]) ->
    ViewOptions = [{'limit', kz_datamgr:max_bulk_read()}
                  ,{'startkey', [AccountId]}
                  ,{'endkey', [AccountId, kz_json:new()]}
                  ],
    ?SUP_LOG_DEBUG("==> (~p/~p) fixing numbers app usage for account ~s"
                  ,[length(AccountIds) + 1, Total, AccountId]
                  ),

    View = <<"numbers/list_assigned_and_app">>,
    Funs = [fun split_by_list_assigned_and_app/2
           ,fun check_app_usage/2
           ,fun open_docs_from_dbs/2
           ,fun apply_apps_fixers/2
           ,fun save_to_dbs/2
           ],
    NewState = get_results_loop(State, NumberDb, View, ViewOptions, Funs),
    fix_apps_in_number_dbs_for_account(NewState#{apps_fixers => #{}}
                                      ,NumberDb
                                      ,Total
                                      ,AccountIds
                                      ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec remove_wrong_assigned_from_accounts() -> 'ok'.
remove_wrong_assigned_from_accounts() ->
    remove_wrong_assigned_from_accounts(kapps_util:get_all_accounts()).

-spec remove_wrong_assigned_from_accounts(kz_term:ne_binaries()) -> 'ok'.
remove_wrong_assigned_from_accounts(AccountDbs) ->
    ?SUP_LOG_DEBUG("::: start removing wrong assigned numbers from ~b account dbs"
                  ,[length(AccountDbs)]
                  ),
    State = remove_accountdb_wrong_assigned_state([kz_util:format_account_db(A) || A <- AccountDbs]),
    loop_dbs(State, fun loop_remove_bad_assignment_account/2).

-spec remove_wrong_assigned_from_single_accountdb(kz_term:ne_binary()) -> 'ok'.
remove_wrong_assigned_from_single_accountdb(Account) ->
    remove_wrong_assigned_from_accounts([kz_util:format_account_db(Account)]).

%% @private
-spec remove_accountdb_wrong_assigned_state(kz_term:ne_binaries()) -> loop_state().
remove_accountdb_wrong_assigned_state(Dbs) ->
    maps:merge(loop_dbs_init()
              ,#{dbs => Dbs}
              ).

%% @private
-spec loop_remove_bad_assignment_account(loop_state(), kz_term:ne_binary()) -> loop_state().
loop_remove_bad_assignment_account(State, AccountDb) ->
    ViewOptions = [{'limit', kz_datamgr:max_bulk_read()}
                  ],
    View = <<"numbers/assigned_to">>,
    Funs = [fun do_remove_wrong_assigned_from_account/2],
    get_results_loop(State, AccountDb, View, ViewOptions, Funs).

%% @private
-spec do_remove_wrong_assigned_from_account(loop_state(), kz_term:ne_binary()) -> loop_state().
do_remove_wrong_assigned_from_account(#{todo := []}=State, _) ->
    ?SUP_LOG_DEBUG("     no wrong assignment to remove"),
    State;
do_remove_wrong_assigned_from_account(#{todo := JObjs}=State, AccountDb) ->
    ?SUP_LOG_DEBUG("     ~b wrong assignments to remove", [length(JObjs)]),
    AccountId = kz_util:format_account_id(AccountDb),
    ToRemove = [kz_doc:id(JObj)
                || JObj <- JObjs,
                   kz_json:get_value(<<"key">>, JObj) =/= AccountId
               ],
    _ = kz_datamgr:del_docs(AccountDb, ToRemove),
    State#{todo => []}.

%%------------------------------------------------------------------------------
%% @private
%% @end
%%------------------------------------------------------------------------------
-spec loop_dbs_init() -> loop_state().
loop_dbs_init() ->
    #{ko => #{}
     ,todo => []
     }.

-type loop_db_fun() :: fun((loop_state(), kz_term:ne_binary()) -> loop_state()).
-type loop_db_funs() :: [loop_db_fun()].

%% @private
-spec loop_dbs(loop_state(), loop_db_fun()) -> 'ok'.
loop_dbs(Options, Fun) ->
    Dbs = lists:usort(maps:get('dbs', Options, [])),
    loop_dbs(maps:merge(loop_dbs_init(), Options#{dbs => Dbs})
            ,length(maps:get('dbs', Options))
            ,?TIME_BETWEEN_DBS_MS
            ,Fun
            ).

%% @private
-spec loop_dbs(loop_state(), non_neg_integer(), non_neg_integer(), loop_db_fun()) -> 'ok'.
loop_dbs(#{dbs := []}=State, _, _, _) ->
    report_loop_state(State);
loop_dbs(#{dbs := [Db | Dbs]}=State, Total, SleepTime, Fun) ->
    ?SUP_LOG_DEBUG("(~p/~p) processing '~s' database"
                  ,[length(Dbs) + 1, Total, Db]
                  ),
    NewState = Fun(State#{dbs => Dbs}, Db),
    _ = timer:sleep(SleepTime),
    loop_dbs(NewState, Total, SleepTime, Fun).

-spec report_loop_state(loop_state()) -> 'ok'.
report_loop_state(#{ko := KO}) when map_size(KO) > 0 ->
    ?SUP_LOG_DEBUG("::: done. see console log for error summary"),
    io:put_chars(
      kz_term:to_binary(
        ["\n\n"
        ,kz_json:encode(kz_json:from_list(
                          [Prop
                           || {_, V}=Prop <- maps:to_list(KO),
                              kz_term:is_not_empty(V)
                          ]
                         )
                       ,[pretty]
                       )
        ,"\n"
        ]
       )
     );
report_loop_state(_) ->
    ?SUP_LOG_DEBUG("::: done successfully~n").

%%------------------------------------------------------------------------------
%% @private
%% @end
%%------------------------------------------------------------------------------
-spec get_results_loop(loop_state(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist(), loop_db_funs()) ->
                              loop_state().
get_results_loop(State, Db, View, ViewOpts, Funs) ->
    get_results_loop(State#{get_loop_count => 1}, Db, View, ViewOpts, Funs, 2).

%% @private
-spec get_results_loop(loop_state(), kz_term:ne_binary(), kz_term:ne_binary()
                      ,kz_term:proplist(), loop_db_funs(), non_neg_integer()
                      ) -> loop_state().
get_results_loop(#{ko := KO, get_loop_count := LoopCount}=State, Db, _View, _, _, Retries) when Retries =< 0 ->
    DbKO = maps:get(Db, KO, #{}),
    ?SUP_LOG_DEBUG(" -- loop ~b: maximum retries to get results: ~100p"
                  ,[LoopCount, maps:get('last_error', DbKO)]
                  ),
    State#{todo => []
          ,get_loop_count => 0
          };
get_results_loop(#{get_loop_count := LoopCount}=State, Db, View, ViewOpts, Funs, Retries) ->
    %% to print which retries is this. max retries is set above to 2
    ?SUP_LOG_DEBUG("  -- loop ~b: getting results from ~s ~s (try: ~b)"
                  ,[LoopCount, Db, View, 3 - Retries]
                  ),
    Results = kz_datamgr:get_results(Db, View, ViewOpts),
    handle_get_results(State, Db, View, ViewOpts, Funs, Retries, Results).

%% @private
-spec handle_get_results(loop_state(), kz_term:ne_binary(), kz_term:ne_binary()
                        ,kz_term:proplist(), loop_db_funs(), non_neg_integer()
                        ,kazoo_data:get_results_return()
                        ) -> loop_state().
handle_get_results(#{get_loop_count := LoopCount}=State, _Db, _View, _, _, _, {'ok', []}) ->
    ?SUP_LOG_DEBUG("  -- loop ~b: no more results", [LoopCount]),
    State#{todo => []
          ,get_loop_count => 0
          };
handle_get_results(#{get_loop_count := LoopCount}=State, Db, View, ViewOpts, Funs, Retries, {'ok', JObjs}) ->
    NewState = apply_funs_on_results(State, Db, Funs, JObjs),
    NewViewOptions = props:set_value('skip', props:get_integer_value('skip', ViewOpts, 0) + length(JObjs), ViewOpts),
    get_results_loop(NewState#{get_loop_count => LoopCount + 1}, Db, View, NewViewOptions, Funs, Retries);
handle_get_results(#{ko := KO, get_loop_count := LoopCount}=State, Db, View, ViewOpts, Funs, Retries, {'error', 'timeout'}) ->
    ?SUP_LOG_DEBUG("  -- timeout to get results, maybe trying again..."),

    _ = timer:sleep(?MILLISECONDS_IN_SECOND),

    DbKO = maps:get(Db, KO, #{}),
    NewState = State#{ko => KO#{Db => DbKO#{'last_error' => <<"get_results-", View/binary, "-timeout">>}}
                     ,todo => []
                     ,get_loop_count => LoopCount + 1
                     },
    get_results_loop(NewState, Db, View, ViewOpts, Funs, Retries - 1);
handle_get_results(#{ko := KO}=State, Db, View, _, _, _, {'error', Reason}) ->
    ?SUP_LOG_DEBUG("  -- failed to get results: ~100p"
                  ,[Reason]
                  ),
    DbKO = maps:get(Db, KO, #{}),
    Error = to_binary_data_error(Reason),
    State#{ko => KO#{Db => DbKO#{'last_error' => <<"get_results-", View/binary, "-", Error/binary>>}}
          ,todo => []
          ,get_loop_count => 0
          }.

-spec apply_funs_on_results(loop_state(), kz_term:ne_binary(), loop_db_funs(), kz_json:objects()) -> loop_state().
apply_funs_on_results(State, Db, Funs, Results) ->
    ApplyFun = fun(Fun, StateAcc) -> Fun(StateAcc, Db) end,
    (lists:foldl(ApplyFun, State#{todo => Results}, Funs))#{todo => []}.

%%------------------------------------------------------------------------------
%% @private These function are working on JObjs
%% @end
%%------------------------------------------------------------------------------
-spec get_docs_from_result(loop_state(), kz_term:ne_binary()) -> loop_state().
get_docs_from_result(#{todo := JObjs}=State, _Db) ->
    State#{todo => [kz_json:get_value(<<"doc">>, JObj) || JObj <- JObjs]}.

%% @private see note above
-spec delete_revision(loop_state(), kz_term:ne_binary()) -> loop_state().
delete_revision(#{todo := Todos
                 ,delete_revision := 'true'
                 }=State, _Db) ->
    ?SUP_LOG_DEBUG("     delete revision from ~b documents", [length(Todos)]),
    State#{todo => [kz_doc:delete_revision(JObj) || JObj <- Todos]};
delete_revision(State, _) ->
    State.

%%------------------------------------------------------------------------------
%% @private These function are working on JObjs
%% @end
%%------------------------------------------------------------------------------
-spec split_docs_by_number_dbs(loop_state(), kz_term:ne_binary()) -> loop_state().
split_docs_by_number_dbs(#{todo := Todos}=State, _Db) ->
    ?SUP_LOG_DEBUG("     grouping ~b docs by number db", [length(Todos)]),
    F = fun (JObj, M) ->
                NumberDb = knm_converters:to_db(knm_converters:normalize(kz_doc:id(JObj))),
                M#{NumberDb => [JObj | maps:get(NumberDb, M, [])]}
        end,
    State#{todo => maps:to_list(lists:foldl(F, #{}, Todos))}.

%% @private see note above
-spec split_docs_by_assigned_to(loop_state(), kz_term:ne_binary()) -> loop_state().
split_docs_by_assigned_to(#{todo := Todos}=State, _Db) ->
    ?SUP_LOG_DEBUG("     grouping ~b docs by assigned_to", [length(Todos)]),
    F = fun (JObj, M) ->
                AccountDb = kz_util:format_account_db(
                              kz_json:get_value(<<"pvt_assigned_to">>, JObj)
                             ),
                M#{AccountDb => [JObj | maps:get(AccountDb, M, [])]}
        end,
    State#{todo => maps:to_list(lists:foldl(F, #{}, Todos))}.

-spec split_by_list_assigned_and_app(loop_state(), kz_term:ne_binary()) -> loop_state().
split_by_list_assigned_and_app(#{todo := Todos}=State, _Db) ->
    ?SUP_LOG_DEBUG("     grouping ~b docs by assigned_to", [length(Todos)]),
    F = fun (JObj, M) ->
                [AssginedTo, _PvtUsedBy] = kz_json:get_value(<<"key">>, JObj),
                AccountDb = kz_util:format_account_db(AssginedTo),
                M#{AccountDb => [JObj | maps:get(AccountDb, M, [])]}
        end,
    State#{todo => maps:to_list(lists:foldl(F, #{}, Todos))}.

%%------------------------------------------------------------------------------
%% @private We don't save fixed apps to another db, it should always save to
%% whatever db it reads from. So DONT group todo by {db, objects}!
%%
%% These function are working either JObjs or `[{Db, JObjs}]'
%% @end
%%------------------------------------------------------------------------------
-spec check_app_usage(loop_state(), kz_term:ne_binary()) -> loop_state().
check_app_usage(#{todo := []}=State, _Db) ->
    ?SUP_LOG_DEBUG("     no numbers to check apps usage", []),
    State;
check_app_usage(#{todo := [{_AccountDb, _JObjs}|_]=Todos}=State, _NumberDb) ->
    ?SUP_LOG_DEBUG("     check apps usage for ~b numbers", [length(Todos)]),
    check_app_usage_fold(State#{todo => []}, Todos);
check_app_usage(#{todo := JObjs}=State, AccountDb) ->
    ?SUP_LOG_DEBUG("     check apps usage for ~b numbers", [length(JObjs)]),
    check_app_usage_fold(State#{todo => []}, [{AccountDb, JObjs}]).

%% @private see note above
-spec check_app_usage_fold(loop_state(), [{kz_term:ne_binary(), kz_json:objects()}]) -> loop_state().
check_app_usage_fold(State, []) ->
    State;
check_app_usage_fold(#{todo := Todo
                      ,apps_fixers := Fixers
                      }=State
                    ,[{AccountDbForEver, JObjs}|Rest]
                    ) ->
    Ids = [kz_doc:id(JObj) || JObj <- JObjs],
    AppsUsing = get_account_dids_apps(AccountDbForEver, Ids),

    DbFixers = create_apps_fixer(JObjs, AppsUsing, #{}),

    check_app_usage_fold(State#{todo => Todo ++ maps:keys(DbFixers)
                               ,apps_fixers => maps:merge(Fixers, DbFixers)
                               }
                        ,Rest
                        ).

%% @private
-spec create_apps_fixer(kz_json:objects(), map(), map()) -> map().
create_apps_fixer([], _, Acc) ->
    Acc;
create_apps_fixer([JObj|JObjs], AppsUsing, Fixers) ->
    Number = kz_doc:id(JObj),
    NumUsedBy = case kz_json:get_value(<<"key">>, JObj, 'null') of
                    [_AssignedTo, App] -> App; %% from number_db
                    App -> App %% from account_db
                end,
    UsedByApp = maps:get(Number, AppsUsing, 'undefined'),
    create_apps_fixer(JObjs, AppsUsing, maybe_fix_used_by(Number, NumUsedBy, UsedByApp, Fixers)).

%%------------------------------------------------------------------------------
%% @private
%% @end
%%------------------------------------------------------------------------------
-spec apply_apps_fixers(loop_state(), kz_term:ne_binary()) -> loop_state().
apply_apps_fixers(#{todo := []}=State, _Db) ->
    ?SUP_LOG_DEBUG("     no numbers to fix apps usage", []),
    State;
apply_apps_fixers(#{todo := Todos}=State, _Db) ->
    apply_apps_fixers_fold(State#{todo => []}, Todos, #{}).

%% @private
-spec apply_apps_fixers_fold(loop_state(), kz_term:proplist(), map()) -> loop_state().
apply_apps_fixers_fold(State, [], Acc) ->
    State#{todo => maps:to_list(Acc)};
apply_apps_fixers_fold(#{apps_fixers := AppsFixers}=State, [{Db, JObjs}|Rest], Acc) ->
    ?SUP_LOG_DEBUG("     applying app usage fixes on ~b numbers for account ~s"
                  ,[length(JObjs), kz_util:format_account_id(Db)]
                  ),
    Todo = [(maps:get(kz_doc:id(JObj), AppsFixers, fun kz_term:identity/1))(JObj)
            || JObj <- JObjs
           ],
    apply_apps_fixers_fold(State, Rest, Acc#{Db => Todo}).

%%------------------------------------------------------------------------------
%% @private
%% @end
%%------------------------------------------------------------------------------
-spec open_docs_from_dbs(loop_state(), kz_term:ne_binary()) -> loop_state().
open_docs_from_dbs(#{todo := []}=State, _) ->
    State;
open_docs_from_dbs(#{todo := Todos}=State, _Db) ->
    open_docs_from_dbs_fold(State#{todo := []}, Todos, #{}).

%% @private
-spec open_docs_from_dbs_fold(loop_state(), kz_term:proplist(), map()) -> loop_state().
open_docs_from_dbs_fold(State, [], Acc) ->
    State#{todo => maps:to_list(Acc)};
open_docs_from_dbs_fold(#{ko := KO}=State, [{Db, Ids}|Rest], Acc) ->
    ?SUP_LOG_DEBUG("     opening ~b documents from ~s", [length(Ids), Db]),
    case kz_datamgr:open_docs(Db, Ids) of
        {'ok', JObjs} ->
            Docs = [kz_json:get_value(<<"doc">>, JObj)
                    || JObj <- JObjs,
                       kz_json:get_value(<<"error">>, JObjs) =:= 'undefined'
                   ],
            open_docs_from_dbs_fold(State, Rest, Acc#{Db => Docs});
        {'error', Reason} ->
            ?SUP_LOG_ERROR("       failed to open docs, skipping db: ~100p", [Reason]),
            Error = to_binary_data_error(Reason),
            DbKO = maps:get(Db, KO, #{}),
            DbFailed = maps:get(failed, DbKO, #{}),
            NewFailed = merge_error_num_ids(<<"open_doc-",Error/binary>>, Ids, DbFailed),
            NewState = State#{ko => KO#{Db => add_failed_to_db_ko(DbKO, NewFailed)}},
            open_docs_from_dbs_fold(NewState, Rest, Acc)
    end.

%%------------------------------------------------------------------------------
%% @private
%% @end
%%------------------------------------------------------------------------------
-spec save_to_dbs(loop_state(), kz_term:ne_binary()) -> loop_state().
save_to_dbs(#{todo := []}=State, _Db) ->
    ?SUP_LOG_DEBUG("     nothing to save"),
    State;
save_to_dbs(#{todo := [{_Db, _JObjs}|_]=Todos}=State, _OtherDb) ->
    ?SUP_LOG_DEBUG("     start saving to ~b dbs", [length(Todos)]),
    save_to_dbs(State#{todo => []}, length(Todos), Todos, 2);
save_to_dbs(#{todo := JObjs}=State, Db) ->
    ?SUP_LOG_DEBUG("     start saving to 1 db"),
    save_to_dbs(State#{todo => []}, 1, [{Db, JObjs}], 2).

-spec save_to_dbs(loop_state(), non_neg_integer(), kz_term:proplist(), non_neg_integer()) -> loop_state().
save_to_dbs(State, _, [], _) ->
    State;
save_to_dbs(#{ko := KO}=State, Total, [{Db, JObjs}|Rest], Retries) when Retries =< 0 ->
    DbKO = maps:get(Db, KO, #{}),
    DbFailed = maps:get(failed, DbKO, #{}),
    LastError = maps:get('last_error', DbKO),

    ?SUP_LOG_DEBUG("       reached to maximum retries to save db: ~100p", [Db, LastError]),

    NewFailed = merge_error_num_ids(<<"save_docs-", LastError/binary>>, [kz_doc:id(J) || J <- JObjs], DbFailed),
    NewState = State#{ko => KO#{Db => maps:remove(last_error, add_failed_to_db_ko(DbKO, NewFailed))}},
    save_to_dbs(NewState, Total, Rest, 2);
save_to_dbs(#{ko := KO}=State, Total, [{Db, JObjs} | Rest], Retries) ->
    ?SUP_LOG_DEBUG("       (~b/~b) saving ~b documents to ~s (try: ~b)"
                  ,[length(Rest) + 1, Total, length(JObjs), Db, 3 - Retries]
                  ),

    DbKO = maps:get(Db, KO, #{}),
    DbFailed = maps:get(failed, DbKO, #{}),
    IsNumberDb = 'numbers' =:= kz_datamgr:db_classification(Db),

    case kz_datamgr:save_docs(Db, JObjs) of
        {'ok', Saved} ->
            NewState = handle_bulk_save_errors(State, Db, Saved, JObjs, 'false'),
            save_to_dbs(NewState, Total, Rest, 2);
        {'error', 'not_found'} when IsNumberDb ->
            ?SUP_LOG_DEBUG("         creating number db ~s", [Db]),
            _ = kz_datamgr:db_create(Db),
            _ = kapps_maintenance:refresh(Db),
            NewState = State#{ko => KO#{Db => DbKO#{last_error => <<"save_docs-db_not_found">>}}},

            _ = timer:sleep(?MILLISECONDS_IN_SECOND),

            save_to_dbs(NewState, Total, [{Db, JObjs} | Rest], Retries - 1);
        {'error', 'not_found'} ->
            ?SUP_LOG_DEBUG("         db not found"),
            NewFailed = merge_error_num_ids(<<"save_docs-db_not_found">>, [kz_doc:id(J) || J <- JObjs], DbFailed),
            NewState = State#{ko => KO#{Db => add_failed_to_db_ko(DbKO, NewFailed)}},
            save_to_dbs(NewState, Total, Rest, 2);
        {'error', 'timeout'} ->
            ?SUP_LOG_DEBUG("         timeout to save to db, maybe trying again..."),
            NewState = State#{ko => KO#{Db => DbKO#{last_error => <<"save_docs-timeout">>}}},

            _ = timer:sleep(?MILLISECONDS_IN_SECOND),

            save_to_dbs(NewState, Total, [{Db, JObjs} | Rest], Retries - 1);
        {'error', Reason} ->
            ?SUP_LOG_DEBUG("         failed to save documents to db: ~100p", [Reason]),
            Error = to_binary_data_error(Reason),
            NewFailed = merge_error_num_ids(<<"save_docs-", Error/binary>>, [kz_doc:id(J) || J <- JObjs], DbFailed),
            NewState = State#{ko => KO#{Db => add_failed_to_db_ko(DbKO, NewFailed)}},
            save_to_dbs(NewState, Total, Rest, 2)
    end.

%% @private
-spec handle_bulk_save_errors(loop_state(), kz_term:ne_binary(), kz_json:objects(), kz_json:objects(), boolean()) ->
                                     loop_state().
handle_bulk_save_errors(#{ko := KO}=State, Db, Saved, JObjs, IsConflictTry) ->
    DbKO = maps:get(Db, KO, #{}),
    DbFailed = maps:get(failed, DbKO, #{}),
    {ConflictSet, Failed} = split_by_error(Saved, gb_sets:new(), DbFailed, 0),

    ConflictSize = gb_sets:size(ConflictSet),

    case not IsConflictTry
        andalso maps:get('retry_conflict', State, 'false')
        andalso ConflictSize > 0
    of
        'true' ->
            ConflictJObjs = [JObj
                             || JObj <- JObjs,
                                gb_sets:is_element(kz_doc:id(JObj), ConflictSet)
                            ],
            save_conflicts_to_db(State#{ko => KO#{Db => add_failed_to_db_ko(DbKO, Failed)}}
                                ,Db
                                ,ConflictJObjs
                                );
        'false' when ConflictSize =:= 0 ->
            State#{ko => KO#{Db => add_failed_to_db_ko(DbKO, Failed)}};
        'false' ->
            NewFailed = merge_error_num_ids(<<"save_docs-conflict">>, gb_sets:to_list(ConflictSet), Failed),
            State#{ko => KO#{Db => add_failed_to_db_ko(DbKO, NewFailed)}}
    end.

add_failed_to_db_ko(DbKO, Failed) when map_size(Failed) =:= 0 ->
    DbKO;
add_failed_to_db_ko(DbKO, Failed) ->
    DbKO#{failed => Failed}.

%% @private
-spec merge_error_num_ids(map(), map()) -> map().
merge_error_num_ids(Old, New) ->
    maps:fold(fun merge_error_num_ids/3, Old, New).

%% @private
-spec merge_error_num_ids(kz_term:ne_binary(), kz_term:ne_binaries(), map()) -> map().
merge_error_num_ids(ErrorType, Ids, Acc) ->
    maps:update_with(ErrorType, fun(Nums) -> lists:flatten([Nums, Ids]) end, Ids, Acc).

%% @private
-spec split_by_error(kz_json:objects(), gb_sets:set(), map(), non_neg_integer()) ->
                            {gb_sets:set(), map()}.
split_by_error([], ConflictSet, Failed, TotalFailed) ->
    maybe_log_failed_saves(ConflictSet, TotalFailed),
    {ConflictSet, Failed};
split_by_error([JObj|JObjs], ConflictSet, Failed, TotalFailed) ->
    case kz_json:get_value(<<"error">>, JObj) of
        'undefined' -> split_by_error(JObjs, ConflictSet, Failed, TotalFailed);
        <<"conflict">> ->
            split_by_error(JObjs, gb_sets:add_element(kz_doc:id(JObj), ConflictSet), Failed, TotalFailed);
        Reason ->
            Error = to_binary_data_error(Reason),
            NewFailed = maps:update_with(<<"save_docs-", Error/binary>>, fun(Nums) -> [kz_doc:id(JObj)| Nums] end
                                        ,[kz_doc:id(JObj)]
                                        ,Failed
                                        ),
            split_by_error(JObjs, ConflictSet, NewFailed, TotalFailed + 1)
    end.

%% @private
-spec maybe_log_failed_saves(gb_sets:set(), non_neg_integer()) -> 'ok'.
maybe_log_failed_saves(ConflictSet, 0) ->
    case gb_sets:size(ConflictSet) of
        0 -> 'ok';
        Size ->
            ?SUP_LOG_DEBUG("         [conflict: ~b]", [Size])
    end;
maybe_log_failed_saves(ConflictSet, TotalFailed) when TotalFailed > 0 ->
    case gb_sets:size(ConflictSet) of
        0 ->
            ?SUP_LOG_DEBUG("         [failed: ~b]", [TotalFailed]);
        Size ->
            ?SUP_LOG_DEBUG("         [failed: ~b] [conflict: ~b]", [TotalFailed, Size])
    end.

%% @private
-spec save_conflicts_to_db(loop_state(), kz_term:ne_binary(), kz_json:objects()) ->
                                  loop_state().
save_conflicts_to_db(#{ko := KO}=State, Db, ConflictJObjs) ->
    ?SUP_LOG_DEBUG("         trying to ensure save ~b conflicts", [length(ConflictJObjs)]),
    DbKO = maps:get(Db, KO, #{}),
    DbFailed = maps:get(failed, DbKO, #{}),

    ConflictIds = [kz_doc:id(JObj) || JObj <- ConflictJObjs],

    {Errored, Revs} = get_conflicts_revs(Db, ConflictIds),

    DefinitelyFailed = merge_error_num_ids(DbFailed, Errored),

    ToSave = [kz_doc:set_revision(JObj, Rev)
              || JObj <- ConflictJObjs,
                 Rev <- [maps:get(kz_doc:id(JObj), Revs, 'undefined')],
                 Rev =/= 'undefined'
             ],
    _ = timer:sleep(100),
    ?SUP_LOG_DEBUG("           saving ~b conflicts", [length(ToSave)]),
    case kz_datamgr:save_docs(Db, ToSave) of
        {'ok', Saved} ->
            NewState = State#{ko => KO#{Db => add_failed_to_db_ko(DbKO, DefinitelyFailed)}},
            handle_bulk_save_errors(NewState, Db, Saved, ToSave, 'true');
        {'error', Reason} ->
            ?SUP_LOG_DEBUG("             nope, attempt failed: ~100p", [Reason]),
            Error = to_binary_data_error(Reason),
            MeowFailed = merge_error_num_ids(<<"try_save_conflicts-", Error/binary>>, ConflictIds, DefinitelyFailed),
            State#{ko => KO#{Db => DbKO#{failed => add_failed_to_db_ko(DbKO, MeowFailed)}}}
    end.

%% @private
-spec get_conflicts_revs(kz_term:ne_binary(), kz_term:ne_binaries()) ->
                                {map(), map()}.
get_conflicts_revs(Db, ConflictIds) ->
    ?SUP_LOG_DEBUG("           fetching ~b revs from ~s", [length(ConflictIds), Db]),
    case kz_datamgr:all_docs(Db, [{'keys', ConflictIds}]) of
        {'ok', JObjs} ->
            split_errors_opened_revs(JObjs, {#{}, #{}});
        {'error', Reason} ->
            ?SUP_LOG_DEBUG("             fetching revs from db failed: ~100p", [Reason]),
            Error = <<"fetch_rev-", (to_binary_data_error(Reason))/binary>>,
            {#{Error => ConflictIds}, #{}}
    end.

%% @private
-spec split_errors_opened_revs(kz_json:objects(), {map(), map()}) ->
                                      {map(), map()}.
split_errors_opened_revs([], Acc) -> Acc;
split_errors_opened_revs([JObj|JObjs], {Errors, Revs}) ->
    Id = kz_json:get_value(<<"key">>, JObj),
    case kz_json:get_value(<<"error">>, JObj) of
        'undefined' ->
            Acc = {Errors
                  ,Revs#{Id => kz_json:get_ne_binary_value([<<"value">>, <<"rev">>], JObj)}
                  },
            split_errors_opened_revs(JObjs, Acc);
        Reason ->
            Error = <<"fetch_rev-", Reason/binary>>,
            Acc = {Errors#{Error => [Id | maps:get(Error, Errors, [])]}
                  ,Revs
                  },
            split_errors_opened_revs(JObjs, Acc)
    end.

-spec to_binary_data_error(any()) -> kz_term:ne_binary().
to_binary_data_error(Bin) when is_binary(Bin) ->
    Bin;
to_binary_data_error(Atom) when is_atom(Atom) ->
    kz_term:to_binary(Atom);
to_binary_data_error(SomethingElse) ->
    kz_term:safe_cast(SomethingElse
                     ,<<"datastore_fault_not_mine">>
                     ,fun kz_term:to_binary/1
                     ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_account_dids_apps(kz_term:ne_binary(), kz_term:ne_binaries()) -> map().
get_account_dids_apps(Account, Ids) ->
    ViewOpts= [{'keys', Ids}],
    ?SUP_LOG_DEBUG("      getting app usage for ~b numbers in account ~s", [length(Ids), Account]),
    AccountDb = kz_util:format_account_db(Account),
    case get_dids_for_app(AccountDb, <<"callflow">>, ViewOpts) of
        {'ok', CallflowDIDs} ->
            case get_dids_for_app(AccountDb, <<"trunkstore">>, []) of
                {'ok', TrunkstoreDIDs} ->
                    handle_dids_app(CallflowDIDs, TrunkstoreDIDs);
                {'error', _R} ->
                    ?SUP_LOG_DEBUG("         failed to get trunkstore numbers (~100p), skipping..."
                                  ,[_R]
                                  ),
                    #{}
            end;
        {'error', _R} ->
            ?SUP_LOG_DEBUG("         failed to get callflow numbers (~100p), skipping..."
                          ,[_R]
                          ),
            #{}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec handle_dids_app(kz_term:ne_binaries(), kz_term:ne_binaries()) -> map().
handle_dids_app(CallflowDIDs, TrunkstoreDIDs) ->
    Callflow = gb_sets:from_list(CallflowDIDs),
    Trunk = gb_sets:from_list(TrunkstoreDIDs),
    Multi = gb_sets:intersection(Callflow, Trunk),
    case gb_sets:is_empty(Multi) of
        'true' ->
            maps:merge(maps:from_list([{D, <<"callflow">>} || D <- CallflowDIDs])
                      ,maps:from_list([{D, <<"trunkstore">>} || D <- TrunkstoreDIDs])
                      );
        'false' ->
            Msg = kz_term:to_binary(
                    io_lib:format("numbers used by both trunkstore and callflow: ~s"
                                 ,[kz_binary:join(gb_sets:to_list(Multi))]
                                 )
                   ),
            ?SUP_LOG_DEBUG("~s", [Msg]),
            maps:merge(maps:from_list(
                         [{D, <<"callflow">>}
                          || D <- CallflowDIDs,
                             not gb_sets:is_element(D, Multi)
                         ] ++ [{D, <<"calltrunkflow">>}
                               || D <- gb_sets:to_list(Multi)
                              ]
                        )
                      ,maps:from_list([{D, <<"trunkstore">>}
                                       || D <- TrunkstoreDIDs,
                                          not gb_sets:is_element(D, Multi)
                                      ])
                      )
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_fix_used_by(kz_term:ne_binary(), 'null' | kz_term:ne_binary(), kz_term:api_ne_binary(), map()) -> map().
maybe_fix_used_by(_, _, <<"calltrunkflow">>, ToFix) ->
    ToFix;
maybe_fix_used_by(_, 'null', 'undefined', ToFix) ->
    ToFix;
maybe_fix_used_by(Number, _YouAreWrongSir, 'undefined', ToFix) ->
    ToFix#{Number => delete_used_by_fun()};
maybe_fix_used_by(_, UsedBy, UsedBy, ToFix) ->
    ToFix;
maybe_fix_used_by(Number, _YouAreWrongSir, UsedByApp, ToFix) ->
    ToFix#{Number => add_used_by_fun(UsedByApp)}.

-spec add_used_by_fun(kz_term:ne_binary()) -> fun((kz_json:object()) -> kz_json:object()).
add_used_by_fun(UsedBy) ->
    fun(JObj) -> kz_json:set_value(<<"pvt_used_by">>, UsedBy, JObj) end.

-spec delete_used_by_fun() -> fun((kz_json:object()) -> kz_json:object()).
delete_used_by_fun() ->
    fun(JObj) -> kz_json:delete_key(<<"pvt_used_by">>, JObj) end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec fix_account_db_numbers(kz_term:ne_binary()) -> 'ok'.
fix_account_db_numbers(Account) ->
    ?SUP_LOG_DEBUG(":::: fixing account ~s, or not", [Account]),
    _ = copy_assigned_number_dbs_to_account(Account),
    _ = remove_wrong_assigned_from_single_accountdb(Account),

    _ = fix_apps_for_single_account_db(Account),
    _ = fix_apps_in_number_dbs_for_single_account(Account),
    _ = kz_services:reconcile(Account),
    'ok'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec migrate() -> 'ok'.
migrate() ->
    lager:info("ensuring admin-only features"),
    ensure_adminonly_features_are_reachable(),
    lager:info("refreshing number dbs"),
    _ = refresh_numbers_dbs(),
    lager:info("migrating unassigned numbers"),
    migrate_unassigned_numbers(),
    migrate_carrier_module_settings().

-spec migrate_unassigned_numbers() -> 'ok'.
migrate_unassigned_numbers() ->
    ?SUP_LOG_DEBUG("::: fixing unassigned numbers", []),
    _ = [migrate_unassigned_numbers(NumDb) || NumDb <- knm_util:get_all_number_dbs()],
    ?SUP_LOG_DEBUG("::: finished fixing unassigned numbers", []).

-spec migrate_unassigned_numbers(kz_term:ne_binary()) -> 'ok'.
migrate_unassigned_numbers(<<?KNM_DB_PREFIX_ENCODED, _/binary>> = NumberDb) ->
    ?SUP_LOG_DEBUG(" ==> start fixing ~s", [NumberDb]),
    migrate_unassigned_numbers(NumberDb, 0),
    ?SUP_LOG_DEBUG(" ==> done fixing ~s", [NumberDb]);
migrate_unassigned_numbers(<<?KNM_DB_PREFIX_encoded, Suffix/binary>>) ->
    migrate_unassigned_numbers(<<?KNM_DB_PREFIX_ENCODED, Suffix/binary>>);
migrate_unassigned_numbers(<<?KNM_DB_PREFIX, Suffix/binary>>) ->
    migrate_unassigned_numbers(<<?KNM_DB_PREFIX_ENCODED, Suffix/binary>>);
migrate_unassigned_numbers(Number) ->
    migrate_unassigned_numbers(knm_converters:to_db(Number)).

-spec migrate_unassigned_numbers(kz_term:ne_binary(), integer()) -> 'ok'.
migrate_unassigned_numbers(NumberDb, Offset) ->
    ViewOptions = [{'limit', kz_datamgr:max_bulk_insert()}
                  ,{'skip', Offset}
                  ],
    ?SUP_LOG_DEBUG("[~s] checking for unassigned numbers with offset ~b", [NumberDb, Offset]),
    case kz_datamgr:get_results(NumberDb, <<"numbers/unassigned">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', JObjs} ->
            Length = length(JObjs),
            ?SUP_LOG_DEBUG("[~s] fixing ~b docs", [NumberDb, Length]),
            fix_unassign_doc([kz_doc:id(JObj) || JObj <- JObjs]),
            timer:sleep(?TIME_BETWEEN_ACCOUNTS_MS),
            migrate_unassigned_numbers(NumberDb, Offset + Length);
        {'error', _R} ->
            ?SUP_LOG_DEBUG("failed to get unassigned DIDs from ~s: ~p", [NumberDb, _R])
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_dids_for_app(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()) -> kazoo_data:get_results_return().
get_dids_for_app(AccountDb, <<"callflow">>, ViewOptions) ->
    View = <<"callflows/listing_by_number">>,
    ?SUP_LOG_DEBUG("         getting callflow numbers from ~s", [AccountDb]),
    kz_datamgr:get_result_keys(AccountDb, View, ViewOptions);
get_dids_for_app(AccountDb, <<"trunkstore">>, ViewOptions) ->
    View = <<"trunkstore/lookup_did">>,
    ?SUP_LOG_DEBUG("         getting trunkstore numbers from ~s", [AccountDb]),
    kz_datamgr:get_result_keys(AccountDb, View, ViewOptions).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-ifndef(TEST).
-type dids() :: gb_sets:set(kz_term:ne_binary()).

-spec get_DIDs_callflow_set(kz_term:ne_binary(), kz_term:proplist()) -> dids().
get_DIDs_callflow_set(AccountDb, ViewOptions) ->
    case get_dids_for_app(AccountDb, <<"callflow">>, ViewOptions) of
        {'ok', DIDs} -> gb_sets:from_list(DIDs);
        {'error', _R} ->
            ?SUP_LOG_DEBUG("failed to get callflow DIDs from ~s: ~p", [AccountDb, _R]),
            gb_sets:new()
    end.

-spec get_DIDs_trunkstore_set(kz_term:ne_binary(), kz_term:proplist()) -> dids().
get_DIDs_trunkstore_set(AccountDb, ViewOptions) ->
    case get_dids_for_app(AccountDb, <<"trunkstore">>, ViewOptions) of
        {'ok', DIDs} -> gb_sets:from_list(DIDs);
        {'error', _R} ->
            ?SUP_LOG_DEBUG("failed to get trunkstore DIDs from ~s: ~p", [AccountDb, _R]),
            gb_sets:new()
    end.
-endif.

-spec app_using(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_ne_binary().
-ifdef(TEST).
app_using(?TEST_OLD7_NUM, ?CHILD_ACCOUNT_DB) -> <<"trunkstore">>;
app_using(?NE_BINARY, ?MATCH_ACCOUNT_ENCODED(_)) -> 'undefined'.
-else.
app_using(Num, AccountDb) ->
    CallflowNums = get_DIDs_callflow_set(AccountDb, [{'key', Num}]),
    TrunkstoreNums = get_DIDs_trunkstore_set(AccountDb, [{'key', Num}]),
    app_using(Num, AccountDb, CallflowNums, TrunkstoreNums).

-spec app_using(kz_term:ne_binary(), kz_term:ne_binary(), dids(), dids()) -> kz_term:api_ne_binary().
app_using(Num, _, CallflowNums, TrunkstoreNums) ->
    case gb_sets:is_element(Num, CallflowNums) of
        'true' -> <<"callflow">>;
        'false' ->
            case gb_sets:is_element(Num, TrunkstoreNums) of
                'true' -> <<"trunkstore">>;
                'false' -> 'undefined'
            end
    end.
-endif.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec escape(kz_term:ne_binary()) -> kz_term:ne_binary().
escape(?NE_BINARY=Bin0) ->
    StartSz = byte_size(Start= <<"<<">>),
    EndSz   = byte_size(End  = <<">>">>),
    Bin = iolist_to_binary(io_lib:format("~p", [Bin0])),
    SizeOfWhatIWant = byte_size(Bin) - (StartSz + EndSz),
    <<Start:StartSz/binary, Escaped:SizeOfWhatIWant/binary, End:EndSz/binary>> = Bin,
    Escaped.

-type js_match_block() :: fun((kz_term:ne_binary()) -> iolist()).
-spec number_services_map(js_match_block()) -> kz_term:ne_binary().
number_services_map(FunMatchBlock) ->
    iolist_to_binary(
      ["function(doc) {"
       "  if (doc.pvt_type != 'number' || doc.pvt_deleted) return;"
       "  var resM = {};"
       "  resM[doc.pvt_module_name] = 1;"
       "  var resC = {};"
      ,generate_js_classifiers(FunMatchBlock),
       "  var resF = {};"
       "  var used = doc.pvt_features || {};"
       "  for (var feature in used)"
       "    if (used.hasOwnProperty(feature))"
       "      resF[feature] = 1;"
       "  emit(doc._id, {'classifications':resC, 'features':resF});"
       "}"
      ]).

-spec generate_js_classifiers(js_match_block()) -> iolist().
generate_js_classifiers(FunMatchBlock) ->
    ClassifiersJObj = knm_converters:available_classifiers(), %%TODO: per-account classifiers.
    Pairs = [{Classification, kz_json:get_value([Classification, <<"regex">>], ClassifiersJObj)}
             || Classification <- kz_json:get_keys(ClassifiersJObj)
            ],
    {Classifications, Regexs} = lists:unzip(Pairs),
    generate_js_classifiers(Classifications, Regexs, FunMatchBlock).

-spec generate_js_classifiers(kz_term:ne_binaries(), kz_term:ne_binaries(), js_match_block()) -> iolist().
generate_js_classifiers(Classifications, Regexs, FunMatchBlock) ->
    ["  var e164 = doc._id;"
     "  if (false) return;"
    ,[["  else if (e164.match(", escape(Regex), "))"
      ,FunMatchBlock(Class)
      ]
      || {Class, Regex} <- lists:zip(Classifications, Regexs),
         is_binary(Regex)
     ]
    ].

-spec number_services_red() -> kz_term:ne_binary().
number_services_red() ->
    iolist_to_binary(
      ["function(Keys, Values, _Rereduce) {"
       "  var incr = function (o, k, v) {"
       "    o[k] = v + ((o[k] === undefined) ? 0 : o[k]);"
       "    return o;"
       "  };"
       "  var acc = function (Oout, Oin) {"
       "    for (var Ofield in Oin)"
       "      if (Oin.hasOwnProperty(Ofield))"
       "        Oout = incr(Oout, Ofield, Oin[Ofield]);"
       "    return Oout;"
       "  };"
       "  var resF = {};"
       "  var resC = {};"
       "  for (var i in Values) {"
       "    var Value = Values[i];"
       "    resF = acc(resF, Value['features'] || {});"
       "    var Classifications = Value['classifications'] || {};"
       "    for (var klass in Classifications) {"
       "      if (Classifications.hasOwnProperty(klass))"
       "        resC[klass] = acc(resC[klass] || {}, Classifications[klass]);"
       "    }"
       "  }"
       "  return {'classifications':resC, 'features':resF};"
       "}"
      ]).

-spec fix_unassign_doc(kz_term:ne_binaries()) -> 'ok'.
fix_unassign_doc(DIDs) ->
    Setters = [{fun knm_phone_number:set_used_by/2, 'undefined'}
              ,fun knm_phone_number:remove_denied_features/1
              ],
    Options = [{'auth_by', ?KNM_DEFAULT_AUTH_BY}
               %% No caching + bulk doc writes
              ,{'batch_run', 'true'}
              ],
    case knm_numbers:update(DIDs, Setters, Options) of
        #{ko := Map} when map_size(Map) =:= 0 -> 'ok';
        #{ko := KOs} ->
            ?SUP_LOG_DEBUG("failed fixing ~b unassigned numbers.", [maps:size(KOs)])
    end.

-spec generate_numbers(kz_term:ne_binary(), kz_term:ne_binary(), pos_integer(), non_neg_integer()) -> 'ok'.
generate_numbers(Type, AccountId, StartingNumber, Quantity) ->
    M = kz_term:to_atom(<<"knm_", Type/binary>>, 'true'),
    M:generate_numbers(AccountId, kz_term:to_integer(StartingNumber), kz_term:to_integer(Quantity)).


-spec delete(kz_term:ne_binary()) -> 'no_return'.
delete(Num) ->
    case knm_number:delete(Num, knm_number_options:default()) of
        {'ok', _} -> io:format("Removed ~s\n", [Num]);
        {'error', _R} -> io:format("ERROR: ~p\n", [_R])
    end,
    'no_return'.


-spec purge_discovery() -> 'no_return'.
purge_discovery() ->
    _ = [purge_number_db(NumDb, ?NUMBER_STATE_DISCOVERY) || NumDb <- knm_util:get_all_number_dbs()],
    'no_return'.

-spec purge_deleted(kz_term:ne_binary()) -> 'no_return'.
purge_deleted(Prefix) ->
    purge_number_db(<<?KNM_DB_PREFIX_ENCODED, Prefix/binary>>, ?NUMBER_STATE_DELETED),
    'no_return'.

-spec purge_deleted() -> 'no_return'.
purge_deleted() ->
    _ = [purge_number_db(NumDb, ?NUMBER_STATE_DELETED) || NumDb <- knm_util:get_all_number_dbs()],
    'no_return'.

-spec purge_discovery(kz_term:ne_binary()) -> 'no_return'.
purge_discovery(Prefix) ->
    purge_number_db(<<?KNM_DB_PREFIX_ENCODED, Prefix/binary>>, ?NUMBER_STATE_DISCOVERY),
    'no_return'.

-spec purge_number_db(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
purge_number_db(NumberDb, State) ->
    ViewOptions = [{'startkey', [State]}
                  ,{'endkey', [State, kz_json:new()]}
                  ,'include_docs'
                  ,{'limit', 500}
                  ],
    case kz_datamgr:get_results(NumberDb, <<"numbers/status">>, ViewOptions) of
        {'ok', []} -> 'ok';
        {'ok', Numbers} ->
            io:format("removing ~p numbers in state '~s' from ~s~n", [length(Numbers), State, NumberDb]),
            JObjs = [JObj || Number <- Numbers,
                             JObj <- [kz_json:get_value(<<"doc">>, Number)],
                             State =:= kz_json:get_value(?PVT_STATE, JObj)
                    ],
            _ = kz_datamgr:del_docs(NumberDb, JObjs),
            purge_number_db(NumberDb, State)
    end.

-spec is_feature_valid(any()) -> boolean().
is_feature_valid(Thing) ->
    lists:member(Thing, ?ALL_KNM_FEATURES).

-spec invalid_feature(kz_term:ne_binary()) -> 'no_return'.
invalid_feature(Feature) ->
    io:format("Feature '~s' is not a known feature.\n", [Feature]),
    all_features().

-spec all_features() -> 'no_return'.
all_features() ->
    io:format("Known features:\n\t~s\n", [list_features(?ALL_KNM_FEATURES)]),
    'no_return'.

-spec list_features(kz_term:ne_binaries()) -> iodata().
list_features(Features) ->
    kz_util:iolist_join($\s, Features).

-spec error_with_number(kz_term:ne_binary(), any()) -> 'no_return'.
error_with_number(Num, Error) ->
    Reason = case kz_json:is_json_object(Error) of
                 'false' -> Error;
                 'true' -> knm_errors:error(Error)
             end,
    io:format("Error with number ~s: ~s\n", [Num, Reason]),
    'no_return'.

-spec print_feature_permissions(kz_term:ne_binaries(), kz_term:ne_binaries()) -> 'no_return'.
print_feature_permissions(Allowed, Denied) ->
    io:format("\tFeatures allowed: ~s\n"
              "\tFeatures denied: ~s\n"
             ,[list_features(Allowed), list_features(Denied)]
             ),
    'no_return'.

-spec list_number_feature_permissions(knm_number:knm_number()) -> 'no_return'.
list_number_feature_permissions(N) ->
    PN = knm_number:phone_number(N),
    Num = knm_phone_number:number(PN),
    Allowed = knm_phone_number:features_allowed(PN),
    Denied = knm_phone_number:features_denied(PN),
    io:format("Feature permissions on ~s:\n", [Num]),
    print_feature_permissions(Allowed, Denied).

-spec edit_feature_permissions_on_number(kz_term:ne_binary(), fun(), kz_term:ne_binary()) -> 'no_return'.
edit_feature_permissions_on_number(Num, Fun, Feature) ->
    case is_feature_valid(Feature) of
        'false' -> invalid_feature(Feature);
        'true' ->
            Updates = [{Fun, Feature}],
            case knm_number:update(Num, Updates) of
                {'ok', N} -> list_number_feature_permissions(N);
                {'error', Error} -> error_with_number(Num, Error)
            end
    end.

-spec feature_permissions_on_number(kz_term:ne_binary()) -> 'no_return'.
feature_permissions_on_number(Num) ->
    case knm_number:get(Num) of
        {'error', Error} -> error_with_number(Num, Error);
        {'ok', N} -> list_number_feature_permissions(N)
    end.

-spec add_allowed_feature_on_number(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
add_allowed_feature_on_number(?NE_BINARY=Feature, ?NE_BINARY=Num) ->
    edit_feature_permissions_on_number(Num, fun knm_phone_number:add_allowed_feature/2, Feature).

-spec remove_allowed_feature_on_number(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
remove_allowed_feature_on_number(?NE_BINARY=Feature, ?NE_BINARY=Num) ->
    edit_feature_permissions_on_number(Num, fun knm_phone_number:remove_allowed_feature/2, Feature).

-spec add_denied_feature_on_number(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
add_denied_feature_on_number(?NE_BINARY=Feature, ?NE_BINARY=Num) ->
    edit_feature_permissions_on_number(Num, fun knm_phone_number:add_denied_feature/2, Feature).

-spec remove_denied_feature_on_number(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
remove_denied_feature_on_number(?NE_BINARY=Feature, ?NE_BINARY=Num) ->
    edit_feature_permissions_on_number(Num, fun knm_phone_number:remove_denied_feature/2, Feature).

-spec feature_permissions_on_reseller_of(kz_term:ne_binary()) -> 'no_return'.
feature_permissions_on_reseller_of(?MATCH_ACCOUNT_RAW(AccountId)) ->
    Allowed = empty_list_when_undefined(?FEATURES_ALLOWED_RESELLER(AccountId)),
    Denied = empty_list_when_undefined(?FEATURES_DENIED_RESELLER(AccountId)),
    ResellerId = kz_services_reseller:get_id(AccountId),
    io:format("Feature permissions on reseller of ~s (~s):\n", [AccountId, ResellerId]),
    print_feature_permissions(Allowed, Denied).

-spec empty_list_when_undefined(kz_term:api_list()) -> kz_term:ne_binaries().
empty_list_when_undefined('undefined') -> [];
empty_list_when_undefined(NeBinaries) -> NeBinaries.

-spec edit_allowed_feature_permissions_on_reseller_of(kz_term:ne_binary(), fun(), kz_term:ne_binary()) -> 'no_return'.
edit_allowed_feature_permissions_on_reseller_of(AccountId, Fun, Feature) ->
    case is_feature_valid(Feature) of
        'false' -> invalid_feature(Feature);
        'true' ->
            Allowed = empty_list_when_undefined(?FEATURES_ALLOWED_RESELLER(AccountId)),
            NewFeatures = lists:usort(Fun(Feature, Allowed)),
            ResellerId = kz_services_reseller:get_id(AccountId),
            _ = kapps_account_config:set(ResellerId, ?KNM_CONFIG_CAT, ?KEY_FEATURES_ALLOW, NewFeatures),
            feature_permissions_on_reseller_of(AccountId)
    end.

-spec edit_denied_feature_permissions_on_reseller_of(kz_term:ne_binary(), fun(), kz_term:ne_binary()) -> 'no_return'.
edit_denied_feature_permissions_on_reseller_of(AccountId, Fun, Feature) ->
    case is_feature_valid(Feature) of
        'false' -> invalid_feature(Feature);
        'true' ->
            Denied = empty_list_when_undefined(?FEATURES_DENIED_RESELLER(AccountId)),
            NewFeatures = lists:usort(Fun(Feature, Denied)),
            ResellerId = kz_services_reseller:get_id(AccountId),
            _ = kapps_account_config:set(ResellerId, ?KNM_CONFIG_CAT, ?KEY_FEATURES_DENY, NewFeatures),
            feature_permissions_on_reseller_of(AccountId)
    end.

-spec add_allowed_feature_on_reseller_of(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
add_allowed_feature_on_reseller_of(?NE_BINARY=Feature, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    Cons = fun (AFeature, Features) -> [AFeature|Features] end,
    edit_allowed_feature_permissions_on_reseller_of(AccountId, Cons, Feature).

-spec remove_allowed_feature_on_reseller_of(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
remove_allowed_feature_on_reseller_of(?NE_BINARY=Feature, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    edit_allowed_feature_permissions_on_reseller_of(AccountId, fun lists:delete/2, Feature).

-spec add_denied_feature_on_reseller_of(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
add_denied_feature_on_reseller_of(?NE_BINARY=Feature, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    Cons = fun (AFeature, Features) -> [AFeature|Features] end,
    edit_denied_feature_permissions_on_reseller_of(AccountId, Cons, Feature).

-spec remove_denied_feature_on_reseller_of(kz_term:ne_binary(), kz_term:ne_binary()) -> 'no_return'.
remove_denied_feature_on_reseller_of(?NE_BINARY=Feature, ?MATCH_ACCOUNT_RAW(AccountId)) ->
    edit_denied_feature_permissions_on_reseller_of(AccountId, fun lists:delete/2, Feature).

-spec feature_permissions_on_system_config() -> 'no_return'.
feature_permissions_on_system_config() ->
    Allowed = knm_providers:system_allowed_features(),
    io:format("Features allowed on system config document:\n\t~s\n", [list_features(Allowed)]),
    'no_return'.

-spec reset_allowed_features_to_defaults_on_system_config() -> 'no_return'.
reset_allowed_features_to_defaults_on_system_config() ->
    set_features_on_system_config(?DEFAULT_FEATURES_ALLOWED_SYSTEM).

-spec set_features_on_system_config(kz_term:ne_binaries()) -> 'no_return'.
set_features_on_system_config(Features) ->
    _ = kapps_config:set(?KNM_CONFIG_CAT, ?KEY_FEATURES_ALLOW, lists:usort(Features)),
    feature_permissions_on_system_config().

-spec edit_allowed_feature_permissions_on_system_config(fun(), kz_term:ne_binary()) -> 'no_return'.
edit_allowed_feature_permissions_on_system_config(Fun, Feature) ->
    case is_feature_valid(Feature) of
        'false' -> invalid_feature(Feature);
        'true' ->
            Allowed = knm_providers:system_allowed_features(),
            set_features_on_system_config(Fun(Feature, Allowed))
    end.

-spec add_allowed_feature_on_system_config(kz_term:ne_binary()) -> 'no_return'.
add_allowed_feature_on_system_config(?NE_BINARY=Feature) ->
    Cons = fun (AFeature, Features) -> [AFeature|Features] end,
    edit_allowed_feature_permissions_on_system_config(Cons, Feature).

-spec remove_allowed_feature_on_system_config(kz_term:ne_binary()) -> 'no_return'.
remove_allowed_feature_on_system_config(?NE_BINARY=Feature) ->
    edit_allowed_feature_permissions_on_system_config(fun lists:delete/2, Feature).

-spec ensure_adminonly_features_are_reachable() -> 'no_return'.
ensure_adminonly_features_are_reachable() ->
    Configured = knm_providers:system_allowed_features(),
    case lists:usort(?ADMIN_ONLY_FEATURES) -- Configured of
        [] -> 'no_return';
        ToAdd ->
            io:format("Adding the following admin-only number features to system_config: ~s"
                     ,[list_features(ToAdd)]),
            set_features_on_system_config(ToAdd ++ Configured)
    end.

-spec migrate_carrier_module_settings() -> 'ok'.
migrate_carrier_module_settings() ->
    %% carrier modules needing to migrate settings
    knm_vitelity_util:migrate().

-spec migrate_port_requests() -> 'ok'.
migrate_port_requests() ->
    knm_port_request:migrate().
