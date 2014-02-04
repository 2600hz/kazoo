%%%-------------------------------------------------------------------
%%% @Copyright (c) 2010-2014, 2600Hz
%%% @doc
%%% Maintenance module for migrating CDRs to the new Transient
%%% Account database sharding structure.
%%% @end
%%% @contributors
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(cdr_maintenance).

%% API
-export([flush/0]).
-export([stop_v3_migrator/0
         ,create_test_migrate_accounts/0
         ,create_test_migrate_accounts/1
         ,create_test_migrate_accounts/2
         ,create_test_migrate_accounts/3
         ,delete_test_accounts/0
         ,start_v3_migrator/0
         ,start_v3_test_migrator/0
         ,get_v3_migrator_status/0
        ]).

-include("cdr.hrl").

-type input_term() :: pos_integer() | atom() | string() | ne_binary().

-define(DFLT_NUM_TEST_ACCOUNTS, 2).
-define(DFLT_NUM_MONTHS_LGCY_DATA, 6).
-define(DFLT_NUM_CDR_PER_DAY, 4).

%%%===================================================================
%%% API
%%%===================================================================
-spec flush() -> 'ok'.
flush() ->
    wh_cache:flush_local(?CDR_CACHE).

-spec stop_v3_migrator() -> any().
stop_v3_migrator() ->
    cdr_sup:stop_v3_migrate().

-spec start_v3_migrator() -> any().
start_v3_migrator() ->
    cdr_sup:start_v3_migrate().

-spec get_v3_migrator_status() -> 'ok'.
get_v3_migrator_status() ->
    cdr_sup:get_v3_migrate_status().

-spec create_test_migrate_accounts() -> 'ok' | wh_std_return().
create_test_migrate_accounts() ->
    create_test_migrate_accounts(?DFLT_NUM_TEST_ACCOUNTS
                                 ,?DFLT_NUM_MONTHS_LGCY_DATA
                                 ,?DFLT_NUM_CDR_PER_DAY
                                ).

-spec create_test_migrate_accounts(input_term()) -> 'ok' | wh_std_return().
create_test_migrate_accounts(NumTestAccounts) ->
    create_test_migrate_accounts(NumTestAccounts
                                 ,?DFLT_NUM_MONTHS_LGCY_DATA
                                 ,?DFLT_NUM_CDR_PER_DAY
                                ).

-spec create_test_migrate_accounts(input_term(), input_term()) ->
                                          'ok' | wh_std_return().
create_test_migrate_accounts(NumTestAccounts
                             ,NumMonthsLgcyData) ->
    create_test_migrate_accounts(NumTestAccounts
                                 ,NumMonthsLgcyData
                                 ,?DFLT_NUM_CDR_PER_DAY
                                ).

-spec create_test_migrate_accounts(input_term(), input_term(), input_term()) ->
                                          'ok' | wh_std_return().
create_test_migrate_accounts(NumTestAccounts
                             ,NumMonthsLgcyData
                             ,NumCdrsPerDay) ->
    cdr_v3_migrate_lib:generate_test_accounts(wh_util:to_integer(NumTestAccounts)
                                              ,wh_util:to_integer(NumMonthsLgcyData)
                                              ,wh_util:to_integer(NumCdrsPerDay)
                                             ).

-spec delete_test_accounts() -> 'ok' | wh_std_return().
delete_test_accounts() ->
    cdr_v3_migrate_lib:delete_test_accounts().

-spec start_v3_test_migrator() -> any().
start_v3_test_migrator() ->
    create_test_migrate_accounts(),
    cdr_sup:start_v3_migrate().

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
