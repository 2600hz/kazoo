%%%-------------------------------------------------------------------
%%% @Copyright (C) 2010-2013, 2600Hz
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
         ,delete_test_migrate_accounts/0
         ,start_v3_migrator/0
         ,start_v3_test_migrator/0
         ,clean_v3_test_migrator/0
        ]).

-include("cdr.hrl").

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

-spec create_test_migrate_accounts() -> any().
create_test_migrate_accounts() ->
    NumTestAccounts = 2,
    NumMonthsLegacyData = 6,
    NumCdrsPerDay = 4,
    _ = cdr_v3_migrate_lib:generate_test_accounts(NumTestAccounts, NumMonthsLegacyData, NumCdrsPerDay).

delete_test_migrate_accounts() ->
    NumTestAccounts = 2,
    NumMonthsLegacyData = 6,
    _ = cdr_v3_migrate_lib:delete_test_accounts(NumTestAccounts, NumMonthsLegacyData).

-spec start_v3_test_migrator() -> any().
start_v3_test_migrator() ->
    NumTestAccounts = 2,
    NumMonthsLegacyData = 6,
    NumCdrsPerDay = 4,
    _ = cdr_v3_migrate_lib:generate_test_accounts(NumTestAccounts, NumMonthsLegacyData, NumCdrsPerDay),
    
    cdr_sup:start_v3_migrate().

-spec clean_v3_test_migrator() -> any().
clean_v3_test_migrator() ->    
    NumTestAccounts = 2,
    NumMonthsLegacyData = 6,
    _ = cdr_v3_migrate_lib:delete_test_accounts(NumTestAccounts, NumMonthsLegacyData).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
