%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Handle rolling over ledgers at the start of the month
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_ledger_rollover).

%% behaviour: tasks_provider

-export([init/0]).

%% Triggerables
-export([handle_req/0]).

-export([rollover_accounts/2
        ,refresh_ledger_view/2
        ]).

-include("tasks.hrl").

-define(MOD_CAT, <<(?CONFIG_CAT)/binary, ".ledger_rollover">>).

-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(?TRIGGER_DAILY, ?MODULE, 'handle_req').

-spec handle_req() -> 'ok'.
handle_req() ->
    handle_req(erlang:date()).

-spec handle_req(kz_time:date()) -> 'ok'.
handle_req({Year, Month, 1}) ->
    _P = kz_process:spawn(fun rollover_accounts/2, [Year, Month]),
    lager:info("its a new month ~p-~p, rolling over ledgers in ~p", [Year, Month, _P]);
handle_req({Year, Month, _Day}) ->
    case kapps_config:is_true(?MOD_CAT, <<"refresh_view_enabled">>, 'true') of
        'true' ->
            _P = kz_process:spawn(fun refresh_ledger_view/2, [Year, Month]),
            lager:debug("refreshing ledger totals in ~p", [_P]);
        'false' -> 'ok'
    end.

-spec refresh_ledger_view(kz_time:year(), kz_time:month()) -> 'ok'.
refresh_ledger_view(Year, Month) ->
    PerPage = kapps_config:get_integer(?MOD_CAT, <<"refresh_in_parallel">>, 50),
    refresh_ledger_view(Year, Month, PerPage, get_page('undefined', PerPage)).

refresh_ledger_view(Year, Month, _PerPage, {'ok', Accounts, 'undefined'}) ->
    _ = [refresh_ledger(Year, Month, kz_doc:id(Account)) || Account <- Accounts],
    lager:debug("finished refreshing ledgers view");
refresh_ledger_view(Year, Month, PerPage, {'ok', Accounts, NextStartKey}) ->
    _ = [refresh_ledger(Year, Month, kz_doc:id(Account)) || Account <- Accounts],
    refresh_ledger_view(Year, Month, PerPage, get_page(NextStartKey, PerPage)).

refresh_ledger(Year, Month, AccountId) ->
    MODB = kz_util:format_account_id(AccountId, Year, Month),
    _ = kazoo_modb:get_results(MODB, <<"ledgers/totals_by_source">>, [{'limit', 1}]).

-spec rollover_accounts(kz_time:year(), kz_time:month()) -> 'ok'.
rollover_accounts(Year, Month) ->
    rollover_accounts(Year, Month, get_page('undefined')).

-spec rollover_accounts(kz_time:year(), kz_time:month(), kz_datamgr:paginated_results()) -> 'ok'.
rollover_accounts(Year, Month, {'ok', Accounts, 'undefined'}) ->
    _ = [kz_ledgers:rollover(kz_doc:id(Account), Year, Month) || Account <- Accounts],
    lager:info("finished rolling over accounts");
rollover_accounts(Year, Month, {'ok', Accounts, NextPageKey}) ->
    _ = [kz_ledgers:rollover(kz_doc:id(Account), Year, Month) || Account <- Accounts],
    rollover_accounts(Year, Month, get_page(NextPageKey));
rollover_accounts(_Year, _Month, {'error', _E}) ->
    lager:error("failed to query account listing during rollover: ~p", [_E]).

-spec get_page(kz_json:api_json_term()) -> kz_datamgr:paginated_results().
get_page(NextStartKey) ->
    get_page(NextStartKey, kapps_config:get_integer(?MOD_CAT, <<"rollover_in_parallel">>, 10)).

-spec get_page(kz_json:api_json_term(), pos_integer()) -> kz_datamgr:paginated_results().
get_page('undefined', PageSize) ->
    query([{'page_size', PageSize}]);
get_page(NextStartKey, PageSize) ->
    query([{'startkey', NextStartKey}
          ,{'page_size', PageSize}
          ]).

-spec query(kz_datamgr:view_options()) -> kz_datamgr:paginated_results().
query(ViewOptions) ->
    kz_datamgr:paginate_results(?KZ_ACCOUNTS_DB
                               ,<<"accounts/listing_by_id">>
                               ,ViewOptions
                               ).
