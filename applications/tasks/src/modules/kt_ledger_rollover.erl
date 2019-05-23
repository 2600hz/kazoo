%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Handle rolling over ledgers at the start of the month
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_ledger_rollover).

%% behaviour: tasks_provider

-export([init/0]).

%% Triggerables
-export([handle_req/0]).

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
    lager:info("its a new month ~p-~p", [Year, Month]),
    rollover_accounts(Year, Month);
handle_req({_Year, _Month, _Day}) -> 'ok'.

-spec rollover_accounts(kz_time:year(), kz_time:month()) -> 'ok'.
rollover_accounts(Year, Month) ->
    rollover_accounts(Year, Month, kz_datamgr:paginate_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_id">>, [])).

-spec rollover_accounts(kz_time:year(), kz_time:month(), {'ok', kz_json:objects(), kz_json:api_json_term()} | kz_datamgr:data_error()) -> 'ok'.
rollover_accounts(Year, Month, {'ok', Accounts, 'undefined'}) ->
    _ = [kz_ledgers:rollover(kz_doc:id(Account), Year, Month) || Account <- Accounts],
    lager:info("finished rolling over ~p accounts", [length(Accounts)]);
rollover_accounts(Year, Month, {'ok', Accounts, NextPageKey}) ->
    _ = [kz_ledgers:rollover(kz_doc:id(Account), Year, Month) || Account <- Accounts],
    rollover_accounts(Year, Month
                     ,kz_datamgr:paginate_results(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_id">>, [{'startkey', NextPageKey}])
                     );
rollover_accounts(_Year, _Month, {'error', _E}) ->
    lager:error("failed to query account listing during rollover: ~p", [_E]).
