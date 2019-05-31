%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc Handle creating MODBs ahead of time
%%% @end
%%%-----------------------------------------------------------------------------
-module(kt_modb_creation).

%% behaviour: tasks_provider

-export([init/0]).

%% Triggerables
-export([handle_req/0]).

-export([create_modbs/0, create_modbs/2]).

-include("tasks.hrl").

-define(MOD_CAT, <<(?CONFIG_CAT)/binary, ".modb_creation">>).

-spec init() -> 'ok'.
init() ->
    _ = tasks_bindings:bind(?TRIGGER_DAILY, ?MODULE, 'handle_req').

-spec handle_req() -> 'ok'.
handle_req() ->
    CreateOnDay = kapps_config:get_integer(?MOD_CAT, <<"creation_day">>, 28),
    handle_req(CreateOnDay, erlang:date()).

-spec handle_req(kz_time:day(), kz_time:date()) -> 'ok'.
handle_req(Day, {Year, Month, Day}) ->
    _P = kz_util:spawn(fun create_modbs/2, [Year, Month]),
    lager:info("it is modb creation day! creating in ~p", [_P]);
handle_req(_CreateOnDay, {_Year, _Month, _Day}) -> 'ok'.

-spec create_modbs() -> 'ok'.
create_modbs() ->
    {Year, Month, _D} = erlang:date(),
    _P = kz_util:spawn(fun create_modbs/2, [Year, Month]),
    io:format("creating modbs in ~p~n", [_P]).

-spec create_modbs(kz_time:year(), kz_time:month()) -> 'ok'.
create_modbs(Year, Month) ->
    create_modbs(Year, Month, kz_datamgr:get_results_count(?KZ_ACCOUNTS_DB, <<"accounts/listing_by_id">>, [])).

-spec create_modbs(kz_time:year(), kz_time:month(), {'ok', non_neg_integer()} | kz_datamgr:data_error()) -> 'ok'.
create_modbs(Year, Month, {'ok', NumAccounts}) ->
    NextMonthS = calendar:datetime_to_gregorian_seconds({kz_date:normalize({Year, Month+1, 1}), {0,0,0}}),
    NowS = kz_time:now_s(),
    SecondsLeft = NextMonthS - NowS,

    %% Conservatively create 1 MODB per time unit
    AccountsPerPass = kapps_config:get_integer(?MOD_CAT, <<"create_in_parallel">>, 1),

    SecondsPerPass = (SecondsLeft div AccountsPerPass) div NumAccounts,

    lager:info("creating ~p modbs (~p per pass), ~p seconds delay between passes"
              ,[NumAccounts, AccountsPerPass, SecondsPerPass]
              ),
    create_modbs_metered(Year, Month, AccountsPerPass, SecondsPerPass).

create_modbs_metered(Year, Month, AccountsPerPass, SecondsPerPass) ->
    create_modbs_metered(Year, Month, AccountsPerPass, SecondsPerPass
                        ,get_page(AccountsPerPass, 'undefined')
                        ).

create_modbs_metered(Year, Month, _AccountsPerPass, _SecondsPerPass, {'ok', Accounts, 'undefined'}) ->
    _ = [create_modb(Year, Month, Account) || Account <- Accounts],
    lager:info("finished creating account MODBs");
create_modbs_metered(Year, Month, AccountsPerPass, SecondsPerPass, {'ok', Accounts, NextPageKey}) ->
    NowS = kz_time:now_s(),
    _ = [create_modb(Year, Month, Account) || Account <- Accounts],
    ElapsedS = kz_time:elapsed_s(NowS),
    WaitS = SecondsPerPass - ElapsedS,
    lager:info("created ~p modb(s), waiting ~ps for next pass", [length(Accounts), WaitS]),
    timer:sleep(WaitS * ?MILLISECONDS_IN_SECOND),
    create_modbs_metered(Year, Month, AccountsPerPass, SecondsPerPass
                        ,get_page(AccountsPerPass, NextPageKey)
                        );
create_modbs_metered(_Year, _Month, _AccountsPerPass, _SecondsPerPass, {'error', _E}) ->
    lager:info("error paginating accounts: ~p", [_E]).

-spec create_modb(kz_time:year(), kz_time:month(), kz_json:object()) -> boolean().
create_modb(Year, Month, AccountView) ->
    AccountId = kz_doc:id(AccountView),
    AccountMODB = kz_util:format_account_id(AccountId, Year, Month),
    kazoo_modb:maybe_create(AccountMODB).

-spec get_page(pos_integer(), kz_json:api_json_term()) -> kz_datamgr:paginated_results().
get_page(AccountsPerPass, 'undefined') ->
    query([{'page_size', AccountsPerPass}]);
get_page(AccountsPerPass, NextStartKey) ->
    query([{'page_size', AccountsPerPass}
          ,{'startkey', NextStartKey}
          ]).

-spec query(kz_datamgr:view_options()) -> kz_datamgr:paginated_results().
query(ViewOptions) ->
    kz_datamgr:paginate_results(?KZ_ACCOUNTS_DB
                               ,<<"accounts/listing_by_id">>
                               ,ViewOptions
                               ).
