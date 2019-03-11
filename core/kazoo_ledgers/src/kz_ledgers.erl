%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_ledgers).

-export([total_sources/1
        ,total_sources/3
        ]).
-export([total_sources_from_previous/1]).
-export([list_source/2
        ,list_source/4
        ]).
-export([total_source/2
        ,total_source/4
        ]).
-export([available_ledgers/1]).
-export([verify_monthly_rollover_exists/1
        ,get_monthly_rollover/1
        ,get_monthly_rollover/3
        ]).
-export([rollover/1
        ,rollover/3
        ,rollover/4
        ]).
-export([sum_amount/1]).

-include("kazoo_ledgers.hrl").

-define(DEFAULT_AVIALABLE_LEDGERS,
        [kz_json:from_list([{<<"name">>, <<"per-minute-voip">>}
                           ,{<<"friendly_name">>, <<"Per Minute VoIP">>}
                           ,{<<"markup_type">>, [<<"percentage">>]}
                           ])
        ,kz_json:from_list([{<<"name">>, <<"services">>}
                           ,{<<"friendly_name">>, <<"Monthly Recurring Services">>}
                           ,{<<"markup_type">>, []}
                           ])
        ,kz_json:from_list([{<<"name">>, <<"payments">>}
                           ,{<<"friendly_name">>, <<"Payments">>}
                           ,{<<"markup_type">>, []}
                           ])
        ,kz_json:from_list([{<<"name">>, <<"rollovers">>}
                           ,{<<"friendly_name">>, <<"Monthly Rollover">>}
                           ,{<<"markup_type">>, []}
                           ])
        ]
       ).

-define(ROLLOVER_ID(Y,M), kazoo_modb_util:modb_id(Y, M, <<"ledgers_monthly_rollover">>)).
-type ledgers() :: [kz_ledger:ledger()].
-export_type([ledgers/0]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec total_sources(kz_term:ne_binary()) -> kz_currency:available_units_return().
total_sources(Account) ->
    total_sources(Account, []).

-spec total_sources(kz_term:ne_binary(), kz_time:year(), kz_time:month()) ->
                           kz_currency:available_units_return().
total_sources(Account, Year, Month) ->
    Options = [{'year', Year}
              ,{'month', Month}
              ],
    total_sources(Account, Options).

-spec total_sources(kz_term:ne_binary(), kazoo_modb:view_options()) ->
                           kz_currency:available_units_return().
total_sources(Account, Options) ->
    case get_sources_total(Account, Options) of
        {'ok', Total} -> {'ok', Total};
        {'error', 'missing_ledgers'} ->
            maybe_migrate_legacy_rollover(Account, Options);
        {'error', 'missing_rollover'} ->
            maybe_migrate_legacy_rollover(Account, Options);
        {'error', _Reason} = Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec total_sources_from_previous(kz_term:ne_binary()) -> kz_currency:available_units_return().
total_sources_from_previous(Account) ->
    {CurrentYear, CurrentMonth, _} = erlang:date(),
    {PreviousYear, PreviousMonth} =
        kazoo_modb_util:prev_year_month(CurrentYear, CurrentMonth),
    total_sources(Account, PreviousYear, PreviousMonth).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_sources_total(kz_term:ne_binary(), kazoo_modb:view_options()) ->
                               kz_currency:available_units_return().
get_sources_total(Account, Options) ->
    View = <<"ledgers/total_by_source">>,
    ViewOptions = ['reduce'
                  ,{'group_level', 1}
                  ,'missing_as_error'
                   | Options
                  ],
    case kazoo_modb:get_results(Account, View, ViewOptions) of
        {'ok', []} ->
            {'error', 'missing_ledgers'};
        {'ok', JObjs} ->
            sum_sources(JObjs);
        {'error', 'db_not_found'}=Error ->
            lager:info("unable to get balance for ~s, database does not exist", [Account]),
            Error;
        {'error', _Reason} = Error ->
            {DefaultYear, DefaultMonth, _} = erlang:date(),
            Year = props:get_value('year', Options, DefaultYear),
            Month = props:get_value('month', Options, DefaultMonth),
            lager:warning("unable to get balance for ~s ~p-~p: ~p"
                         ,[Account, Year, Month, _Reason]
                         ),
            Error
    end.

-spec sum_sources(kz_json:objects()) -> kz_currency:available_units_return().
sum_sources(JObjs) ->
    case lists:foldl(fun sum_sources_foldl/2, {'false', 0}, JObjs) of
        {'false', _Total} ->
            {'error', 'missing_rollover'};
        {'true', Total} -> {'ok', Total}
    end.

-spec sum_sources_foldl(kz_json:object(), {boolean(), kz_currency:units()}) ->
                               {boolean(), kz_currency:units()}.
sum_sources_foldl(JObj, {FoundRollover, Sum}) ->
    Value = kz_json:get_integer_value(<<"value">>, JObj, 0),
    case kz_json:get_value(<<"key">>, JObj) of
        [<<"rollovers">>] ->
            {'true', Sum + Value};
        _Else ->
            {FoundRollover, Sum + Value}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec list_source(kz_term:ne_binary(), kz_term:ne_binary()) ->
                         {'ok', ledgers()} |
                         {'error', any()}.
list_source(Account, Source) ->
    ViewOptions = [{'startkey', [Source]}
                  ,{'endkey', [Source, kz_json:new()]}
                  ,{'result_key', <<"doc">>}
                  ,'include_docs'
                  ,'missing_as_error'
                  ],
    case kazoo_modb:get_results(Account, ?LIST_BY_SOURCE, ViewOptions) of
        {'error', _} = Error -> Error;
        {'ok', JObjs} ->
            {'ok', [kz_ledger:from_json(JObj)
                    || JObj <- JObjs
                   ]
            }
    end.

-spec list_source(kz_term:ne_binary(), kz_term:ne_binary(), kz_time:seconds(), kz_time:seconds()) ->
                         {'ok', ledgers()} |
                         {'error', any()}.
list_source(Account, Source, CreatedFrom, CreatedTo)
  when is_integer(CreatedFrom), CreatedFrom > 0,
       is_integer(CreatedTo), CreatedTo > 0 ->
    MODBs = kazoo_modb:get_range(Account, CreatedFrom, CreatedTo),
    ViewOptions = [{'databases', MODBs}
                  ,{'startkey', [Source, CreatedFrom]}
                  ,{'endkey', [Source, CreatedTo]}
                  ,{'result_key', <<"doc">>}
                  ,'include_docs'
                  ,'missing_as_error'
                  ],
    get_ranged(?LIST_BY_SOURCE, ViewOptions).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec total_source(kz_term:ne_binary(), kz_term:ne_binary()) ->
                          kz_currency:available_units_return().
total_source(Account, Source) ->
    ViewOptions = [{'startkey', [Source]}
                  ,{'endkey', [Source, kz_json:new()]}
                  ,{'result_key', <<"value">>}
                  ,{'group', 1}
                  ,'reduce'
                  ,'missing_as_error'
                  ],
    handle_total_source_result(
      kazoo_modb:get_results(Account, ?TOTAL_BY_SOURCE, ViewOptions)
     ).

-spec total_source(kz_term:ne_binary(), kz_term:ne_binary(), kz_time:seconds(), kz_time:seconds()) ->
                          kz_currency:available_units_return().
total_source(Account, Source, CreatedFrom, CreatedTo)
  when is_integer(CreatedFrom), CreatedFrom > 0,
       is_integer(CreatedTo), CreatedTo > 0 ->
    MODBs = kazoo_modb:get_range(Account, CreatedFrom, CreatedTo),
    ViewOptions = [{'databases', MODBs}
                  ,{'startkey', [Source, CreatedFrom]}
                  ,{'endkey', [Source, CreatedTo]}
                  ,{'result_key', <<"value">>}
                  ,{'group', 1}
                  ,'reduce'
                  ,'missing_as_error'
                  ],
    handle_total_source_result(
      get_ranged(?TOTAL_BY_SOURCE, ViewOptions)
     ).

-spec handle_total_source_result({'ok', [kz_currency:units()]} | {'error', any()}) ->
                                        kz_currency:available_units_return().
handle_total_source_result({'error', _} = Error) -> Error;
handle_total_source_result({'ok', []}) -> {'ok', 0};
handle_total_source_result({'ok', [Total]}) -> {'ok', Total}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_ranged(kz_term:ne_binary(), kz_term:proplist()) ->
                        {'ok', kz_json:objects() | ledgers()} |
                        {'error', any()}.
get_ranged(View, Options) ->
    MODbs = props:get_value('databases', Options, []),
    case MODbs =:= [] of
        'true' -> {'error', 'no_account_db'};
        'false' ->
            ViewOptions = props:filter_undefined([{'group', 'true'}
                                                 ,{'group_level', 0}
                                                 ,{'reduce', 'true'}
                                                  | props:delete('databases', Options)
                                                 ]),
            lager:debug("getting ledgers starting from ~p to ~p from dbs: ~p"
                       ,[props:get_value('startkey', ViewOptions)
                        ,props:get_value('endkey', ViewOptions)
                        ,MODbs
                        ]),
            get_ranged(View, Options, MODbs, [])
    end.

-spec get_ranged(kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binaries(), kz_json:objects()) ->
                        {'ok', kz_json:objects() | ledgers()} |
                        kz_datamgr:data_error().
get_ranged(_View, _Options, [], Results) -> {'ok', Results};
get_ranged(View, Options, [MODb|MODbs], Results) ->
    HasDoc = props:get_value('result_key', Options) =:= <<"doc">>,
    case kazoo_modb:get_results(MODb, View, Options) of
        {'error', _Reason} = Error -> Error;
        {'ok', JObjs} when HasDoc ->
            Ledgers = [kz_ledger:set_modb(kz_ledger:from_json(JObj), MODb)
                       || JObj <- JObjs
                      ],
            get_ranged(View, Options, MODbs, Ledgers ++ Results);
        {'ok', JObjs} ->
            get_ranged(View, Options, MODbs, JObjs ++ Results)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec available_ledgers(kz_term:api_binary()) -> kz_json:objects().
available_ledgers(AccountId) ->
    kapps_account_config:get_global(AccountId, <<"ledgers">>, <<"registered_ledgers">>, ?DEFAULT_AVIALABLE_LEDGERS).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec verify_monthly_rollover_exists(kz_term:ne_binary()) -> boolean().
verify_monthly_rollover_exists(Account) ->
    case get_monthly_rollover(Account) of
        {'ok', _} -> 'true';
        {'error', _} -> 'false'
    end.

-spec get_monthly_rollover(kz_term:ne_binary()) -> {'ok', kz_ledger:ledger()} |
                                                   {'error', any()}.
get_monthly_rollover(Account) ->
    {CurrentYear, CurrentMonth, _} = erlang:date(),
    get_monthly_rollover(Account, CurrentYear, CurrentMonth).

-spec get_monthly_rollover(kz_term:ne_binary(), kz_time:year(), kz_time:month()) ->
                                  {'ok', kz_ledger:ledger()} |
                                  {'error', any()}.
get_monthly_rollover(Account, Year, Month) ->
    case kazoo_modb:open_doc(Account, ?ROLLOVER_ID(Year,Month), Year, Month) of
        {'ok', LedgerJObj} ->
            Ledger = kz_ledger:set_modb(kz_ledger:from_json(LedgerJObj)
                                       ,Account
                                       ,Year
                                       ,Month
                                       ),
            {'ok', Ledger};
        {'error', _Reason} = Error ->
            lager:debug("unable to get monthly rollover for ~s ~p-~p: ~p"
                       ,[Account, Year, Month, _Reason]
                       ),
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_migrate_legacy_rollover(kz_term:ne_binary(), kazoo_modb:view_options()) ->
                                           kz_currency:available_units_return().
maybe_migrate_legacy_rollover(Account, Options) ->
    {DefaultYear, DefaultMonth, _} = erlang:date(),
    Year = props:get_integer_value('year', Options, DefaultYear),
    Month = props:get_integer_value('month', Options, DefaultMonth),
    lager:warning("ledger rollover not found in ~s ~p-~p"
                 ,[Account, Year, Month]
                 ),
    %% NOTE: This is the migration point for legacy balances,
    %%  if the MODb was created during a period where transactions
    %%  where used to track balance (and transactions were rolled over)
    %%  create the ledger rollover from the transaction balance
    case should_rollover_monthly_balance() of
        'true' -> migrate_legacy_rollover(Account, Options, Year, Month);
        'false' ->
            lager:debug("monthly balance rollover is disabled, assuming previous balance was 0", []),
            _ = rollover(Account, Year, Month, 0),
            get_sources_total(Account, Options)
    end.

-spec migrate_legacy_rollover(kz_tern:ne_binary(), kazoo_modb:view_options(), kz_time:year(), kz_time:month()) ->
                                     kz_currency:available_units_return().
migrate_legacy_rollover(Account, Options, Year, Month) ->
    case kz_transactions:legacy_total(Account, Year, Month) of
        {'ok', Amount} ->
            lager:debug("found transaction rollover, migrating $~p to ledger balance"
                       ,[kz_currency:units_to_dollars(Amount)]
                       ),
            _ = rollover(Account, Year, Month, Amount),
            get_sources_total(Account, Options);
        {'error', 'no_legacy_transactions'} ->
            _ = rollover(Account, Year, Month),
            get_sources_total(Account, Options);
        {'error', _Reason} = Error ->
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec rollover(kz_term:ne_binary()) -> {'ok', kz_ledger:ledger()} |
                                       {'error', any()}.
rollover(Account) ->
    {Year, Month, _} = erlang:date(),
    rollover(Account, Year, Month).

-spec rollover(kz_term:ne_binary(),  kz_time:year(), kz_time:month()) ->
                      {'ok', kz_ledger:ledger()} |
                      {'error', any()}.
rollover(Account, Year, Month) ->
    case should_rollover_monthly_balance() of
        'true' -> rollover_past_available_units(Account, Year, Month);
        'false' ->
            lager:debug("monthly balance rollover is disabled, assuming previous balance was 0", []),
            rollover(Account, Year, Month, 0)
    end.

-spec rollover_past_available_units(kz_term:ne_binary(),  kz_time:year(), kz_time:month()) ->
                                           {'ok', kz_ledger:ledger()} |
                                           {'error', any()}.
rollover_past_available_units(Account, Year, Month) ->
    {PreviousYear, PreviousMonth} =
        kazoo_modb_util:prev_year_month(Year, Month),
    case kz_currency:past_available_units(Account, PreviousYear, PreviousMonth) of
        {'error', 'db_not_found'} ->
            rollover(Account, Year, Month, 0);
        {'error', _R} = Error ->
            Error;
        {'ok', Total} ->
            rollover(Account, Year, Month, Total)
    end.

-spec rollover(kz_term:ne_binary(),  kz_time:year(), kz_time:month(), kz_currency:units()) ->
                      kz_currency:available_units_return().
rollover(Account, Year, Month, Total) ->
    Metadata = kz_json:from_list([{<<"automatic_description">>, 'true'}]),
    Id = <<(kz_term:to_binary(Year))/binary
          ,(kz_date:pad_month(Month))/binary
         >>,
    Setters =
        props:filter_empty(
          [{fun kz_ledger:set_account/2, Account}
          ,{fun kz_ledger:set_source_service/2, <<"rollovers">>}
          ,{fun kz_ledger:set_source_id/2, Id}
          ,{fun kz_ledger:set_description/2, <<"Monthly rollover for ", Id/binary>>}
          ,{fun kz_ledger:set_period_start/2, kz_time:now_s()}
          ,{fun kz_ledger:set_metadata/2, Metadata}
          ,{fun kz_ledger:set_unit_amount/2, Total}
          ,{fun kz_ledger:set_id/2, ?ROLLOVER_ID(Year,Month)}
          ,{fun kz_ledger:set_ledger_type/2, ledger_type(Total)}
          ,{fun kz_ledger:set_executor_trigger/2, <<"automatic">>}
          ,{fun kz_ledger:set_executor_module/2, ?MODULE}
          ]
         ),
    LedgerJObj =
        kz_ledger:to_json(kz_ledger:setters(Setters)),
    case kazoo_modb:save_doc(Account, LedgerJObj, Year, Month) of
        {'ok', _SavedJObj} ->
            lager:info("created ledger rollover for ~s ~p-~p"
                      ,[Account, Year, Month]
                      ),
            ViewOptions = [{'year', Year}
                          ,{'month', Month}
                          ],
            get_sources_total(Account, ViewOptions);
        {'error', 'conflict'} ->
            lager:info("ledger rollover for ~s ~p-~p exists already"
                      ,[Account, Year, Month]
                      ),
            ViewOptions = [{'year', Year}
                          ,{'month', Month}
                          ],
            get_sources_total(Account, ViewOptions);
        {'error', _Reason} = Error ->
            lager:warning("unable to save ledger rollover for ~s ~p-~p: ~p"
                         ,[Account, Year, Month, _Reason]
                         ),
            Error
    end.

-spec ledger_type(kz_currency:units()) -> kzd_ledgers:ledger_type().
ledger_type(Total) when Total < 0 ->
    kzd_ledgers:type_debit();
ledger_type(_Total) ->
    kzd_ledgers:type_credit().

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec sum_amount(ledgers()) -> kz_currency:units().
sum_amount(Ledgers) ->
    lists:foldl(fun sum_amount/2, 0, Ledgers).

-spec sum_amount(kz_ledger:ledger(), kz_currency:units()) -> kz_currency:units().
sum_amount(Ledger, Sum) ->
    kz_ledger:unit_amount(Ledger) + Sum.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_rollover_monthly_balance() -> boolean().
should_rollover_monthly_balance() ->
    kapps_config:get_is_true(?CONFIG_CAT, <<"rollover_monthly_balance">>, 'true').
