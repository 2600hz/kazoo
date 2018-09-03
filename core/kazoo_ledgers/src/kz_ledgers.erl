%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2018, 2600Hz
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
-export([verify_monthly_rollup_exists/1
        ,get_monthly_rollup/1
        ,get_monthly_rollup/3
        ]).
-export([rollup/1
        ,rollup/3
        ,rollup/4
        ]).

-include("kazoo_ledgers.hrl").

-define(DEFAULT_AVIALABLE_LEDGERS,
        [kz_json:from_list([{<<"name">>, <<"per-minute-voip">>}
                           ,{<<"friendly_name">>, <<"Per Minute VoIP">>}
                           ,{<<"markup_type">>, [<<"percentage">>]}
                           ])
        ,kz_json:from_list([{<<"name">>, <<"kazoo-services">>}
                           ,{<<"friendly_name">>, <<"Kazoo Services">>}
                           ,{<<"markup_type">>, []}
                           ])
        ,kz_json:from_list([{<<"name">>, <<"kazoo-ledgers">>}
                           ,{<<"friendly_name">>, <<"Kazoo Ledgers">>}
                           ,{<<"markup_type">>, []}
                           ])
        ]
       ).

-define(ROLLUP_ID(Y,M), kazoo_modb_util:modb_id(Y, M, <<"kazoo_ledgers_monthly_rollup">>)).
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
            maybe_migrate_legacy_rollup(Account, Options);
        {'error', 'missing_rollup'} ->
            maybe_migrate_legacy_rollup(Account, Options);
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
                  ,'group'
                  ,{'group_level', 1}
                  ,'missing_as_error'
                   | Options
                  ],
    case kazoo_modb:get_results(Account, View, ViewOptions) of
        {'ok', []} ->
            {'error', 'missing_ledgers'};
        {'ok', JObjs} ->
            sum_sources(Account, Options, JObjs);
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

-spec sum_sources(kz_term:ne_binary(), kazoo_modb:view_options(), kz_json:objects()) ->
                         kz_currency:available_units_return().
sum_sources(Account, Options, JObjs) ->
    case lists:foldl(fun sum_sources_foldl/2, {'false', 0}, JObjs) of
        {'false', _Total} ->
            {'error', 'missing_rollup'};
        {'true', Total} ->
            log_sources_sum(Account, Options, Total)
    end.

-spec log_sources_sum(kz_term:ne_binary(), kazoo_modb:view_options(), kz_currency:units()) ->
                             kz_currency:available_units_return().
log_sources_sum(Account, Options, Total) ->
    {DefaultYear, DefaultMonth, _} = erlang:date(),
    Year = props:get_integer_value('year', Options, DefaultYear),
    Month = props:get_integer_value('month', Options, DefaultMonth),
    lager:debug("account ~s ledger total for ~p-~p is $~p"
               ,[Account
                ,Year
                ,Month
                ,kz_currency:units_to_dollars(Total)]
               ),
    {'ok', Total}.

-spec sum_sources_foldl(kz_json:object(), {boolean(), kz_currency:units()}) ->
                               {boolean(), kz_currency:units()}.
sum_sources_foldl(JObj, {FoundRollup, Sum}) ->
    Value = kz_json:get_integer_value(<<"value">>, JObj, 0),
    case kz_json:get_value(<<"key">>, JObj) of
        [<<"kazoo-ledgers">>] ->
            {'true', Sum + Value};
        _Else ->
            {FoundRollup, Sum + Value}
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
                        {'error', any()}.
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
-spec verify_monthly_rollup_exists(kz_term:ne_binary()) -> boolean().
verify_monthly_rollup_exists(Account) ->
    case get_monthly_rollup(Account) of
        {'ok', _} -> 'true';
        {'error', _} -> 'false'
    end.

-spec get_monthly_rollup(kz_term:ne_binary()) -> {'ok', kz_ledger:ledger()} |
                                                 {'error', any()}.
get_monthly_rollup(Account) ->
    {CurrentYear, CurrentMonth, _} = erlang:date(),
    get_monthly_rollup(Account, CurrentYear, CurrentMonth).

-spec get_monthly_rollup(kz_term:ne_binary(), kz_time:year(), kz_time:month()) ->
                                {'ok', kz_ledger:ledger()} |
                                {'error', any()}.
get_monthly_rollup(Account, Year, Month) ->
    case kazoo_modb:open_doc(Account, ?ROLLUP_ID(Year,Month), Year, Month) of
        {'ok', LedgerJObj} ->
            Ledger = kz_ledger:set_modb(kz_ledger:from_json(LedgerJObj)
                                       ,Account
                                       ,Year
                                       ,Month
                                       ),
            {'ok', Ledger};
        {'error', _Reason} = Error ->
            lager:debug("unable to get monthly rollup for ~s ~p-~p: ~p"
                       ,[Account, Year, Month, _Reason]
                       ),
            Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_migrate_legacy_rollup(kz_term:ne_binary(), kazoo_modb:view_options()) ->
                                         kz_currency:available_units_return().
maybe_migrate_legacy_rollup(Account, Options) ->
    {DefaultYear, DefaultMonth, _} = erlang:date(),
    Year = props:get_integer_value('year', Options, DefaultYear),
    Month = props:get_integer_value('month', Options, DefaultMonth),
    lager:warning("ledger rollup not found in ~s ~p-~p"
                 ,[Account, Year, Month]
                 ),
    %% NOTE: This is the migration point for legacy balances,
    %%  if the MODb was created during a period where transactions
    %%  where used to track balance (and transactions were rolled over)
    %%  create the ledger rollover from the transaction balance
    case kz_transactions:legacy_total(Account, Year, Month) of
        {'ok', Amount} ->
            lager:debug("found transaction rollup, migrating $~p to ledger balance"
                       ,[kz_currency:units_to_dollars(Amount)]
                       ),
            _ = rollup(Account, Year, Month, Amount),
            get_sources_total(Account, Options);
        {'error', 'no_legacy_transactions'} ->
            _ = rollup(Account, Year, Month),
            get_sources_total(Account, Options);
        {'error', _Reason} = Error -> Error
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec rollup(kz_term:ne_binary()) -> {'ok', kz_ledger:ledger()} |
                                     {'error', any()}.
rollup(Account) ->
    {Year, Month, _} = erlang:date(),
    rollup(Account, Year, Month).

-spec rollup(kz_term:ne_binary(),  kz_time:year(), kz_time:month()) ->
                    {'ok', kz_ledger:ledger()} |
                    {'error', any()}.
rollup(Account, Year, Month) ->
    {PreviousYear, PreviousMonth} =
        kazoo_modb_util:prev_year_month(Year, Month),
    case kz_currency:past_available_units(Account, PreviousYear, PreviousMonth) of
        {'error', _R} = Error -> Error;
        {'ok', Total} ->
            rollup(Account, Year, Month, Total)
    end.

-spec rollup(kz_term:ne_binary(),  kz_time:year(), kz_time:month(), kz_currency:units()) ->
                    kz_currency:available_units_return().
rollup(Account, Year, Month, Total) ->
    Id = <<(kz_term:to_binary(Year))/binary, "-"
          ,(kz_term:to_binary(Month))/binary
         >>,
    Setters =
        props:filter_empty(
          [{fun kz_ledger:set_account/2, Account}
          ,{fun kz_ledger:set_source_service/2, <<"kazoo-ledgers">>}
          ,{fun kz_ledger:set_source_id/2, Id}
          ,{fun kz_ledger:set_description/2, <<"Kazoo ledgers monthly rollup">>}
          ,{fun kz_ledger:set_period_start/2, kz_time:now_s()}
          ,{fun kz_ledger:set_metadata/2, metadata()}
          ,{fun kz_ledger:set_unit_amount/2, Total}
          ,{fun kz_ledger:set_id/2, ?ROLLUP_ID(Year,Month)}
          ,{fun kz_ledger:set_ledger_type/2, ledger_type(Total)}
          ]
         ),
    LedgerJObj =
        kz_ledger:to_json(kz_ledger:setters(Setters)),
    case kazoo_modb:save_doc(Account, LedgerJObj, Year, Month) of
        {'ok', _SavedJObj} ->
            lager:info("created ledger rollup for ~s ~p-~p"
                      ,[Account, Year, Month]
                      ),
            ViewOptions = [{'year', Year}
                          ,{'month', Month}
                          ],
            get_sources_total(Account, ViewOptions);
        {'error', _Reason} = Error ->
            lager:warning("unable to save ledger rollup for ~s ~p-~p: ~p"
                         ,[Account, Year, Month, _Reason]
                         ),
            Error
    end.

-spec metadata() -> kz_json:object().
metadata() ->
    kz_json:from_list(
      [{<<"type">>, <<"rollup">>}
      ,{<<"trigger">>, <<"automatic">>}
      ]
     ).

-spec ledger_type(kz_currency:units()) -> kzd_ledgers:ledger_type().
ledger_type(Total) when Total < 0 ->
    kzd_ledgers:type_debit();
ledger_type(_Total) ->
    kzd_ledgers:type_credit().
