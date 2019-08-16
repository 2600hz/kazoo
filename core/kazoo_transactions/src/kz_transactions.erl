%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @author Peter Defebvre
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_transactions).

-export([fetch/1
        ,fetch/2
        ,fetch/3
        ]).
-export([legacy_total/1
        ,legacy_total/3
        ]).

-include("kazoo_transactions.hrl").

-define(LIST_BY_TIMESTAMP, <<"transactions/list_by_timestamp">>).

-type transactions() :: [kz_transaction:transaction()].
-export_type([transactions/0]).

%%------------------------------------------------------------------------------
%% @doc fetch last transactions from From to To
%% @end
%%------------------------------------------------------------------------------
-spec fetch(kz_term:ne_binary()) -> {'ok', transactions()} | {'error', any()}.
fetch(Account) ->
    fetch(Account, []).

-spec fetch(kz_term:ne_binary(), kz_term:proplist()) -> {'ok', transactions()} | {'error', any()}.
fetch(?NE_BINARY=Account, Options) ->
    ViewOptions = [{'result_key', <<"doc">>}
                  ,'include_docs'
                  ,'missing_as_error'
                   | Options
                  ],
    case kazoo_modb:get_results(Account, ?LIST_BY_TIMESTAMP, ViewOptions) of
        {'error', _}=Error -> Error;
        {'ok', JObjs} ->
            {'ok', [kz_transaction:from_json(JObj) || JObj <- JObjs]}
    end.

-spec fetch(kz_term:ne_binary(), kz_time:gregorian_seconds(), kz_time:gregorian_seconds()) ->
                   {'ok', transactions()} |
                   {'error', any()}.
fetch(?NE_BINARY=Account, CreatedFrom, CreatedTo)
  when is_integer(CreatedFrom), CreatedFrom > 0,
       is_integer(CreatedTo), CreatedTo > 0 ->
    MODbs = kazoo_modb:get_range(Account, CreatedFrom, CreatedTo),
    ViewOptions = [{'databases', MODbs}
                  ,{'startkey', CreatedFrom}
                  ,{'endkey', CreatedTo}
                  ,{'result_key', <<"doc">>}
                  ,'include_docs'
                  ,'missing_as_error'
                  ],
    get_ranged(?LIST_BY_TIMESTAMP, ViewOptions).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec legacy_total(kz_term:ne_binary()) -> kz_currency:available_units_return().
legacy_total(Account) ->
    {Year, Month, _} = erlang:date(),
    legacy_total(Account, Year, Month).

-spec legacy_total(kz_term:ne_binary(), kz_time:year(), kz_time:month()) ->
                          kz_currency:available_units_return().
legacy_total(Account, Year, Month) ->
    View = <<"transactions/legacy_total">>,
    ViewOptions = [{'year', Year}
                  ,{'month', Month}
                  ,{'group_level', 1}
                  ,{'result_key', <<"value">>}
                  ,'reduce'
                  ,'missing_as_error'
                  ],
    case kazoo_modb:get_results(Account, View, ViewOptions) of
        {'error', _Reason} = Error -> Error;
        {'ok', [Total]} when is_integer(Total) -> {'ok', Total};
        {'ok', []} -> {'error', 'no_legacy_transactions'}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec get_ranged(kz_term:ne_binary(), kz_term:proplist()) -> {'ok', transactions()} | {'error', any()}.
get_ranged(View, Options) ->
    MODbs = props:get_value('databases', Options, []),
    case MODbs =:= [] of
        'true' -> {'error', 'no_account_db'};
        'false' ->
            ViewOptions = props:delete('databases', Options),
            lager:debug("getting transactions starting from ~p to ~p from dbs: ~p"
                       ,[props:get_value('startkey', ViewOptions)
                        ,props:get_value('endkey', ViewOptions)
                        ,MODbs
                        ]),
            get_ranged(View, Options, MODbs, [])
    end.

-spec get_ranged(kz_term:ne_binary(), kz_term:proplist(), kz_term:ne_binaries(), kz_json:objects()) ->
                        {'ok', transactions()} |
                        {'error', any()}.
get_ranged(_View, _Options, [], Results) -> {'ok', Results};
get_ranged(View, Options, [MODb|MODbs], Results) ->
    case kazoo_modb:get_results(MODb, View, Options) of
        {'error', _Reason} = Error -> Error;
        {'ok', JObjs} ->
            Transactions = [kz_transaction:set_modb(
                              kz_transaction:from_json(JObj), MODb
                             )
                            || JObj <- JObjs
                           ],
            get_ranged(View, Options, MODbs, Transactions ++ Results)
    end.
