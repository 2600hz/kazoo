%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Various utilities to work with currency.
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_currency).

-export([available_units/1
        ,available_units/2
        ]).
-export([available_dollars/1
        ,available_dollars/2
        ]).
-export([past_available_units/1
        ,past_available_units/2
        ,past_available_units/3
        ,past_available_units/4
        ]).
-export([past_available_dollars/1
        ,past_available_dollars/2
        ,past_available_dollars/3
        ,past_available_dollars/4
        ]).
-export([dollars_to_units/1]).
-export([units_to_dollars/1]).
-export([pretty_print_dollars/1]).
-export([rollover/1
        ,rollover/3
        ,rollover/4
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(DOLLAR_TO_UNIT, 10000).

-type units() :: integer().
-type dollars() :: number().
-type available_units_return() :: {'ok', units()} | {'error', any()}.
-type available_dollars_return() :: {'ok', dollars()} | {'error', any()}.
-export_type([units/0
             ,dollars/0
             ,available_units_return/0
             ,available_dollars_return/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec available_units(kz_term:ne_binary()) -> available_units_return().
available_units(Account) ->
    kz_ledgers:total_sources(Account).

-spec available_units(kz_term:ne_binary(), Default) -> units() | Default.
available_units(Account, Default) ->
    case available_units(Account) of
        {'error', _Reason} -> Default;
        {'ok', AvailableUnits} ->
            AvailableUnits
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec available_dollars(kz_term:ne_binary()) -> available_dollars_return().
available_dollars(Account) ->
    case available_units(Account) of
        {'error', _Reason} = Error -> Error;
        {'ok', Units} ->
            {'ok', units_to_dollars(Units)}
    end.

-spec available_dollars(kz_term:ne_binary(), Default) -> dollars() | Default.
available_dollars(Account, Default) ->
    case available_dollars(Account) of
        {'error', _Reason} -> Default;
        {'ok', AvailableDollars} ->
            AvailableDollars
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec past_available_units(kz_term:ne_binary()) -> available_units_return().
past_available_units(Account) ->
    kz_ledgers:total_sources_from_previous(Account).

-spec past_available_units(kz_term:ne_binary(), Default) ->
          units() | Default.
past_available_units(Account, Default) ->
    case past_available_units(Account) of
        {'error', _Reason} -> Default;
        {'ok', Units} -> Units
    end.

-spec past_available_units(kz_term:ne_binary(), kz_time:year(), kz_time:month()) ->
          available_units_return().
past_available_units(Account, Year, Month) ->
    kz_ledgers:total_sources(Account, Year, Month).

-spec past_available_units(kz_term:ne_binary(), kz_time:year(), kz_time:month(), Default) ->
          units() | Default.
past_available_units(Account, Year, Month, Default) ->
    case kz_ledgers:total_sources(Account, Year, Month) of
        {'error', _Reason} -> Default;
        {'ok', Units} -> Units
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec past_available_dollars(kz_term:ne_binary()) -> available_dollars_return().
past_available_dollars(Account) ->
    case past_available_units(Account) of
        {'error', _Reason} = Error -> Error;
        {'ok', Units} ->
            {'ok', units_to_dollars(Units)}
    end.

-spec past_available_dollars(kz_term:ne_binary(), Default) ->
          units() | Default.
past_available_dollars(Account, Default) ->
    case past_available_units(Account) of
        {'error', _Reason} -> Default;
        {'ok', Units} ->
            units_to_dollars(Units)
    end.

-spec past_available_dollars(kz_term:ne_binary(), kz_time:year(), kz_time:month()) ->
          available_dollars_return().
past_available_dollars(Account, Year, Month) ->
    case past_available_units(Account, Year, Month) of
        {'error', _Reason} = Error -> Error;
        {'ok', Units} ->
            {'ok', units_to_dollars(Units)}
    end.

-spec past_available_dollars(kz_term:ne_binary(), kz_time:year(), kz_time:month(), Default) ->
          dollars() | Default.
past_available_dollars(Account, Year, Month, Default) ->
    case past_available_units(Account, Year, Month) of
        {'error', _Reason} -> Default;
        {'ok', Units} ->
            units_to_dollars(Units)
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec dollars_to_units(kz_term:text() | dollars()) -> units().
dollars_to_units(Dollars) when is_number(Dollars) ->
    round(Dollars * ?DOLLAR_TO_UNIT);
dollars_to_units(Dollars) ->
    dollars_to_units(kz_term:to_float(Dollars)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec units_to_dollars(units()) -> dollars().
units_to_dollars(Units) when is_number(Units) ->
    trunc(Units) / ?DOLLAR_TO_UNIT;
units_to_dollars(Units) ->
    units_to_dollars(kz_term:to_integer(Units)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec pretty_print_dollars(dollars()) -> kz_term:ne_binary().
pretty_print_dollars(Amount) ->
    kz_term:to_binary(io_lib:format("$~.2f", [Amount])).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec rollover(kz_term:ne_binary()) -> available_units_return().
rollover(Account) ->
    kz_ledgers:rollover(Account).

-spec rollover(kz_term:ne_binary(),  kz_time:year(), kz_time:month()) -> available_units_return().
rollover(Account, Year, Month) ->
    kz_ledgers:rollover(Account, Year, Month).

-spec rollover(kz_term:ne_binary(),  kz_time:year(), kz_time:month(), units()) ->
          available_units_return().
rollover(Account, Year, Month, Units) ->
    kz_ledgers:rollover(Account, Year, Month, Units).
