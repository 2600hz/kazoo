%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_iso3166_util).

-export([iso3166a2/1
        ,iso3166a3/1
        ,country/1
        ]
      ).

-include("include/knm_iso3166.hrl").

%%------------------------------------------------------------------------------
%% @doc resolves an iso3166 a3 country code to an iso3166 a2 code
%% @end
%%------------------------------------------------------------------------------
-spec iso3166a2(kz_term:ne_binary()) -> kz_term:ne_binary() | {'error', 'invalid_country'}.
iso3166a2(<<CountryCode:3/binary>>) ->
    lager:notice("~p", [CountryCode]),
    CountryCode2 = kz_term:to_upper_binary(CountryCode),
    case proplists:get_value(CountryCode2, ?A2_DB) of
        'undefined' -> {'error', 'invalid_country'};
        A2 -> A2
    end.

%%------------------------------------------------------------------------------
%% @doc resolves an iso3166 a2 country code to an iso3166 a3 code
%% @end
%%------------------------------------------------------------------------------
-spec iso3166a3(kz_term:ne_binary()) -> kz_term:ne_binary() | {'error', 'invalid_country'}.
iso3166a3(<<CountryCode:2/binary>>) ->
    CountryCode2 = kz_term:to_upper_binary(CountryCode),
    case proplists:get_value(CountryCode2, ?A2_DB) of
        'undefined' -> {'error', 'invalid_country'};
        A3 -> A3
    end.

%%------------------------------------------------------------------------------
%% @doc resolve country code to a map of iso3166 data
%% @end
%%------------------------------------------------------------------------------
-spec country(kz_term:ne_binary()) -> country() | {'error', 'invalid_country'}.
country(<<CountryCode:2/binary>>) ->
    case iso3166a3(CountryCode) of
        {'error', 'invalid_country'} -> {'error', 'invalid_country'};
        A3 -> country(A3)
    end;
country(<<CountryCode:3/binary>>) ->
    CountryCode2 = kz_term:to_upper_binary(CountryCode),
    case proplists:get_value(CountryCode2, ?A3_DB) of
        {A2, CountryName} ->
            country_to_map(A2, CountryCode2, CountryName);
        {A2, CountryName, CCTLD} ->
            country_to_map(A2, CountryCode2, CountryName, CCTLD);
        'undefined' -> {'error', 'invalid_country'}
    end.

%%------------------------------------------------------------------------------
%% @doc resolves a country code to a map with the country's iso3166 data
%% @end
%%------------------------------------------------------------------------------
-spec country_to_map(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> country().
country_to_map(A2, A3, CountryName) ->
  country_to_map(A2, A3, CountryName, A2).

-spec country_to_map(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> country().
country_to_map(A2, A3, CountryName, CCTLD) ->
  #{a2 => A2
   ,a3 => A3
   ,name => CountryName
   ,cctld => <<".",CCTLD/binary>>
   ,itu => knm_util:prefix_for_country(A2)
   }.
