%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2022, 2600Hz
%%% @doc Handle client requests for phone number
%%% @end
%%%-----------------------------------------------------------------------------
-module(knm_inteliquent).
-behaviour(knm_gen_carrier).

-export([info/0]).
-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).
-export([check_numbers/1]).

-include("knm.hrl").

-define(KNM_CARRIER_NAME, "inteliquent").
-define(KNM_CARRIER_CONFIG_CAT, <<(?KNM_CONFIG_CAT)/binary, ".", ?KNM_CARRIER_NAME>>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec info() -> map().
info() ->
    #{?CARRIER_INFO_MAX_PREFIX => 3
     }.

%%------------------------------------------------------------------------------
%% @doc Is this carrier handling numbers local to the system?
%%
%% <div class="notice">A non-local (foreign) carrier module makes HTTP requests.</div>
%% @end
%%------------------------------------------------------------------------------
-spec is_local() -> boolean().
is_local() -> 'false'.

%%------------------------------------------------------------------------------
%% @doc Check with carrier if these numbers are registered with it.
%% @end
%%------------------------------------------------------------------------------
-spec check_numbers(kz_term:ne_binaries()) -> {'ok', kz_json:object()} |
          {'error', 'not_implemented'}.
check_numbers(_Numbers) -> {'error', 'not_implemented'}.

%%------------------------------------------------------------------------------
%% @doc Query carrier for available numbers.
%% @end
%%------------------------------------------------------------------------------
-spec find_numbers(kz_term:ne_binary(), pos_integer(), knm_carriers:options()) ->
          {'ok', knm_number:knm_numbers()}.
find_numbers(_Prefix, _Quantity, _Options) ->
    {'ok', []}.

%%------------------------------------------------------------------------------
%% @doc Acquire a given number from carrier.
%% @end
%%------------------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) ->
          no_return().
acquire_number(Number) ->
    knm_errors:by_carrier(?MODULE, 'not_implemented', Number).

%%------------------------------------------------------------------------------
%% @doc Return number back to carrier.
%% @end
%%------------------------------------------------------------------------------
-spec disconnect_number(knm_number:knm_number()) -> knm_number:knm_number().
disconnect_number(Number) -> Number.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_number_billable(knm_phone_number:knm_phone_number()) -> boolean().
is_number_billable(_Number) -> 'true'.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec should_lookup_cnam() -> boolean().
should_lookup_cnam() -> 'true'.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
