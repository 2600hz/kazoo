%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% Handle tower-of-power MDN phone numbers.
%%% Note: disconnected numbers are marked as deleted.
%%%
%%% @end
%%% @contributors
%%%   Pierre Fenoll
%%%-------------------------------------------------------------------
-module(knm_mdn).
-behaviour(knm_gen_carrier).

-export([is_local/0]).
-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).

-include("knm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Is this carrier handling numbers local to the system?
%% Note: a non-local (foreign) carrier module makes HTTP requests.
%% @end
%%--------------------------------------------------------------------
-spec is_local() -> boolean().
is_local() -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quantity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), knm_carriers:options()) ->
                          {'ok', knm_number:knm_numbers()} |
                          {'error', any()}.
find_numbers(_Prefix, _Quantity, _Options) ->
    {'error', 'not_available'}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec is_number_billable(knm_number:knm_number()) -> 'false'.
is_number_billable(_Number) -> 'false'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number(knm_number:knm_number()) ->
                            knm_number:knm_number().
acquire_number(Number) -> Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------

-spec disconnect_number(knm_number:knm_number()) ->
                               knm_number:knm_number().
disconnect_number(Number) ->
    knm_number_states:to_deleted(Number).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.
