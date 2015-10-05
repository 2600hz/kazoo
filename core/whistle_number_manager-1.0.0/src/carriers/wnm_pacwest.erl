%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(wnm_pacwest).

-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).
-export([is_number_billable/1]).
-export([should_lookup_cnam/0]).

-include("../wnm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) -> {'error', _}.
find_numbers(_Number, _Quanity, _Opts) ->
    {'error', 'non_available'}.

-spec is_number_billable(wnm_number()) -> 'true'.
is_number_billable(_Number) -> 'true'.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number/1 :: (wnm_number()) -> wnm_number().
acquire_number(#number{dry_run='true'}=Number) -> Number;

acquire_number(Number) -> Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number(wnm_number()) -> wnm_number().
disconnect_number(Number) -> Number.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.
