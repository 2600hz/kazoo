%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wnm_local).

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
-spec find_numbers(ne_binary(), pos_integer(), wh_proplist()) ->
                          {'error', 'non_available'}.
find_numbers(Number, Quanity, Opts) when size(Number) < 5 ->
    find_numbers(<<"+1", Number/binary>>, Quanity, Opts);
find_numbers(_Number, _Quanity, _Opts) ->
    %% TODO: given the requestors account number discovery wnm_local numbers that are
    %%       available but managed by accendants of the account.
    {'error', 'non_available'}.


-spec is_number_billable(wnm_number()) -> 'false'.
is_number_billable(_Number) -> 'false'.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number/1 :: (wnm_number()) -> wnm_number().
acquire_number(#number{dry_run='true'}=Number) -> Number;
acquire_number(Number) -> Number.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------

-spec disconnect_number(wnm_number()) -> wnm_number().
disconnect_number(Number) ->
        Number#number{state = <<"released">>, hard_delete='true'}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec should_lookup_cnam() -> 'true'.
should_lookup_cnam() -> 'true'.
