%%%-------------------------------------------------------------------
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% Handle client requests for phone_number documents
%%%
%%% @end
%%% Created : 08 Jan 2012 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(wnm_local).

-export([find_numbers/3]).
-export([acquire_number/1]).
-export([disconnect_number/1]).

-include("../wnm.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Query the local system for a quanity of available numbers
%% in a rate center
%% @end
%%--------------------------------------------------------------------
-spec find_numbers/3 :: (ne_binary(), pos_integer(), wh_proplist()) -> %{'ok', wh_json:object()} |
                                                        {'error', 'non_available'}.
find_numbers(Number, Quanity, Opts) when size(Number) < 5 ->
    find_numbers(<<"+1", Number/binary>>, Quanity, Opts);
find_numbers(_Number, _Quanity, _Opts) ->
    %% TODO: given the requestors account number discovery wnm_local numbers that are
    %%       available but managed by accendants of the account.
    {error, non_available}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Acquire a given number from the carrier
%% @end
%%--------------------------------------------------------------------
-spec acquire_number/1 :: (wnm_number()) -> wnm_number().
acquire_number(Number) -> Number.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Release a number from the routing table
%% @end
%%--------------------------------------------------------------------
-spec disconnect_number/1 :: (wnm_number()) -> wnm_number().
disconnect_number(Number) -> Number.
