%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Utility functions to aid in the operation of trunkstore stuff. <-- very descriptive
%%% @end
%%% Created : 23 Oct 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_util).

-export([to_e164/1]).

%% +18001234567 -> +18001234567
to_e164([$+, $1 | N]=E164) when length(N) == 10 ->
    E164;
%% 18001234567 -> +18001234567
to_e164([$1 | N]=AlmostE164) when length(N) == 10 ->
    [$+ | AlmostE164];
%% 8001234567 -> +18001234567
to_e164(TenDig) when length(TenDig) == 10 ->
    [$+, $1 | TenDig].
