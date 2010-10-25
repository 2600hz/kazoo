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
to_e164(<<$+, $1, N/bitstring>>=E164) when erlang:bit_size(N) == 80 -> % 8bits/ch * 10ch
    E164;
%% 18001234567 -> +18001234567
to_e164(<<$1, N/binary>>=AlmostE164) when erlang:bit_size(N) == 80 ->
    << $+, AlmostE164/bitstring >>;
%% 8001234567 -> +18001234567
to_e164(TenDig) when erlang:bit_size(TenDig) == 80 ->
    <<$+, $1, TenDig/bitstring>>;
to_e164(Other) ->
    Other.

