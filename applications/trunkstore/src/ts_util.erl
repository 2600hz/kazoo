%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.com>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Utility functions to aid in the operation of trunkstore stuff. <-- very descriptive
%%% @end
%%% Created : 23 Oct 2010 by James Aimonetti <james@2600hz.com>
%%%-------------------------------------------------------------------
-module(ts_util).

-export([to_e164/1, to_npanxxxxxx/1, to_1npanxxxxxx/1]).

%% +18001234567 -> +18001234567
to_e164(<<$+, $1, N/bitstring>>=E164) when erlang:bit_size(N) == 80 -> % 8bits/ch * 10ch
    E164;
%% 18001234567 -> +18001234567
to_e164(<<$1, N/binary>>=NPAN1) when erlang:bit_size(N) == 80 ->
    << $+, NPAN1/bitstring >>;
%% 8001234567 -> +18001234567
to_e164(NPAN) when erlang:bit_size(NPAN) == 80 ->
    <<$+, $1, NPAN/bitstring>>;
to_e164(Other) ->
    Other.

to_npanxxxxxx(<<$+, $1, N/bitstring>>) when erlang:bit_size(N) == 80 ->
    N;
to_npanxxxxxx(<<$1, N/bitstring>>) when erlang:bit_size(N) == 80 ->
    N;
to_npanxxxxxx(NPAN) when erlang:bit_size(NPAN) == 80 ->
    NPAN;
to_npanxxxxxx(Other) ->
    Other.

to_1npanxxxxxx(<<$+, $1, N/bitstring>>) when erlang:bit_size(N) == 80 ->
    <<$1, N>>;
to_1npanxxxxxx(<<$1, N/bitstring>>=NPAN1) when erlang:bit_size(N) == 80 ->
    NPAN1;
to_1npanxxxxxx(NPAN) when erlang:bit_size(NPAN) == 80 ->
    <<$1, NPAN>>;
to_1npanxxxxxx(Other) ->
    Other.
