%%%===================================================================
%%% @copyright (C) 2012, Erlang Solutions Ltd.
%%% This file is licensed under BSD 2-Clause License (see LICENSE file)
%%% @doc Encoding and decoding of Base16-encoded binaries
%%% @end
%%%===================================================================

-module(base16).

-export([encode/1, decode/1]).

%%--------------------------------------------------------------------
%% Public API
%%--------------------------------------------------------------------

-spec encode(binary()) -> binary().
encode(Data) ->
    << <<(hex(N div 16)), (hex(N rem 16))>> || <<N>> <= Data >>.

-spec decode(binary()) -> binary().
decode(Base16) when size(Base16) rem 2 =:= 0 ->
    << <<(unhex(H) bsl 4 + unhex(L))>> || <<H,L>> <= Base16 >>.

%%--------------------------------------------------------------------
%% Helpers
%%--------------------------------------------------------------------

hex(N) when N < 10 ->
    N + $0;
hex(N) when N < 16 ->
    N - 10 + $a.

unhex(D) when $0 =< D andalso D =< $9 ->
    D - $0;
unhex(D) when $a =< D andalso D =< $f ->
    10 + D - $a;
unhex(D) when $A =< D andalso D =< $F ->
    10 + D - $A.
