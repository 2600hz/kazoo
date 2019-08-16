%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_base64url).

-export([decode/1
        ,encode/1
        ]).

-spec encode(binary() | iolist()) -> binary().
encode(Bin) when is_binary(Bin) ->
    << << (urlencode_digit(D)) >> || <<D>> <= base64:encode(Bin), D =/= $= >>;
encode(L) when is_list(L) ->
    encode(iolist_to_binary(L)).

-spec decode(binary() | iolist()) -> binary().
decode(Bin) when is_binary(Bin) ->
    Bin2 = case byte_size(Bin) rem 4 of
               2 -> << Bin/binary, "==" >>;
               3 -> << Bin/binary, "=" >>;
               _ -> Bin
           end,
    base64:decode(<< << (urldecode_digit(D)) >> || <<D>> <= Bin2 >>);
decode(L) when is_list(L) ->
    decode(iolist_to_binary(L)).

-spec urlencode_digit(char()) -> char().
urlencode_digit($/) -> $_;
urlencode_digit($+) -> $-;
urlencode_digit(D)  -> D.

-spec urldecode_digit(char()) -> char().
urldecode_digit($_) -> $/;
urldecode_digit($-) -> $+;
urldecode_digit(D)  -> D.
