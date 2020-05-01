%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Conversion of types
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_binary).

-export([format/2]).

-export([rand_hex/1
        ,hexencode/1
        ,from_hex/1
        ,from_hex_string/1
        ,to_utf8/1
        ]).

-export([ucfirst/1, lcfirst/1
        ,to_camel_case/1
        ,strip/1, strip/2
        ,strip_left/2, strip_right/2
        ,suffix/2
        ,truncate/2, truncate/3
        ,truncate_left/2, truncate_right/2
        ]).

-export([clean/1, clean/2
        ,remove_white_spaces/1, remove_white_spaces/2
        ,remove_non_numeric/1
        ]).

-export([md5/1]).
-export([pad/3, pad_left/3
        ,join/1, join/2
        ,reverse/1
        ]).
-export([pos/2, closests/2]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec format(string() | binary(), [term()]) -> binary().
format(Format, Args) ->
    kz_term:to_binary(
      io_lib:format(Format, Args)
     ).

%%------------------------------------------------------------------------------
%% @doc Ensure a binary is a minimum size, padding it if not with a given
%% value.
%% @end
%%------------------------------------------------------------------------------
-spec pad(binary(), non_neg_integer(), binary()) -> binary().
pad(Bin, Size, Value) when byte_size(Bin) < Size ->
    pad(<<Bin/binary, Value/binary>>, Size, Value);
pad(Bin, _, _) -> Bin.

-spec pad_left(binary(), non_neg_integer(), binary()) -> binary().
pad_left(Bin, Size, Value) when byte_size(Bin) < Size ->
    pad_left(<<Value/binary, Bin/binary>>, Size, Value);
pad_left(Bin, _Size, _Value) -> Bin.

%%------------------------------------------------------------------------------
%% @doc Join a binary together with a separator.
%% @end
%%------------------------------------------------------------------------------

-spec join([kz_term:text()]) -> binary().
join(Bins) -> join(Bins, <<", ">>).

-spec join([kz_term:text()], iodata() | char()) -> binary().
join([], _) -> <<>>;
join([Bin], _) -> kz_term:to_binary(Bin);
join([Bin|Bins], Sep) ->
    iolist_to_binary(
      [kz_term:to_binary(Bin)] ++ [[Sep, kz_term:to_binary(B)] || B <- Bins]
     ).

-spec md5(kz_term:text()) -> kz_term:ne_binary().
md5(Text) -> kz_term:to_hex_binary(erlang:md5(kz_term:to_binary(Text))).

-spec remove_white_spaces(binary(), kz_term:proplist()) -> binary().
remove_white_spaces(Bin, Opts) ->
    case props:get_value(<<"remove_white_spaces">>, Opts, 'true') of
        'false' -> Bin;
        'true' -> remove_white_spaces(Bin)
    end.

-spec remove_white_spaces(binary()) -> binary().
remove_white_spaces(Bin) ->
    << <<X>> || <<X>> <= Bin, X =/= $\s >>.

-spec remove_non_numeric(binary()) -> binary().
remove_non_numeric(Bin) ->
    << <<N>> || <<N>> <= Bin, kz_term:is_ascii_number(N)>>.

-spec clean(binary()) -> binary().
clean(Bin) ->
    clean(Bin, []).

-spec clean(binary(), kz_term:proplist()) -> binary().
clean(Bin, Opts) ->
    Routines = [fun remove_white_spaces/2],
    lists:foldl(fun(F, B) -> F(B, Opts) end, Bin, Routines).

-type strip_option() :: 'both' | 'left' | 'right' | char() | nonempty_string() | <<_:8>>.
-type strip_options() :: [strip_option()].

-spec strip(binary()) -> binary().
strip(B) -> strip(B, 'both').

-spec strip(binary(), strip_option() | strip_options()) -> binary().
strip(B, 'left') -> strip_left(B, $\s);
strip(B, 'right') -> strip_right(B, $\s);
strip(B, 'both') -> strip_right(strip_left(B, $\s), $\s);
strip(B, C) when is_integer(C) -> strip_right(strip_left(B, C), C);
strip(B, Cs) when is_list(Cs) ->
    lists:foldl(fun(C, Acc) -> strip(Acc, C) end, B, Cs);
strip(B, <<C>>) -> strip(B, C).

-spec strip_left(binary(), char() | binary()) -> binary().
strip_left(<<C, B/binary>>, C) -> strip_left(B, C);
strip_left(B, _) -> B.

-spec strip_right(binary(), char() | binary()) -> binary().
strip_right(C, C) -> <<>>;
strip_right(<<C, B/binary>>, C) ->
    case strip_right(B, C) of
        <<>> -> <<>>;
        T -> <<C, T/binary>>
    end;
strip_right(<<A, B/binary>>, C) ->
    <<A, (strip_right(B, C))/binary>>;
strip_right(<<>>, _) -> <<>>.

%%------------------------------------------------------------------------------
%% @doc Ensure a binary is a maximum given size, truncating it if not.
%% @end
%%------------------------------------------------------------------------------

-spec truncate(binary(), non_neg_integer()) -> binary().
truncate(Bin, Size) ->
    truncate(Bin, Size, 'right').

-spec truncate(binary(), non_neg_integer(), 'left' | 'right') -> binary().
truncate(Bin, Size, 'left') ->
    truncate_left(Bin, Size);
truncate(Bin, Size, 'right') ->
    truncate_right(Bin, Size).

-spec truncate_left(binary(), non_neg_integer()) -> binary().
truncate_left(Bin, Size) when byte_size(Bin) > Size ->
    binary:part(Bin, {byte_size(Bin), -Size});
truncate_left(Bin, _) ->
    Bin.

-spec truncate_right(binary(), non_neg_integer()) -> binary().
truncate_right(Bin, Size) when byte_size(Bin) > Size ->
    binary:part(Bin, {0, Size});
truncate_right(Bin, _) ->
    Bin.

-spec suffix(binary(), binary()) -> boolean().
suffix(<<>>, _Bin) -> 'false';
suffix(<<_/binary>> = Suffix, <<_/binary>> = Bin) ->
    try truncate_left(Bin, byte_size(Suffix)) =:= Suffix
    catch
        _:_ -> 'false'
    end.

-spec hexencode(kz_term:text()) -> binary().
hexencode(<<_/binary>> = Bin) ->
    hexencode(Bin, <<>>);
hexencode(S) ->
    hexencode(kz_term:to_binary(S)).

hexencode(<<>>, Acc) -> Acc;
hexencode(<<Hi:4, Lo:4, Rest/binary>>, Acc) ->
    hexencode(Rest
             ,list_to_binary([Acc
                             ,kz_term:to_hex_char(Hi)
                             ,kz_term:to_hex_char(Lo)
                             ])
             ).

-spec from_hex(binary()) -> binary().
from_hex(Bin) ->
    kz_term:to_binary(from_hex_string(kz_term:to_list(Bin))).

-spec from_hex_string(list()) -> list().
from_hex_string(Str) ->
    from_hex_string(Str, []).

-spec from_hex_string(list(), list()) -> list().
from_hex_string([], Acc) -> lists:reverse(Acc);
from_hex_string([Div, Rem | T], Acc) ->
    Lo = hex_char_to_binary(Rem),
    Hi = hex_char_to_binary(Div),

    Sum = (Hi * 16) + Lo,

    from_hex_string(T, [Sum|Acc]).

-spec hex_char_to_binary(pos_integer()) -> pos_integer().
hex_char_to_binary(B) when B < 58 ->
    kz_term:to_lower_char(B) - $0;
hex_char_to_binary(B) ->
    kz_term:to_lower_char(B) - ($a - 10).

-spec rand_hex(pos_integer() | binary() | string()) -> kz_term:ne_binary().
rand_hex(Size) when not is_integer(Size) ->
    rand_hex(kz_term:to_integer(Size));
rand_hex(Size) when is_integer(Size)
                    andalso Size > 0 ->
    kz_term:to_hex_binary(crypto:strong_rand_bytes(Size)).

-spec ucfirst(kz_term:ne_binary()) -> kz_term:ne_binary().
ucfirst(<<F:8, Bin/binary>>) -> <<(kz_term:to_upper_char(F)):8, Bin/binary>>.

-spec lcfirst(kz_term:ne_binary()) -> kz_term:ne_binary().
lcfirst(<<F:8, Bin/binary>>) -> <<(kz_term:to_lower_char(F)):8, Bin/binary>>.

-spec to_camel_case(any()) -> binary().
to_camel_case(Binary) when is_binary(Binary) ->
    << <<(ucfirst(kz_term:to_lower_binary(Word)))/binary>>
       || Word <- binary:split(Binary, [<<$_>>, <<$->>, <<$.>>], [global]),
          Word =/= <<>>
    >>.

-spec pos(char(), binary()) -> non_neg_integer() | -1.
pos(Char, Bin) ->
    pos(Char, Bin, 0).
pos(_, <<>>, _) -> -1;
pos(Char, <<Char:8, _/binary>>, N) -> N;
pos(Char, <<_:8, Bin/binary>>, N) ->
    pos(Char, Bin, N + 1).

-spec closests([char(),...], binary()) -> [{char(), non_neg_integer()}].
closests(Chars, Bin) ->
    Pairs = [{Char, Pos}
             || Char <- Chars,
                Pos <- [pos(Char, Bin)],
                Pos =/= -1
            ],
    lists:keysort(2, Pairs).

-spec reverse(binary()) -> binary().
reverse(Binary) ->
    Size = erlang:size(Binary)*8,
    <<X:Size/integer-little>> = Binary,
    <<X:Size/integer-big>>.

-spec to_utf8(binary()) -> binary().
to_utf8(<<Value/binary>>) ->
    %% io_lib:printable_unicode_list/1 check added to avoid encoding file's content
    %% like audio files.
    case io_lib:printable_unicode_list(binary_to_list(Value)) of
        'true' ->
            %% it must be a string or bitstring
            unicode:characters_to_binary(io_lib:format("~ts", [Value]));
        'false' ->
            %% it must be a file's content
            Value
    end.
