%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz INC
%%% @doc
%%% Erlang terms very general convertion utilities
%%%
%%% If a function takes any() it should probably go here
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(kz_term).

-export([shuffle_list/1]).

-export([to_integer/1, to_integer/2
         ,to_float/1, to_float/2
         ,to_number/1
         ,to_hex/1, to_hex_binary/1, rand_hex_binary/1
         ,hexencode_binary/1
         ,from_hex_binary/1, from_hex_string/1
         ,to_list/1, to_binary/1
         ,to_atom/1, to_atom/2
        ]).
-export([to_boolean/1, is_boolean/1
         ,is_true/1, is_false/1
         ,is_empty/1, is_not_empty/1
         ,is_proplist/1
         ,identity/1
         ,always_true/1, always_false/1
        ]).
-export([to_lower_binary/1, to_upper_binary/1
         ,to_lower_string/1, to_upper_string/1
         ,ucfirst_binary/1, lcfirst_binary/1
         ,strip_binary/1, strip_binary/2
         ,strip_left_binary/2, strip_right_binary/2
         ,suffix_binary/2
         ,truncate_binary/2, truncate_binary/3
         ,truncate_left_binary/2, truncate_right_binary/2
        ]).

-export([clean_binary/1, clean_binary/2
         ,remove_white_spaces/1
        ]).

-export([binary_md5/1]).
-export([pad_binary/3, pad_binary_left/3
        ,join_binary/1, join_binary/2]).
-export([floor/1, ceiling/1]).


-include_lib("kernel/include/inet.hrl").

-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_api.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Ensure a binary is a minimum size, padding it if not with a given
%% value.
%% @end
%%--------------------------------------------------------------------
-spec pad_binary(binary(), non_neg_integer(), binary()) -> binary().
pad_binary(Bin, Size, Value) when size(Bin) < Size ->
    pad_binary(<<Bin/binary, Value/binary>>, Size, Value);
pad_binary(Bin, _, _) -> Bin.

-spec pad_binary_left(binary(), non_neg_integer(), binary()) -> binary().
pad_binary_left(Bin, Size, Value) when size(Bin) < Size ->
    pad_binary_left(<<Value/binary, Bin/binary>>, Size, Value);
pad_binary_left(Bin, _Size, _Value) -> Bin.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Join a binary together with a seperator.
%%
%% @end
%%--------------------------------------------------------------------
-spec join_binary([text() | atom(),...]) -> binary().
-spec join_binary([text() | atom(),...], binary()) -> binary().

join_binary(Bins) -> join_binary(Bins, <<", ">>).
join_binary([], _) -> <<>>;
join_binary([Bin], _) -> to_binary(Bin);
join_binary([Bin|Bins], Sep) ->
    iolist_to_binary(
      [to_binary(Bin)] ++ [[Sep, to_binary(B)] || B <- Bins]
     ).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec shuffle_list(list()) -> list().
shuffle_list([]) -> [];
shuffle_list(List) when is_list(List) ->
    Len = length(List),
    randomize_list(round(math:log(Len) + 0.5), List).

-spec randomize_list(list()) -> list().
-spec randomize_list(pos_integer(), list()) -> list().

randomize_list(List) ->
    D = lists:keysort(1, [{random:uniform(), A} || A <- List]),
    {_, D1} = lists:unzip(D),
    D1.

randomize_list(1, List) -> randomize_list(List);
randomize_list(T, List) ->
    lists:foldl(fun(_E, Acc) ->
                        randomize_list(Acc)
                end, randomize_list(List), lists:seq(1, (T - 1))).

%% must be a term that can be changed to a list
-spec to_hex(binary() | string()) -> string().
to_hex(S) ->
    string:to_lower(lists:flatten([io_lib:format("~2.16.0B", [H]) || H <- to_list(S)])).

-spec to_hex_binary(binary() | string()) -> binary().
to_hex_binary(S) ->
    Bin = to_binary(S),
    << <<(binary_to_hex_char(B div 16)), (binary_to_hex_char(B rem 16))>> || <<B>> <= Bin>>.

hexencode_binary(<<_/binary>> = Bin) ->
    hexencode_binary(Bin, <<>>);
hexencode_binary(S) ->
    hexencode_binary(to_binary(S)).

hexencode_binary(<<>>, Acc) -> Acc;
hexencode_binary(<<Hi:4, Lo:4, Rest/binary>>, Acc) ->
    hexencode_binary(Rest, <<Acc/binary
                             ,(binary_to_hex_char(Hi))
                             ,(binary_to_hex_char(Lo))
                           >>).

-spec from_hex_binary(binary()) -> binary().
from_hex_binary(Bin) ->
    to_binary(from_hex_string(to_list(Bin))).

-spec from_hex_string(list()) -> list().
-spec from_hex_string(list(), list()) -> list().
from_hex_string(Str) ->
    from_hex_string(Str, []).

from_hex_string([], Acc) -> lists:reverse(Acc);
from_hex_string([Div, Rem | T], Acc) ->
    Lo = hex_char_to_binary(Rem),
    Hi = hex_char_to_binary(Div),

    Sum = (Hi * 16) + Lo,

    from_hex_string(T, [Sum|Acc]).

-spec hex_char_to_binary(pos_integer()) -> pos_integer().
hex_char_to_binary(B) when B < 58 ->
    (to_lower_char(B) - $0);
hex_char_to_binary(B) ->
    to_lower_char(B) - ($a - 10).

-spec rand_hex_binary(pos_integer() | ne_binary()) -> ne_binary().
rand_hex_binary(Size) when not is_integer(Size) ->
    rand_hex_binary(?MODULE:to_integer(Size));
rand_hex_binary(Size) when is_integer(Size) andalso Size > 0 ->
    to_hex_binary(rand_hex(Size)).

-spec rand_hex(pos_integer()) -> ne_binary().
rand_hex(Size) ->
    try crypto:strong_rand_bytes(Size) of
        Bytes -> Bytes
    catch
        _:'low_entropy' -> crypto:rand_bytes(Size)
    end.

-spec binary_to_hex_char(pos_integer()) -> pos_integer().
binary_to_hex_char(N) when N < 10 -> $0 + N;
binary_to_hex_char(N) when N < 16 -> $a - 10 + N.


-spec to_integer(string() | binary() | integer() | float()) -> integer().
-spec to_integer(string() | binary() | integer() | float(), 'strict' | 'notstrict') -> integer().
to_integer(X) -> to_integer(X, 'notstrict').

to_integer(X, 'strict') when is_float(X) -> erlang:error('badarg');
to_integer(X, 'notstrict') when is_float(X) -> round(X);
to_integer(X, S) when is_binary(X) -> to_integer(binary_to_list(X), S);
to_integer(X, S) when is_list(X) ->
    try list_to_integer(X) of
        I -> I
    catch
        'error':'badarg' when S =:= 'notstrict' ->
            round(list_to_float(X))
    end;
to_integer(X, _) when is_integer(X) ->
    X.

-spec to_float(string() | binary() | integer() | float()) -> float().
-spec to_float(string() | binary() | integer() | float(), 'strict' | 'notstrict') -> float().
to_float(X) -> to_float(X, 'notstrict').

to_float(X, S) when is_binary(X) -> to_float(binary_to_list(X), S);
to_float(X, S) when is_list(X) ->
    try list_to_float(X) of
        F -> F
    catch
        'error':'badarg' when S =:= 'notstrict' -> list_to_integer(X)*1.0 %% "500" -> 500.0
    end;
to_float(X, 'strict') when is_integer(X) -> erlang:error('badarg');
to_float(X, 'notstrict') when is_integer(X) -> X * 1.0;
to_float(X, _) when is_float(X) -> X.

-spec to_number(binary() | string() | number()) -> number().
to_number(X) when is_number(X) -> X;
to_number(X) when is_binary(X) -> to_number(to_list(X));
to_number(X) when is_list(X) ->
    try list_to_integer(X) of
        Int -> Int
    catch
        'error':'badarg' -> list_to_float(X)
    end.

-spec to_list(atom() | list() | binary() | integer() | float()) -> list().
to_list(X) when is_float(X) -> mochinum:digits(X);
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_list(X) -> X.

%% Known limitations:
%%   Converting [256 | _], lists with integers > 255
-spec to_binary(atom() | string() | binary() | integer() | float() | pid() | iolist()) -> binary().
to_binary(X) when is_float(X) -> to_binary(mochinum:digits(X));
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X));
to_binary(X) when is_list(X) -> iolist_to_binary(X);
to_binary(X) when is_pid(X) -> to_binary(pid_to_list(X));
to_binary(X) when is_binary(X) -> X.

%% the safer version, won't let you leak atoms
-spec to_atom(atom() | list() | binary() | integer() | float()) -> atom().
to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_list(X) -> list_to_existing_atom(X);
to_atom(X) -> to_atom(to_list(X)).

%% only if you're really sure you want this
%% to protect yourself a bit from overrunning the atom table,
%% pass a list of safe values for X
%% so if X is a binary, the SafeList would be [ne_binary(),...]
%% if X is a list, the SafeList would be [nonempty_string(),...]
%% etc. So to_atom will not coerce the type of X to match the types in SafeList
%% when doing the lists:member/2
-spec to_atom(atom() | list() | binary() | integer() | float(), 'true' | list()) -> atom().
to_atom(X, _) when is_atom(X) -> X;
to_atom(X, 'true') when is_list(X) -> list_to_atom(X);
to_atom(X, 'true') -> to_atom(to_list(X), 'true');
to_atom(X, 'false') -> to_atom(X);
to_atom(X, SafeList) when is_list(SafeList) ->
    to_atom(to_list(X), lists:member(X, SafeList)).

-spec to_boolean(binary() | string() | atom()) -> boolean().
to_boolean(<<"true">>) -> 'true';
to_boolean("true") -> 'true';
to_boolean('true') -> 'true';
to_boolean(<<"false">>) -> 'false';
to_boolean("false") -> 'false';
to_boolean('false') -> 'false'.

-spec is_true(binary() | string() | atom()) -> boolean().
is_true(<<"true">>) -> 'true';
is_true("true") -> 'true';
is_true('true') -> 'true';
is_true(_) -> 'false'.

-spec always_true(any()) -> 'true'.
always_true(_) -> 'true'.

-spec is_false(binary() | string() | atom()) -> boolean().
is_false(<<"false">>) -> 'true';
is_false("false") -> 'true';
is_false('false') -> 'true';
is_false(_) -> 'false'.

-spec always_false(any()) -> 'false'.
always_false(_) -> 'false'.

-spec is_boolean(binary() | string() | atom()) -> boolean().
is_boolean(<<"true">>) -> 'true';
is_boolean("true") -> 'true';
is_boolean('true') -> 'true';
is_boolean(<<"false">>) -> 'true';
is_boolean("false") -> 'true';
is_boolean('false') -> 'true';
is_boolean(_) -> 'false'.

-spec is_empty(any()) -> boolean().
is_empty(0) -> 'true';
is_empty([]) -> 'true';
is_empty("0") -> 'true';
is_empty("false") -> 'true';
is_empty("NULL") -> 'true';
is_empty("undefined") -> 'true';
is_empty(<<>>) -> 'true';
is_empty(<<"0">>) -> 'true';
is_empty(<<"false">>) -> 'true';
is_empty(<<"NULL">>) -> 'true';
is_empty(<<"undefined">>) -> 'true';
is_empty('null') -> 'true';
is_empty('false') -> 'true';
is_empty('undefined') -> 'true';
is_empty(Float) when is_float(Float), Float =:= 0.0 -> 'true';
is_empty(MaybeJObj) ->
    case kz_json:is_json_object(MaybeJObj) of
        'false' -> 'false'; %% if not a json object, it's not empty
        'true' -> kz_json:is_empty(MaybeJObj)
    end.

-spec is_not_empty(any()) -> boolean().
is_not_empty(Term) -> (not is_empty(Term)).

-spec is_proplist(any()) -> boolean().
is_proplist(Term) when is_list(Term) ->
    lists:all(fun({_,_}) -> 'true'; (A) -> is_atom(A) end, Term);
is_proplist(_) -> 'false'.

-spec identity(X) -> X.
identity(X) -> X.

-spec to_lower_binary(any()) -> api_binary().
to_lower_binary('undefined') -> 'undefined';
to_lower_binary(Bin) when is_binary(Bin) -> << <<(to_lower_char(B))>> || <<B>> <= Bin>>;
to_lower_binary(Else) -> to_lower_binary(to_binary(Else)).

-spec to_lower_string(any()) -> 'undefined' | list().
to_lower_string('undefined') -> 'undefined';
to_lower_string(L) when is_list(L) ->
    [to_lower_char(C) || C <- L];
to_lower_string(Else) ->
    to_lower_string(to_list(Else)).

-spec ucfirst_binary(ne_binary()) -> ne_binary().
ucfirst_binary(<<F:8, Bin/binary>>) -> <<(to_upper_char(F)):8, Bin/binary>>.

-spec lcfirst_binary(ne_binary()) -> ne_binary().
lcfirst_binary(<<F:8, Bin/binary>>) -> <<(to_lower_char(F)):8, Bin/binary>>.

-spec to_lower_char(char()) -> char().
to_lower_char(C) when is_integer(C), $A =< C, C =< $Z -> C + 32;
%% Converts latin capital letters to lowercase, skipping 16#D7 (extended ascii 215) "multiplication sign: x"
to_lower_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 -> C + 32; % from string:to_lower
to_lower_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE -> C + 32; % so we only loop once
to_lower_char(C) -> C.

-spec to_upper_binary(any()) -> api_binary().
to_upper_binary('undefined') -> 'undefined';
to_upper_binary(Bin) when is_binary(Bin) -> << <<(to_upper_char(B))>> || <<B>> <= Bin>>;
to_upper_binary(Else) -> to_upper_binary(to_binary(Else)).

-spec to_upper_string(any()) -> 'undefined' | list().
to_upper_string('undefined') -> 'undefined';
to_upper_string(L) when is_list(L) -> [to_upper_char(C) || C <- L];
to_upper_string(Else) -> to_upper_string(to_list(Else)).

-spec to_upper_char(char()) -> char().
to_upper_char(C) when is_integer(C), $a =< C, C =< $z -> C - 32;
to_upper_char(C) when is_integer(C), 16#E0 =< C, C =< 16#F6 -> C - 32;
to_upper_char(C) when is_integer(C), 16#F8 =< C, C =< 16#FE -> C - 32;
to_upper_char(C) -> C.

-spec strip_binary(binary()) -> binary().
-spec strip_binary(binary(), 'both' | 'left' | 'right' | char() | [char()]) -> binary().
-spec strip_left_binary(binary(), char() | binary()) -> binary().
-spec strip_right_binary(binary(), char() | binary()) -> binary().
strip_binary(B) -> strip_binary(B, 'both').

strip_binary(B, 'left') -> strip_left_binary(B, $\s);
strip_binary(B, 'right') -> strip_right_binary(B, $\s);
strip_binary(B, 'both') -> strip_right_binary(strip_left_binary(B, $\s), $\s);
strip_binary(B, C) when is_integer(C) -> strip_right_binary(strip_left_binary(B, C), C);
strip_binary(B, Cs) when is_list(Cs) ->
    lists:foldl(fun(C, Acc) -> strip_binary(Acc, C) end
                ,B
                ,Cs
               ).

strip_left_binary(<<C, B/binary>>, C) -> strip_left_binary(B, C);
strip_left_binary(B, _) -> B.

strip_right_binary(C, C) -> <<>>;
strip_right_binary(<<C, B/binary>>, C) ->
    case strip_right_binary(B, C) of
        <<>> -> <<>>;
        T -> <<C, T/binary>>
    end;
strip_right_binary(<<A, B/binary>>, C) ->
    <<A, (strip_right_binary(B, C))/binary>>;
strip_right_binary(<<>>, _) -> <<>>.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Ensure a binary is a maximum given size, truncating it if not.
%% @end
%%--------------------------------------------------------------------
-spec truncate_binary(binary(), non_neg_integer()) -> binary().
-spec truncate_binary(binary(), non_neg_integer(), 'left' | 'right') -> binary().
truncate_binary(Bin, Size) ->
    truncate_binary(Bin, Size, 'right').

truncate_binary(Bin, Size, 'left') ->
    truncate_left_binary(Bin, Size);
truncate_binary(Bin, Size, 'right') ->
    truncate_right_binary(Bin, Size).

-spec truncate_left_binary(binary(), non_neg_integer()) -> binary().
truncate_left_binary(Bin, Size) when byte_size(Bin) > Size ->
    binary:part(Bin, {byte_size(Bin), -Size});
truncate_left_binary(Bin, _) ->
    Bin.

-spec truncate_right_binary(binary(), non_neg_integer()) -> binary().
truncate_right_binary(Bin, Size) when byte_size(Bin) > Size ->
    binary:part(Bin, {0, Size});
truncate_right_binary(Bin, _) ->
    Bin.

-spec suffix_binary(binary(), binary()) -> boolean().
suffix_binary(<<>>, _Bin) -> 'false';
suffix_binary(<<_/binary>> = Suffix, <<_/binary>> = Bin) ->
    try truncate_left_binary(Bin, byte_size(Suffix)) =:= Suffix of
        Bool -> Bool
    catch
        _:_ -> 'false'
    end.

-spec clean_binary(binary()) -> binary().
-spec clean_binary(binary(), kz_proplist()) -> binary().
clean_binary(Bin) ->
    clean_binary(Bin, []).

clean_binary(Bin, Opts) ->
    Routines = [fun remove_white_spaces/2],
    lists:foldl(fun(F, B) -> F(B, Opts) end, Bin, Routines).

-spec remove_white_spaces(binary(), kz_proplist()) -> binary().
remove_white_spaces(Bin, Opts) ->
    case props:get_value(<<"remove_white_spaces">>, Opts, 'true') of
        'false' -> Bin;
        'true' -> remove_white_spaces(Bin)
    end.

-spec remove_white_spaces(binary()) -> binary().
remove_white_spaces(Bin) ->
    << <<X>> || <<X>> <= Bin, X =/= $\s >>.

-spec binary_md5(text()) -> ne_binary().
binary_md5(Text) -> to_hex_binary(erlang:md5(to_binary(Text))).

%% found via trapexit
-spec floor(integer() | float()) -> integer().
floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        'true' -> T;
        'false' -> T - 1
    end;
floor(X) -> trunc(X).

%% found via trapexit
-spec ceiling(integer() | float()) -> integer().
ceiling(X) when X < 0 -> trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        'true' -> T;
        'false' -> T + 1
    end.
