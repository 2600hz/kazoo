%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Conversion of types.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_term).

-export([shuffle_list/1]).
-export([uniq_list/1]).

-export([to_integer/1, to_integer/2
        ,to_float/1, to_float/2
        ,to_number/1
        ,to_hex/1, to_hex_binary/1, to_hex_char/1
        ,to_list/1
        ,to_binary/1
        ,to_api_binary/1
        ,to_api_term/1
        ,to_atom/1, to_atom/2
        ,to_boolean/1
        ,to_date/1
        ,to_datetime/1
        ,to_lower_binary/1, to_upper_binary/1
        ,to_lower_string/1, to_upper_string/1
        ,to_lower_char/1, to_upper_char/1
        ,to_pid/1

        ,words_to_bytes/1

        ,safe_cast/3

        ,error_to_binary/1
        ]).
-export([is_true/1, is_false/1
        ,is_boolean/1
        ,is_ne_binary/1, is_api_ne_binary/1
        ,is_ne_binaries/1
        ,is_empty/1, is_not_empty/1
        ,is_proplist/1, is_ne_list/1
        ,is_pos_integer/1
        ,is_ascii_code/1, is_ascii_number/1
        ,is_lower_char/1, is_upper_char/1
        ,identity/1
        ,always_true/1, always_false/1
        ]).

-export([a1hash/3, floor/1, ceiling/1]).

-export([iolist_join/2]).

-type text() :: string() | atom() | binary() | iolist().
%% Denotes Erlang data type which can represent as.

-type atoms() :: [atom()].
%% Denotes a list of `atom'.

-type pids() :: [pid()].
%% Denotes a list of `pid'.

-type references() :: [reference()].
%% Denotes a list of `reference'.

-type proplist_key() :: any().
-type proplist_value() :: any().
-type proplist_property() :: atom() | {proplist_key(), proplist_value()}.
%% Denotes definition of each key-value in a proplist.

-type proplist() :: [proplist_property()].
-type api_proplist() :: proplist() | 'undefined'.
%% A key-value form of data, `[{Key, Value}|atom]'.

-type proplists() :: [proplist()].
%% Denotes a list of `proplist'.

-type proplist_kv(K, V) :: [{K, V}].
%% Denotes a list of key-value with given `K' as key's type and `V' as value's type.

-type pid_ref() :: {pid(), reference()}.
-type pid_refs() :: [pid_ref()].
-type api_pid_ref() :: pid_ref() | 'undefined'.
-type api_pid_refs() :: pid_refs() | 'undefined'.

-type api_terms() :: kz_json:object() | proplist().
%% Kazoo API data type, either an Erlang representation of JSON object or a list of key-values.

-type api_binary() :: binary() | 'undefined'.
%% Denotes either data type is defined as `binary()' or it's `undefined'.

-type api_ne_binary() :: ne_binary() | 'undefined'.
%% Denotes either data type is defined as {@link ne_binary()} or it's `undefined'.

-type api_ne_binaries() :: [api_ne_binary()] | 'undefined'.
%% Denotes either data type is defined as {@link ne_binaries()} or it's `undefined'.

-type api_binaries() :: [api_binary()] | 'undefined'.
%% Denotes either data type is defined as {@link api_binary()} or it's `undefined'.

-type api_object() :: kz_json:object() | 'undefined'.
%% Denotes either data type is defined as {@link kz_json:object()} or it's `undefined'.

-type api_objects() :: kz_json:objects() | 'undefined'.
%% Denotes either data type is defined as {@link kz_json:objects()} or it's `undefined'.

-type api_boolean() :: boolean() | 'undefined'.
%% Denotes either data type is defined as `boolean()' or it's `undefined'.

-type api_atom() :: atom() | 'undefined'.
%% Denotes either data type is defined as `atom()' or it's `undefined'.

-type api_atoms() :: atoms() | 'undefined'.
%% Denotes either data type is defined as list of `atom()' or it's `undefined'.

-type api_string() :: string() | 'undefined'.
%% Denotes either data type is defined as `string()' or it's `undefined'.

-type api_reference() :: reference() | 'undefined'.
%% Denotes either data type is defined as `reference()' or it's `undefined'.

-type api_port() :: port() | 'undefined'.

-type api_pid() :: pid() | 'undefined'.
%% Denotes either data type is defined as `pid()' or it's `undefined'.

-type api_list() :: list() | 'undefined'.
%% Denotes either data type is defined as `list()' or it's `undefined'.

-type api_number() :: number() | 'undefined'.
%% Denotes either data type is defined as `list()' or it's `undefined'.

-type api_integer() :: integer() | 'undefined'.
-type api_integers() :: [integer()] | 'undefined'.
-type api_pos_integer() :: pos_integer() | 'undefined'.
%% Denotes either data type is defined as `list()' or it's `undefined'.

-type api_non_neg_integer() :: non_neg_integer() | 'undefined'.
%% Denotes either data type is defined as `list()' or it's `undefined'.

-type api_float() :: float() | 'undefined'.
%% Denotes either data type is defined as `list()' or it's `undefined'.

-type deeplist() :: iolist().
%% Denotes `[any()|'{@link deeplist()}`]'.

-type std_return() :: {'ok', any()} | {'error', any()}.
-type sup_no_return() :: 'no_return' | {'no_return', non_neg_integer()}.
%% Standard return type for request in Kazoo.

-type jobj_return() :: {'ok', kz_json:object()} | {'error', any()}.
%% Like {@link std_return()} but returns {@link kz_json:object()} for success.

-type jobjs_return() :: {'ok', kz_json:objects()} | {'error', any()}.
%% Like {@link std_return()} but returns {@link kz_json:objects()} for success.

-type ne_binary() :: <<_:8,_:_*8>>.
%% Denotes a binary which starts at least with 8 bits and continues to have `k' numbers of 8 bits, a non-empty binary.

-type ne_binaries() :: [ne_binary()].
%% Denotes a list of non-empty binaries.

-type binaries() :: [binary()].

-type strings() :: [string()].
-type integers() :: [integer()].

-type functions() :: [function()].

-export_type([api_atom/0
             ,api_atoms/0
             ,api_binaries/0
             ,api_binary/0
             ,api_boolean/0
             ,api_float/0
             ,api_integer/0
             ,api_integers/0
             ,api_list/0
             ,api_ne_binaries/0
             ,api_ne_binary/0
             ,api_non_neg_integer/0
             ,api_number/0
             ,api_object/0
             ,api_objects/0
             ,api_pid/0
             ,api_pid_ref/0
             ,api_pid_refs/0
             ,api_pos_integer/0
             ,api_proplist/0
             ,api_reference/0
             ,api_port/0
             ,api_string/0
             ,api_terms/0
             ,atoms/0
             ,binaries/0
             ,deeplist/0
             ,functions/0
             ,integers/0
             ,jobj_return/0
             ,jobjs_return/0
             ,ne_binaries/0
             ,ne_binary/0
             ,pid_ref/0
             ,pid_refs/0
             ,pids/0
             ,proplist/0
             ,proplist_key/0
             ,proplist_kv/2
             ,proplist_property/0
             ,proplist_value/0
             ,proplists/0
             ,references/0
             ,std_return/0
             ,strings/0
             ,sup_no_return/0
             ,text/0
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec shuffle_list(list()) -> list().
shuffle_list([]) -> [];
shuffle_list(List) when is_list(List) ->
    randomize_list(round(math:log(length(List)) + 0.5), List).

-spec randomize_list(list()) -> list().
randomize_list(List) ->
    D = lists:keysort(1, [{rand:uniform(), A} || A <- List]),
    {_, D1} = lists:unzip(D),
    D1.

-spec randomize_list(pos_integer(), list()) -> list().
randomize_list(1, List) -> randomize_list(List);
randomize_list(T, List) ->
    lists:foldl(fun(_E, Acc) ->
                        randomize_list(Acc)
                end
               ,randomize_list(List)
               ,lists:seq(1, (T - 1))
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec uniq_list(list()) -> list().
uniq_list([]) -> [];
uniq_list([H|T]) -> [H | [X || X <- uniq_list(T), X =/= H]].

%%------------------------------------------------------------------------------
%% @doc must be a term that can be changed to a list
%% @end
%%------------------------------------------------------------------------------
-spec to_hex(text()) -> string().
to_hex(S) ->
    B = to_hex_binary(S),
    to_list(B).

-spec to_hex_binary(text()) -> binary().
to_hex_binary(S) ->
    Bin = to_binary(S),
    << <<(to_hex_char(B div 16)), (to_hex_char(B rem 16))>> || <<B>> <= Bin>>.

-spec to_integer(string() | binary() | integer() | float()) -> integer().
to_integer(X) -> to_integer(X, 'notstrict').

-spec to_integer(string() | binary() | integer() | float(), 'strict' | 'notstrict') -> integer().
to_integer(X, _) when is_integer(X) -> X;
to_integer(X, 'strict') when is_float(X) -> erlang:error('badarg');
to_integer(X, 'notstrict') when is_float(X) -> round(X);
to_integer(X, 'strict') when is_binary(X) -> binary_to_integer(X);
to_integer(X, 'notstrict') when is_binary(X) ->
    try binary_to_integer(X)
    catch 'error':'badarg' -> round(binary_to_float(X))
    end;
to_integer(X, S) when is_list(X) ->
    try list_to_integer(X)
    catch
        'error':'badarg' when S =:= 'notstrict' ->
            round(list_to_float(X))
    end.

-spec to_float(string() | binary() | integer() | float()) -> float().
to_float(X) -> to_float(X, 'notstrict').

-spec to_float(string() | binary() | integer() | float(), 'strict' | 'notstrict') -> float().
to_float(X, _) when is_float(X) -> X;
to_float(X, 'strict') when is_binary(X) -> binary_to_float(X);
to_float(X, 'notstrict') when is_binary(X) ->
    try binary_to_float(X)
    catch 'error':'badarg' -> binary_to_integer(X) * 1.0
    end;
to_float(X, S) when is_list(X) ->
    try list_to_float(X)
    catch 'error':'badarg' when S =:= 'notstrict' -> 1.0 * list_to_integer(X)
    end;
to_float(X, 'strict') when is_integer(X) -> erlang:error('badarg');
to_float(X, 'notstrict') when is_integer(X) -> X * 1.0.

-spec to_number(binary() | string() | number()) -> number().
to_number(X) when is_number(X) -> X;
to_number(X) when is_binary(X) ->
    try binary_to_integer(X)
    catch 'error':'badarg' -> binary_to_float(X)
    end;
to_number(X) when is_list(X) ->
    try list_to_integer(X)
    catch 'error':'badarg' -> list_to_float(X)
    end.

-spec to_pid(pid() | list() | binary() | atom()) -> api_pid().
to_pid('undefined') -> 'undefined';
to_pid(A) when is_atom(A) -> to_pid(whereis(A));
to_pid(P) when is_pid(P) -> P;
to_pid(X) when is_binary(X) -> to_pid(binary_to_list(X));
to_pid(X) when is_list(X) -> list_to_pid(X).

-spec to_list(pid() | atom() | list() | binary() | integer() | float()) -> list().
to_list(X) when is_list(X) -> X;
to_list(X) when is_float(X) -> kz_mochinum:digits(X);
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_pid(X) -> pid_to_list(X).

%% Known limitations:
%%   Converting [256 | _], lists with integers > 255
-spec to_binary(kz_json:object() | atom() | string() | binary() | integer() | float() | pid() | iolist()) -> binary().
to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_float(X) -> to_binary(kz_mochinum:digits(X));
to_binary(X) when is_integer(X) -> integer_to_binary(X);
to_binary(X) when is_atom(X) -> atom_to_binary(X, utf8);
to_binary(X) when is_list(X) -> iolist_to_binary(X);
to_binary(X) when is_pid(X) -> to_binary(pid_to_list(X));
to_binary(X) ->
    case kz_json:is_json_object(X) of
        'true' -> kz_json:encode(X);
        'false' -> error('badarg')
    end.

-spec to_api_binary(atom() | string() | binary() | integer() | float() | pid() | iolist()) -> api_binary().
to_api_binary('undefined') -> 'undefined';
to_api_binary(Arg) -> to_binary(Arg).

-spec to_api_term(Arg) -> 'undefined' | Arg.
to_api_term(Arg) ->
    case is_empty(Arg) of
        'true' -> 'undefined';
        'false' -> Arg
    end.

%% the safer version, won't let you leak atoms
-spec to_atom(text() | integer() | float()) -> atom().
to_atom(X) when is_atom(X) -> X;
to_atom(X) when is_list(X) -> list_to_existing_atom(X);
to_atom(X) when is_binary(X) -> binary_to_existing_atom(X, 'utf8');
to_atom(X) -> to_atom(to_list(X)).

%% only if you're really sure you want this
%% to protect yourself a bit from overrunning the atom table,
%% pass a list of safe values for X
%% so if X is a binary, the SafeList would be [ne_binary(),...]
%% if X is a list, the SafeList would be [nonempty_string(),...]
%% etc. So to_atom will not coerce the type of X to match the types in SafeList
%% when doing the lists:member/2
-spec to_atom(text() | integer() | float(), boolean() | list()) -> atom().
to_atom(X, _) when is_atom(X) -> X;
to_atom(X, 'true') when is_list(X) -> list_to_atom(X);
to_atom(X, 'true') when is_binary(X) -> binary_to_atom(X, 'utf8');
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

-spec to_date(binary() | string() | integer()) -> kz_time:date().
to_date(X) ->
    {Date, _ } = to_datetime(X),
    Date.

-spec to_datetime(binary() | string() | integer()) -> kz_time:datetime().
to_datetime(X) when is_integer(X) -> calendar:gregorian_seconds_to_datetime(X);
to_datetime(X) when is_binary(X) -> to_datetime(to_integer(X));
to_datetime(X) when is_list(X) -> to_datetime(to_integer(X)).

-spec is_true(binary() | string() | atom()) -> boolean().
is_true(<<"true">>) -> 'true';
is_true("true") -> 'true';
is_true('true') -> 'true';
is_true(_) -> 'false'.

-type caster() :: fun((any()) -> any()).
-spec safe_cast(any(), any(), caster()) -> any().
safe_cast(Value, Default, CastFun) ->
    try CastFun(Value)
    catch
        _:_ -> Default
    end.

-spec always_true(any()) -> 'true'.
always_true(_) -> 'true'.

-spec is_false(binary() | string() | atom()) -> boolean().
is_false(<<"false">>) -> 'true';
is_false("false") -> 'true';
is_false('false') -> 'true';
is_false(_) -> 'false'.

-spec always_false(any()) -> 'false'.
always_false(_) -> 'false'.

-spec is_ne_binary(any()) -> boolean().
is_ne_binary(V) ->
    is_binary(V)
        andalso not is_empty(V).

-spec is_api_ne_binary(any()) -> boolean().
is_api_ne_binary('undefined') -> 'true';
is_api_ne_binary(V) -> is_ne_binary(V).

-spec is_ne_binaries(any()) -> boolean().
is_ne_binaries([]) -> 'true';
is_ne_binaries(V) when is_list(V) ->
    lists:all(fun is_ne_binary/1, V);
is_ne_binaries(_) -> 'false'.

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
is_empty("NULL") -> 'true';
is_empty("undefined") -> 'true';
is_empty([_|_]) -> 'false';

is_empty(<<>>) -> 'true';
is_empty(<<"0">>) -> 'true';
is_empty(<<"NULL">>) -> 'true';
is_empty(<<"undefined">>) -> 'true';
is_empty(<<_/binary>>) -> 'false';

is_empty('null') -> 'true';
is_empty('undefined') -> 'true';

is_empty(Float) when is_float(Float), Float =:= 0.0 -> 'true';

is_empty(Map) when is_map(Map), map_size(Map) =:= 0 -> 'true';
is_empty(Map) when is_map(Map) -> 'false';

is_empty(MaybeJObj) ->
    case kz_json:is_json_object(MaybeJObj) of
        'false' -> 'false'; %% if not a json object, it's not empty
        'true' -> kz_json:is_empty(MaybeJObj)
    end.

-spec is_not_empty(any()) -> boolean().
is_not_empty(Term) -> not is_empty(Term).

-spec is_proplist(any()) -> boolean().
is_proplist(Term) when is_list(Term) ->
    lists:all(fun({_,_}) -> 'true'; (A) -> is_atom(A) end, Term);
is_proplist(_) -> 'false'.

-spec is_ne_list(any()) -> boolean().
is_ne_list([_|_]) -> 'true';
is_ne_list(_) -> 'false'.

-spec is_pos_integer(any()) -> boolean().
is_pos_integer(X) ->
    is_integer(X)
        andalso X > 0.

-spec is_ascii_code(non_neg_integer()) -> boolean().
is_ascii_code(X) ->
    X >= 0
        andalso X =< 127.

-spec is_ascii_number(integer()) -> boolean().
is_ascii_number(X) ->
    X >= $0
        andalso X =< $9.

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

-spec is_upper_char(integer()) -> boolean().
is_upper_char(C) when is_integer(C), $A =< C, C =< $Z -> 'true';
is_upper_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 -> 'true';
is_upper_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE -> 'true';
is_upper_char(_) -> 'false'.

-spec to_lower_char(char()) -> char().
to_lower_char(C) when is_integer(C), $A =< C, C =< $Z -> C + 32;
%% Converts Latin capital letters to lowercase, skipping 16#D7 (extended ASCII 215) "multiplication sign: x"
to_lower_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 -> C + 32; % from string:to_lower
to_lower_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE -> C + 32; % so we only loop once
to_lower_char(C) -> C.

-spec is_lower_char(integer()) -> boolean().
is_lower_char(C) when is_integer(C), $a =< C, C =< $z -> 'true';
is_lower_char(C) when is_integer(C), 16#E0 =< C, C =< 16#F6 -> 'true';
is_lower_char(C) when is_integer(C), 16#F8 =< C, C =< 16#FE -> 'true';
is_lower_char(_)  -> 'false'.

-spec a1hash(ne_binary(), ne_binary(), ne_binary()) -> nonempty_string().
a1hash(User, Realm, Password) ->
    to_hex(erlang:md5(list_to_binary([User,":",Realm,":",Password]))).

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

-spec to_hex_char(pos_integer()) -> pos_integer().
to_hex_char(N) when N < 10 -> $0 + N;
to_hex_char(N) when N < 16 -> $a - 10 + N.

-spec error_to_binary(any()) -> binary().
error_to_binary({'error', Reason}) ->
    error_to_binary(Reason);
error_to_binary(Reason) ->
    try to_binary(Reason)
    catch
        'error':'function_clause' -> <<"Unknown Error">>;
        'error':'badarg' -> <<"Unknown Error">>
    end.

-spec words_to_bytes(integer()) -> integer().
words_to_bytes(Words) ->
    Words * erlang:system_info('wordsize').

-spec iolist_join(Sep, List1) -> List2 when
      Sep :: T,
      List1 :: [T],
      List2 :: [T],
      T :: iodata() | char().
iolist_join(_, []) -> [];
iolist_join(Sep, [H|T]) ->
    [H | iolist_join_prepend(Sep, T)].

-spec iolist_join_prepend(Sep, List1) -> List2 when
      Sep :: T,
      List1 :: [T],
      List2 :: [T],
      T :: iolist().
iolist_join_prepend(_, []) -> [];
iolist_join_prepend(Sep, [H|T]) ->
    [Sep, H | iolist_join_prepend(Sep, T)].
