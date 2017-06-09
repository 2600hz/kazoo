%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2017, 2600Hz INC
%%% @doc
%%% Conversion of types
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
        ,to_hex/1, to_hex_binary/1, to_hex_char/1
        ,to_list/1
        ,to_binary/1
        ,to_api_binary/1
        ,to_atom/1, to_atom/2
        ,to_boolean/1
        ,to_date/1
        ,to_datetime/1
        ,to_lower_binary/1, to_upper_binary/1
        ,to_lower_string/1, to_upper_string/1
        ,to_upper_char/1
        ,to_lower_char/1

        ,error_to_binary/1
        ]).
-export([is_true/1, is_false/1
        ,is_boolean/1
        ,is_ne_binary/1, is_api_ne_binary/1
        ,is_ne_binaries/1
        ,is_empty/1, is_not_empty/1
        ,is_proplist/1
        ,identity/1
        ,always_true/1, always_false/1
        ]).

-export([a1hash/3, floor/1, ceiling/1]).

-include_lib("kazoo_types/include/kz_types.hrl").

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec shuffle_list(list()) -> list().
shuffle_list([]) -> [];
shuffle_list(List) when is_list(List) ->
    randomize_list(round(math:log(length(List)) + 0.5), List).

-spec randomize_list(list()) -> list().
-spec randomize_list(pos_integer(), list()) -> list().
randomize_list(List) ->
    D = lists:keysort(1, [{rand:uniform(), A} || A <- List]),
    {_, D1} = lists:unzip(D),
    D1.

randomize_list(1, List) -> randomize_list(List);
randomize_list(T, List) ->
    lists:foldl(fun(_E, Acc) ->
                        randomize_list(Acc)
                end
               ,randomize_list(List)
               ,lists:seq(1, (T - 1))
               ).

%% must be a term that can be changed to a list
-spec to_hex(binary() | string()) -> string().
to_hex(S) ->
    string:to_lower(lists:flatten([io_lib:format("~2.16.0B", [H]) || H <- to_list(S)])).

-spec to_hex_binary(binary() | string()) -> binary().
to_hex_binary(S) ->
    Bin = to_binary(S),
    << <<(to_hex_char(B div 16)), (to_hex_char(B rem 16))>> || <<B>> <= Bin>>.


-spec to_integer(string() | binary() | integer() | float()) -> integer().
-spec to_integer(string() | binary() | integer() | float(), 'strict' | 'notstrict') -> integer().
to_integer(X) -> to_integer(X, 'notstrict').

to_integer(X, 'strict') when is_float(X) -> erlang:error('badarg');
to_integer(X, 'notstrict') when is_float(X) -> round(X);
to_integer(X, S) when is_binary(X) -> to_integer(binary_to_list(X), S);
to_integer(X, S) when is_list(X) ->
    try list_to_integer(X)
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
    try list_to_float(X)
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
    try list_to_integer(X)
    catch
        'error':'badarg' -> list_to_float(X)
    end.

-spec to_list(ets:tab() | atom() | list() | binary() | integer() | float()) -> list().
to_list(X) when is_float(X) -> kz_mochinum:digits(X);
to_list(X) when is_integer(X) -> integer_to_list(X);
to_list(X) when is_binary(X) -> binary_to_list(X);
to_list(X) when is_atom(X) -> atom_to_list(X);
to_list(X) when is_list(X) -> X.

%% Known limitations:
%%   Converting [256 | _], lists with integers > 255
-spec to_binary(atom() | string() | binary() | integer() | float() | pid() | iolist()) -> binary().
to_binary(X) when is_float(X) -> to_binary(kz_mochinum:digits(X));
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X));
to_binary(X) when is_list(X) -> iolist_to_binary(X);
to_binary(X) when is_pid(X) -> to_binary(pid_to_list(X));
to_binary(X) when is_binary(X) -> X.

-spec to_api_binary(atom() | string() | binary() | integer() | float() | pid() | iolist()) -> api_binary().
to_api_binary('undefined') -> 'undefined';
to_api_binary(Arg) -> to_binary(Arg).

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

-spec to_date(binary() | string() | integer()) -> kz_date().
to_date(X) ->
    {Date, _ } = to_datetime(X),
    Date.

-spec to_datetime(binary() | string() | integer()) -> kz_datetime().
to_datetime(X) when is_integer(X) -> calendar:gregorian_seconds_to_datetime(X);
to_datetime(X) when is_binary(X) -> to_datetime(to_integer(X));
to_datetime(X) when is_list(X) -> to_datetime(to_integer(X)).

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

-spec is_ne_binary(any()) -> boolean().
is_ne_binary(V) ->
    is_binary(V)
        andalso is_not_empty(V).

-spec is_api_ne_binary(any()) -> boolean().
is_api_ne_binary(undefined) -> true;
is_api_ne_binary(V) -> is_ne_binary(V).

-spec is_ne_binaries(any()) -> boolean().
is_ne_binaries([]) -> true;
is_ne_binaries(V)
  when is_list(V) ->
    lists:all(fun is_ne_binary/1, V);
is_ne_binaries(_) -> false.

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
to_lower_binary(Else) -> to_lower_binary(?MODULE:to_binary(Else)).

-spec to_lower_string(any()) -> 'undefined' | list().
to_lower_string('undefined') -> 'undefined';
to_lower_string(L) when is_list(L) ->
    [to_lower_char(C) || C <- L];
to_lower_string(Else) ->
    to_lower_string(?MODULE:to_list(Else)).


-spec to_upper_binary(any()) -> api_binary().
to_upper_binary('undefined') -> 'undefined';
to_upper_binary(Bin) when is_binary(Bin) -> << <<(to_upper_char(B))>> || <<B>> <= Bin>>;
to_upper_binary(Else) -> to_upper_binary(?MODULE:to_binary(Else)).

-spec to_upper_string(any()) -> 'undefined' | list().
to_upper_string('undefined') -> 'undefined';
to_upper_string(L) when is_list(L) -> [to_upper_char(C) || C <- L];
to_upper_string(Else) -> to_upper_string(?MODULE:to_list(Else)).

-spec to_upper_char(char()) -> char().
to_upper_char(C) when is_integer(C), $a =< C, C =< $z -> C - 32;
to_upper_char(C) when is_integer(C), 16#E0 =< C, C =< 16#F6 -> C - 32;
to_upper_char(C) when is_integer(C), 16#F8 =< C, C =< 16#FE -> C - 32;
to_upper_char(C) -> C.

-spec to_lower_char(char()) -> char().
to_lower_char(C) when is_integer(C), $A =< C, C =< $Z -> C + 32;
%% Converts latin capital letters to lowercase, skipping 16#D7 (extended ascii 215) "multiplication sign: x"
to_lower_char(C) when is_integer(C), 16#C0 =< C, C =< 16#D6 -> C + 32; % from string:to_lower
to_lower_char(C) when is_integer(C), 16#D8 =< C, C =< 16#DE -> C + 32; % so we only loop once
to_lower_char(C) -> C.

-spec a1hash(ne_binary(), ne_binary(), ne_binary()) -> nonempty_string().
a1hash(User, Realm, Password) ->
    ?MODULE:to_hex(erlang:md5(list_to_binary([User,":",Realm,":",Password]))).

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

-spec error_to_binary({'error', binary()} | binary()) -> binary().
error_to_binary({'error', Reason}) ->
    error_to_binary(Reason);
error_to_binary(Reason) ->
    try to_binary(Reason)
    catch
        _:_ -> <<"Unknown Error">>
    end.
