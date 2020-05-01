%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Functor.
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_either).

-export([bind/2
        ,cata/3
        ,catar/2
        ,from_maybe/1
        ,left/1, from_left/1
        ,pipe/2
        ,right/1, from_right/1
        ,unless/2
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-type left() :: left(any()).
-type left(T) :: {'error', T}.

-type right() :: right(any()).
-type right(T) :: {'ok', T}.

-type either() :: either(left(), right()).
-type either(L) :: either(L, right()).
-type either(L, R) :: left(L) | right(R).

-export_type([either/0, either/1, either/2
             ,left/0, left/1
             ,right/0, right/1
             ]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec left(left(T)) -> T.
left({'error', Val}) -> Val.

-spec from_left(T) -> left(T).
from_left(Val) -> {'error', Val}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec right(right(T)) -> T.
right({'ok', Val}) -> Val.

-spec from_right(T) -> right(T).
from_right(Val) -> {'ok', Val}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec from_maybe(any()) -> either().
from_maybe('true') -> from_right('true');
from_maybe('false') -> from_left('false');
from_maybe(Term) ->
    case kz_term:is_empty(Term) of
        'true' -> from_left(Term);
        'false' -> from_right(Term)
    end.

%%------------------------------------------------------------------------------
%% @doc This will apply the supplied function over the right side of the either,
%% if one exists, otherwise it returns the Either untouched.
%%
%% For example:
%% ```
%%     %% Right value:
%%     bind({'ok', "World"}
%%         ,fun(String) -> {'ok', "Hello " ++ String ++ "!"} end
%%         ). %% {'ok', "Hello World!"}
%%
%%     %% Left value:
%%     bind({'error', "Evil"}
%%         ,fun(String) -> {'ok', "Hello " ++ String ++ "!"} end
%%         ). %% {'error', "Evil"}
%% '''
%%
%% @see unless/2
%% @end
%%------------------------------------------------------------------------------
-spec bind(either(L, R), fun((R) -> X)) -> X when X :: either(L, any()).
bind({'ok', Val}, Fn) -> Fn(Val);
bind({'error', _}=Left, _Fn) -> Left.

%%------------------------------------------------------------------------------
%% @doc This will apply the supplied function over the left side of the either,
%% if one exists, otherwise it returns the Either untouched.
%%
%% @see bind/2
%% @end
%%------------------------------------------------------------------------------
-spec unless(either(L, R), fun((L) -> X)) -> X when X :: either(any(), R).
unless({'ok', _}=Right, _) -> Right;
unless({'error', Val}, Fn) -> Fn(Val).

%%------------------------------------------------------------------------------
%% @doc The catamorphism for either. If the either is right the right function
%% will be executed with the right value and the value of the function returned.
%% Otherwise the left function will be called with the left value.
%%
%% This is just like lazy case caluse for normal ok and error tuples.
%% @end
%%------------------------------------------------------------------------------
-spec cata(either(L, R), fun((L) -> X), fun((R) -> X)) -> X.
cata({'ok', Val}, _LFn, RFn) -> RFn(Val);
cata({'error', Val}, LFn, _RFn) -> LFn(Val).

-spec catar(either(L, R), fun((R) -> X)) -> X | left(L).
catar({'ok', Val}, RFn) -> RFn(Val);
catar({'error', _}=Left, _RFn) -> Left.

%%------------------------------------------------------------------------------
%% @doc Chain monadic function to apply to `Either'.
%%
%% You might use it
%% like `pipe({ok, 5}, [Increment, Increment, Increment])'.
%% @end
%%------------------------------------------------------------------------------
-spec pipe(either(L, R), [fun((R) -> X)]) -> either(L, X).
pipe({'error', _}=Left, _) -> Left;
pipe({'ok', _}=Right, []) -> Right;
pipe({'ok', Val}, [Fun|Funs]) -> pipe(Fun(Val), Funs).
