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
        ,left/1, from_left/1
        ,right/1, from_right/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-type left() :: left(any()).
-type left(T) :: {'error', T}.

-type right() :: right(any()).
-type right(T) :: {'ok', T}.

-type either() :: either(left(), right()).
-type either(L) :: either(L, right()).
-type either(L, R) :: left(L) | right(R).

-type fn_bind(L, R) :: fun((R) -> either(L, R)).

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
-spec bind(either(L, R), fn_bind(L, R)) -> either(L, R).
bind({'ok', Val}, Fn) -> Fn(Val);
bind({'error', _}=Left, _Fn) -> Left.
