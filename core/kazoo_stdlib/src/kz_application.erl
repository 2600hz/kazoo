%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_application).

-export([get_application/0
        ,put_application/1
        ]).

-spec get_application() -> atom().
get_application() ->
    case get('application') of
        'undefined' -> find_application();
        Application -> Application
    end.

-spec find_application() -> atom().
find_application() ->
    case application:get_application() of
        {'ok', Application} ->
            Application;
        _Else -> 'undefined'
    end.

-spec put_application(atom()) -> atom().
put_application(Application) ->
    put('application', Application).
