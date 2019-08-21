%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(gcm_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), any()) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
    gcm_sup:start_link().

-spec stop(any()) -> ok.
stop(_State) ->
    ok.
