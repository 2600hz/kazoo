%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(crossbar_websocket).

-export([recv/2
        ]).

-type cb_ws_return() :: {cowboy_websocket:commands(), cb_context:context(), 'hibernate'}.

-spec recv(cow_ws:frame(), cb_context:context()) -> cb_ws_return().
recv({'text', _Data}, State) ->
    {[{text, <<"{\"data\":\"responce\"}">>}], State, 'hibernate'};
recv('ping', State) ->
    {[], State, 'hibernate'};
recv( Other, State) ->
    lager:debug("not handling message : ~p", [Other]),
    {[], State, 'hibernate'}.
