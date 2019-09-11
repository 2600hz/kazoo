%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Preforms maintenance operations against the stepswitch dbs
%%% @author Karl Anderson
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cnam_maintenance).

-export([flush/0]).

-include("cnam.hrl").

%%------------------------------------------------------------------------------
%% @doc Flush the stepswitch local cache
%% @end
%%------------------------------------------------------------------------------
-spec flush() -> 'ok'.
flush() ->
    io:format("flushed ~p entries from cnam cache~n", [cnam:flush()]).
