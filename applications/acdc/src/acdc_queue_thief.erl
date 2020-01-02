%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc Caller calls in and gets the first available call from the queue
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_queue_thief).

-export([connect/2]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-spec connect(kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
connect(_Call, _QueueId) ->
    %% no!
    ok.
