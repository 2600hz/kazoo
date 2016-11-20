%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Caller calls in and gets the first available call from the queue
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_queue_thief).

-export([connect/2]).

-include_lib("kazoo/include/kz_types.hrl").

-spec connect(kapps_call:call(), ne_binary()) -> 'ok'.
connect(_Call, _QueueId) ->
    %% no!
    ok.
