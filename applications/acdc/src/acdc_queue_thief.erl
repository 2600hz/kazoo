%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Caller calls in and gets the first available call from the queue
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_queue_thief).

-export([connect/2]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-spec connect(kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
connect(_Call, _QueueId) ->
    %% no!
    ok.
