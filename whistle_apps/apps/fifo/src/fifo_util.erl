%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(fifo_util).

-export([find_fifo_queue/2, find_fifo_queue/3
         ,store_fifo_queue/3, store_fifo_queue/4
        ]).

-include("fifo.hrl").

-spec find_fifo_queue/2 :: (ne_binary(), ne_binary()) -> {'ok', pid()} |
                                                         {'error', 'not_found'}.
-spec find_fifo_queue/3 :: (ne_binary(), ne_binary(), pid()) -> {'ok', pid()} |
                                                                {'error', 'not_found'}.
find_fifo_queue(AcctDB, QID) ->
    {ok, Cache} = fifo_sup:cache_proc(),
    find_fifo_queue(AcctDB, QID, Cache).
find_fifo_queue(AcctDB, QID, Cache) ->
    case wh_cache:peek_local(Cache, queue_cache_key(AcctDB, QID)) of
        {error, not_found}=E -> E;
        {ok, Pid} ->
            case is_process_alive(Pid) of
                true -> {ok, Pid};
                false ->
                    wh_cache:erase_local(Cache, queue_cache_key(AcctDB, QID)),
                    {error, not_found}
            end
    end.

-spec store_fifo_queue/3 :: (ne_binary(), ne_binary(), pid()) -> 'ok'.
-spec store_fifo_queue/4 :: (ne_binary(), ne_binary(), pid(), pid()) -> 'ok'.
store_fifo_queue(AcctDB, QID, Fifo) ->
    {ok, Cache} = fifo_sup:cache_proc(),
    store_fifo_queue(AcctDB, QID, Fifo, Cache).
store_fifo_queue(AcctDB, QID, Fifo, Cache) ->
    wh_cache:store_local(Cache, queue_cache_key(AcctDB, QID), Fifo).

-spec queue_cache_key/2 :: (A, B) -> {?MODULE, 'queue', A, B}.
queue_cache_key(AcctDB, QID) ->
    {?MODULE, queue, AcctDB, QID}.
