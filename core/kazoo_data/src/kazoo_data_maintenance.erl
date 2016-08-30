%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kazoo_data_maintenance).

-include("kz_data.hrl").

-export([flush/0
         ,flush/1
         ,flush/2
        ]).

-spec flush() -> 'ok'.
-spec flush(ne_binary()) -> 'ok'.
-spec flush(ne_binary(), ne_binary()) -> 'ok'.
flush() ->
    _ = kz_datamgr:flush_cache_docs(),
    io:format("flushed all cached docs~n").

flush(Account) ->
    _ = kz_datamgr:flush_cache_docs(kz_util:format_account_db(Account)),
    io:format("flushed all docs cached for account ~s~n", [Account]).

flush(Account, DocId) ->
    _ = kz_datamgr:flush_cache_doc(kz_util:format_account_db(Account), DocId),
    io:format("flushed cached doc ~s for account ~s~n", [DocId, Account]).
