%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(kazoo_data_maintenance).

-include("kz_data.hrl").

-export([flush/0]).
-export([flush_data_plans/0]).
-export([flush_docs/0
        ,flush_docs/1
        ,flush_docs/2
        ]).
-export([trace_module/1
        ,trace_function/1, trace_function/2
        ,trace_pid/1
        ,stop_trace/1
        ]).
-export([open_document/2
        ,open_document_cached/2
        ]).

-spec flush() -> 'ok'.
flush() ->
    _ = kz_datamgr:flush_cache_docs(),
    _ = kzs_plan:flush(),
    io:format("flushed all data manager caches~n").

-spec flush_data_plans() -> 'ok'.
flush_data_plans() ->
    _ = kzs_plan:flush(),
    io:format("flushed all data plans~n").

-spec flush_docs() -> 'ok'.
-spec flush_docs(ne_binary()) -> 'ok'.
-spec flush_docs(ne_binary(), ne_binary()) -> 'ok'.
flush_docs() ->
    _ = kz_datamgr:flush_cache_docs(),
    io:format("flushed all cached docs~n").

flush_docs(Account) ->
    _ = kz_datamgr:flush_cache_docs(kz_util:format_account_db(Account)),
    io:format("flushed all docs cached for account ~s~n", [Account]).

flush_docs(Account, DocId) ->
    _ = kz_datamgr:flush_cache_doc(kz_util:format_account_db(Account), DocId),
    io:format("flushed cached doc ~s for account ~s~n", [DocId, Account]).

-spec trace_module(ne_binary()) -> 'ok'.
trace_module(Module) ->
    start_trace([{'module', kz_term:to_atom(Module)}]).

-spec trace_function(ne_binary()) -> 'ok'.
-spec trace_function(ne_binary(), ne_binary()) -> 'ok'.
trace_function(Function) ->
    start_trace([{'function', kz_term:to_atom(Function)}]).

trace_function(Module, Function) ->
    start_trace([{'module', kz_term:to_atom(Module)}
                ,{'function', kz_term:to_atom(Function)}
                ]).

-spec trace_pid(ne_binary()) -> 'ok'.
trace_pid(Pid) ->
    start_trace([{'pid', kz_term:to_list(Pid)}]).

-spec start_trace(kz_data_tracing:filters()) -> 'ok'.
start_trace(Filters) ->
    {'ok', Ref} = kz_data_tracing:trace_file(Filters),
    io:format("trace started, stop with 'sup kazoo_data_maintenance stop_trace ~s'~n", [Ref]).

-spec stop_trace(ne_binary()) -> 'ok'.
stop_trace(Ref) ->
    {'ok', Filename} = kz_data_tracing:stop_trace(Ref),
    io:format("trace stopped, see log at ~s~n", [Filename]).


-spec open_document(ne_binary(), ne_binary()) -> no_return.
open_document(Db, Id) ->
    print(kz_datamgr:open_doc(Db, Id)).

-spec open_document_cached(ne_binary(), ne_binary()) -> no_return.
open_document_cached(Db, Id) ->
    print(kz_datamgr:open_cache_doc(Db, Id)).

print({ok, JSON}) ->
    io:format("~s\n", [kz_json:encode(JSON)]),
    no_return;
print({error, R}) ->
    io:format("ERROR: ~p\n", [R]),
    no_return.
