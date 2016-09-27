%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz
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

        ,trace_module/1
        ,trace_function/1, trace_function/2
        ,trace_pid/1
        ,stop_trace/1
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

-spec trace_module(ne_binary()) -> 'ok'.
trace_module(Module) ->
    start_trace([{'module', kz_util:to_atom(Module)}]).

-spec trace_function(ne_binary()) -> 'ok'.
-spec trace_function(ne_binary(), ne_binary()) -> 'ok'.
trace_function(Function) ->
    start_trace([{'function', kz_util:to_atom(Function)}]).

trace_function(Module, Function) ->
    start_trace([{'module', kz_util:to_atom(Module)}
                ,{'function', kz_util:to_atom(Function)}
                ]).

-spec trace_pid(ne_binary()) -> 'ok'.
trace_pid(Pid) ->
    start_trace([{'pid', kz_util:to_list(Pid)}]).

-spec start_trace(kz_data_tracing:filters()) -> 'ok'.
start_trace(Filters) ->
    {'ok', Ref} = kz_data_tracing:trace_file(Filters),
    io:format("trace started, stop with 'sup kazoo_data_maintenance stop_trace ~s'~n", [Ref]).

-spec stop_trace(ne_binary()) -> 'ok'.
stop_trace(Ref) ->
    {'ok', Filename} = kz_data_tracing:stop_trace(Ref),
    io:format("trace stopped, see log at ~s~n", [Filename]).
