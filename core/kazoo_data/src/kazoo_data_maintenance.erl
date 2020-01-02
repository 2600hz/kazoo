%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
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

-export([load_doc_from_file/2]).

-export([cache_strategy/0, set_cache_strategy/1]).

-export([dataplan_reload/0, dataplan_reload/1]).

-spec flush() -> 'ok'.
flush() ->
    _ = kz_datamgr:flush_cache_docs(),
    _ = dataplan_reload(),
    io:format("flushed all data manager caches~n").

-spec flush_data_plans() -> 'ok'.
flush_data_plans() ->
    dataplan_reload().

-spec flush_docs() -> 'ok'.
flush_docs() ->
    _ = kz_datamgr:flush_cache_docs(),
    io:format("flushed all cached docs~n").

-spec flush_docs(kz_term:ne_binary()) -> 'ok'.
flush_docs(Account) ->
    _ = kz_datamgr:flush_cache_docs(kzs_util:format_account_db(Account)),
    io:format("flushed all docs cached for account ~s~n", [Account]).

-spec flush_docs(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
flush_docs(Account, DocId) ->
    _ = kz_datamgr:flush_cache_doc(kzs_util:format_account_db(Account), DocId),
    io:format("flushed cached doc ~s for account ~s~n", [DocId, Account]).

-spec trace_module(kz_term:ne_binary()) -> 'ok'.
trace_module(Module) ->
    start_trace([{'module', kz_term:to_atom(Module)}]).

-spec trace_function(kz_term:ne_binary()) -> 'ok'.
trace_function(Function) ->
    start_trace([{'function', kz_term:to_atom(Function)}]).

-spec trace_function(kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
trace_function(Module, Function) ->
    start_trace([{'module', kz_term:to_atom(Module)}
                ,{'function', kz_term:to_atom(Function)}
                ]).

-spec trace_pid(kz_term:ne_binary()) -> 'ok'.
trace_pid(Pid) ->
    start_trace([{'pid', kz_term:to_list(Pid)}]).

-spec start_trace(kz_data_tracing:filters()) -> 'ok'.
start_trace(Filters) ->
    {'ok', Ref} = kz_data_tracing:trace_file(Filters),
    io:format("trace started, stop with 'sup kazoo_data_maintenance stop_trace ~s'~n", [Ref]).

-spec stop_trace(kz_term:ne_binary()) -> 'ok'.
stop_trace(Ref) ->
    {'ok', Filename} = kz_data_tracing:stop_trace(Ref),
    io:format("trace stopped, see log at ~s~n", [Filename]).


-spec open_document(kz_term:ne_binary(), kz_term:ne_binary()) -> no_return.
open_document(Db, Id) ->
    print(kz_datamgr:open_doc(Db, Id)).

-spec open_document_cached(kz_term:ne_binary(), kz_term:ne_binary()) -> no_return.
open_document_cached(Db, Id) ->
    print(kz_datamgr:open_cache_doc(Db, Id)).

print({ok, JSON}) ->
    io:format("~s\n", [kz_json:encode(JSON)]),
    no_return;
print({error, R}) ->
    io:format("ERROR: ~p\n", [R]),
    no_return.

-spec load_doc_from_file(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          data_error().
load_doc_from_file(Db, _FilePath) when size(Db) == 0 ->
    {'error', 'invalid_db_name'};
load_doc_from_file(Db, FilePath) ->
    lager:debug("update db ~s from CouchDB file: ~s", [Db, FilePath]),
    try
        {'ok', Bin} = file:read_file(FilePath),
        JObj = kz_datamgr:maybe_adapt_multilines(kz_json:decode(Bin)),
        kz_datamgr:maybe_update_doc(Db, JObj)
    catch
        _Type:{'badmatch',{'error',Reason}} ->
            lager:debug("bad match: ~p", [Reason]),
            {'error', Reason};
        _Type:Reason ->
            lager:debug("exception: ~p", [Reason]),
            {'error', Reason}
    end.

-spec set_cache_strategy(kz_types:text()) -> 'ok'.
set_cache_strategy(?NE_BINARY = StrategyBin) ->
    set_cache_strategy(kz_term:to_atom(StrategyBin));
set_cache_strategy(Strategy) when is_atom(Strategy) ->
    application:set_env('kazoo_data', 'cache_strategy', Strategy),
    cache_strategy().

-spec cache_strategy() -> 'ok'.
cache_strategy() ->
    Strategy = kzs_cache:cache_strategy(),
    io:format("             strategy: ~s~n", [Strategy]),
    print_strategy_details(Strategy).

-define(STRATEGY_DETAILS(Mitigate, Async)
       ,io:format("  stampede mitigation: ~s~n          async store: ~s~n", [Mitigate, Async])
       ).

print_strategy_details('none') ->
    ?STRATEGY_DETAILS('false', 'false');
print_strategy_details('stampede') ->
    ?STRATEGY_DETAILS('true', 'false');
print_strategy_details('async') ->
    ?STRATEGY_DETAILS('false', 'true');
print_strategy_details('stampede_async') ->
    ?STRATEGY_DETAILS('true', 'true').

-spec dataplan_reload() -> 'ok'.
dataplan_reload() ->
    _ = kzs_plan:reload(),
    io:format("dataplans reloaded~n").

-spec dataplan_reload(kz_term:ne_binary()) -> 'ok'.
dataplan_reload(AccountId) ->
    _ = kzs_plan:reload(AccountId),
    io:format("dataplan for account ~s reloaded~n", [AccountId]).
