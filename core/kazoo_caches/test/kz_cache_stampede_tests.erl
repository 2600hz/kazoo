%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_cache_stampede_tests).

-export([stampede_worker/2]).

-include_lib("eunit/include/eunit.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-define(WORKERS, 5000).

%% all in milliseconds
-define(BASE_TIMEOUT_MS, 50).
-define(STAMPEDE_TIMEOUT, 1500).
-define(DB_WRITE_TIME, 200).

-define(SETUP_LOCAL(F), {'setup', fun init/0, fun cleanup/1, F}).
-define(SETUP_LOAD(F), {'setup', fun init_load/0, fun cleanup/1, F}).
-define(SETUP_FAIL(F), {'setup', fun init_failed/0, fun cleanup/1, F}).
-define(SETUP_CRASH(F), {'setup', fun init_crashed/0, fun cleanup/1, F}).

wait_for_stampede_local_test_() ->
    {"simple stampede test", ?SETUP_LOCAL(fun wait_for_stampede_local/1)}.

stampede_load_test_() ->
    {"basic load test of stampede", ?SETUP_LOAD(fun stampede_load/1)}.

stampede_failed_test_() ->
    {"fail to load key after lock", ?SETUP_FAIL(fun check_failed/1)}.

stampede_crashed_test_() ->
    {"crash when loading key after lock", ?SETUP_CRASH(fun stampede_load/1)}.

init() ->
    {'ok', CachePid} = kz_cache_sup:start_link(?MODULE, ?DB_WRITE_TIME),
    CachePid.

%%------------------------------------------------------------------------------
%% @doc Generates workers to try access the cached key and write it if missing
%% Number of workers that can successfully finish appears to be a factor of
%% (worker-count, stampede-timeout).
%% The more workers starting up around the same time, the more that see the
%% missing key and try to lock the key using kz_cache:mitigate_stampede_local/2
%% There should obviously only be one worker who "locks" the key.
%% The rest of the workers should insert monitor objects into the monitor table
%% to be notified when the key is written to with a "real" value.
%%
%% Runs with 'make eunit':
%%
%% Document write time | Workers | WaitForStampede | Success/Fail
%%
%% 100ms | 3000 |  500 | 3000/0 x 3 1785/1215
%% 100ms | 5000 |  500 | 4165/835 4922/78 2051/2949 5000/0
%%
%% 200ms | 3000 |  500 | 2216/784 2717/283 2492/508 1793/1207
%% 200ms | 3000 | 1000 | 3000/0 x 4
%% 200ms | 5000 | 1000 | 4117/883 4092/908 5000/0 x 3
%% 200ms | 5000 | 1500 | 5000/0 x 4
%%
%% Adding 'read_concurrency' flag to main ETS table:
%% 200ms | 5000 |  500 | 3232/1768 1519/3481 2341/2659
%%
%% No appreciable difference
%%
%% Interestingly, if you remove the timer:sleep() from the beginning of the
%% `stampede_worker` function, DB_WRITE_TIME can be quite large and all workers
%% succeed. Introducing that variability sends the world into chaos.
%%
%% Given the above table, and since most DB fetches are ~100-150ms, setting the
%% stampede timeout in kzs_cache to 1500ms seems conservative and should reduce
%% the pressure on the DBs significantly in the case of a stampede.
%%
%% @end
%%------------------------------------------------------------------------------

init_load() ->
    CachePid = init(),

    Key = kz_binary:rand_hex(5),
    Value = kz_binary:rand_hex(6),

    Pids = [spawn_monitor(?MODULE, 'stampede_worker', [Key, Value])
            || _N <- lists:seq(1, ?WORKERS)
           ],
    Waited = wait_for_workers('false', Pids, {0, 0}),

    {CachePid, Waited, ?WORKERS}.

init_failed() ->
    CachePid = init(),
    Key = kz_binary:rand_hex(5),
    Value = kz_binary:rand_hex(6),

    W1 = spawn_monitor(fun() ->
                               'ok' = kz_cache:mitigate_stampede_local(?MODULE, Key),
                               timer:sleep(50), % db operation fails
                               kz_cache:erase_local(?MODULE, Key)
                       end),
    W2 = spawn_monitor(fun() ->
                               timer:sleep(1),
                               'error' = kz_cache:mitigate_stampede_local(?MODULE, Key),
                               {'error', 'not_found'} = kz_cache:wait_for_stampede_local(?MODULE, Key, 2000),
                               kz_cache:store_local(?MODULE, Key, Value)
                       end),
    wait_for_workers('true', [W1, W2], {0,0}),
    {CachePid, Key, Value}.

check_failed({_CachePid, Key, Value}) ->
    ?_assertEqual({'ok', Value}, kz_cache:peek_local(?MODULE, Key)).

init_crashed() ->
    CachePid = init(),
    Key = kz_binary:rand_hex(5),

    _W1 = spawn_monitor(fun() ->
                                'ok' = kz_cache:mitigate_stampede_local(?MODULE, Key),
                                timer:sleep(50), % db operation fails
                                exit(crash)
                        end),
    W2 = spawn_monitor(fun() ->
                               timer:sleep(1),
                               'error' = kz_cache:mitigate_stampede_local(?MODULE, Key),
                               {'error', 'not_found'} = kz_cache:wait_for_stampede_local(?MODULE, Key, 2000)
                       end),
    Waited = wait_for_workers('true', [W2], {0, 0}),

    {CachePid, Waited, 1}.

cleanup({CachePid, _, _}) ->
    cleanup(CachePid);
cleanup(CachePid) ->
    process_flag('trap_exit', 'true'),
    exit(CachePid, 'shutdown'),
    wait_for_stop(CachePid).

wait_for_stop(CachePid) ->
    receive
        {'EXIT', CachePid, 'shutdown'} -> 'ok';
        _Msg -> wait_for_stop(CachePid)
    end.

wait_for_stampede_local(_) ->
    Timeout = rand:uniform(?BASE_TIMEOUT_MS)+?BASE_TIMEOUT_MS,
    Key = kz_binary:rand_hex(5),
    Value = kz_binary:rand_hex(5),

    _WriterPid = spawn_monitor(fun() -> writer_job_stampede(Key, Value, Timeout) end),
    timer:sleep(10), % allows the writer to lock the key
    ?_assertEqual({'ok', Value}, kz_cache:wait_for_stampede_local(?MODULE, Key, Timeout)).

writer_job_stampede(Key, Value, Timeout) ->
    kz_cache:mitigate_stampede_local(?MODULE, Key),
    timer:sleep(Timeout div 2),
    kz_cache:store_local(?MODULE, Key, Value).

stampede_load({_CachePid, Waited, Workers}) ->
    ?_assertEqual({Workers, 0}, Waited).

stampede_worker(Key, Value) ->
    timer:sleep(rand:uniform(10)),

    MitigationKey = kz_cache:mitigation_key(),
    {'ok', Value} =
        case kz_cache:fetch_local(?MODULE, Key) of
            {MitigationKey, _Pid} ->
                kz_cache:wait_for_stampede_local(?MODULE, Key, ?STAMPEDE_TIMEOUT);
            {'ok', Value}=Ok ->
                Ok;
            {'error', 'not_found'} ->
                mitigate_stampede(Key, Value)
        end.

mitigate_stampede(Key, Value) ->
    case kz_cache:mitigate_stampede_local(?MODULE, Key, [{'expires', ?STAMPEDE_TIMEOUT}]) of
        'ok' ->
            write_to_cache(Key, Value);
        'error' ->
            kz_cache:wait_for_stampede_local(?MODULE, Key, ?STAMPEDE_TIMEOUT)
    end.

write_to_cache(Key, Value) ->
    timer:sleep(?DB_WRITE_TIME), % simulate a db operation or other long-computation
    kz_cache:store_local(?MODULE, Key, Value),
    {'ok', Value}.

wait_for_workers(_ShouldLog, [], Results) -> Results;
wait_for_workers(ShouldLog, [{Pid, Ref} | Pids], {Success, Fail}) ->
    receive
        {'DOWN', Ref, 'process', Pid, 'normal'} ->
            wait_for_workers(ShouldLog, Pids, {Success+1, Fail});
        {'DOWN', Ref, 'process', Pid, _Reason} ->
            ShouldLog
                andalso ?LOG_INFO("pid ~p died: ~p", [Pid, _Reason]),
            wait_for_workers(ShouldLog, Pids, {Success, Fail+1})
    after ?STAMPEDE_TIMEOUT ->
            wait_for_workers(ShouldLog, Pids, {Success, Fail+1})
    end.
