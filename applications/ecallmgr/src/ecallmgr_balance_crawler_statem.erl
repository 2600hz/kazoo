%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc Ecallmgr module (`statem') for disconnecting calls when account
%%% balance drops below zero.
%%%
%%% @author Dinkor (Sergey Korobkov)
%%% @author Daniel Finke
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_balance_crawler_statem).

-behaviour(gen_statem).

%% API
-export([start_link/0]).

%% gen_statem callbacks
-export([init/1
        ,callback_mode/0
        ,terminate/3
        ,code_change/4
        ]).

-export([idle/3
        ,working/3
        ,worker_timeout/3
        ]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(IS_ENABLED, kapps_config:is_true(?APP_NAME, <<"balance_crawler_enabled">>, 'false')).
-define(CRAWLER_CYCLE_MS, kapps_config:get_integer(?APP_NAME, <<"balance_crawler_cycle_ms">>, ?MILLISECONDS_IN_MINUTE)).

-type statem_events() :: 'start_cycle' | 'worker_stop'.
-type statem_state() :: 'idle' | 'working' | 'worker_timeout'.
-type statem_reply() :: {'next_state', statem_state(), kz_term:api_pid()} |
                        {'next_state', statem_state(), kz_term:api_pid(), 'hibernate'}.

%%==============================================================================
%% API
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    case ?IS_ENABLED of
        'true' -> gen_statem:start_link(?SERVER, [], []);
        'false' -> 'ignore'
    end.

%%==============================================================================
%% callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> {'ok', 'idle', 'undefined'}.
init(_Args) ->
    process_flag('trap_exit', 'true'),
    kz_log:put_callid(?MODULE),
    _ = timer:apply_after(?CRAWLER_CYCLE_MS, 'gen_statem', 'cast', [self(), 'start_cycle']),
    {'ok', 'idle', 'undefined'}.

-spec callback_mode() -> 'state_functions'.
callback_mode() ->
    'state_functions'.

-spec handle_info(any(), atom(), statem_state()) -> kz_types:handle_fsm_ret(statem_state()).
handle_info({'EXIT', WorkerPid, Reason}, StateName, WorkerPid) ->
    lager:debug("worker: ~p exited with reason ~p", [WorkerPid, Reason]),
    gen_statem:cast(self(), 'worker_stop'),
    {'next_state', StateName, WorkerPid};
handle_info(_Info, StateName, State) ->
    lager:debug("unhandled msg in ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

-spec terminate(any(), atom(), statem_state()) -> 'ok'.
terminate(_Reason, _StateName, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

-spec code_change(any(), atom(), statem_state(), any()) -> {'ok', atom(), statem_state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

-spec idle(gen_statem:event_type(), statem_events(), kz_term:api_pid()) -> statem_reply().
idle('cast', 'start_cycle', 'undefined') ->
    WorkerPid = spawn_worker(?CRAWLER_CYCLE_MS),
    {'next_state', 'working', WorkerPid};
idle('info', Evt, State) ->
    handle_info(Evt, ?FUNCTION_NAME, State).

-spec working(gen_statem:event_type(), statem_events(), kz_term:api_pid()) -> statem_reply().
working('cast', 'worker_stop', _OldWorkerPid) ->
    {'next_state', 'idle', 'undefined', 'hibernate'};
working('cast', 'start_cycle', WorkerPid) ->
    lager:debug("trying start new worker but old worker(~p) still alive, waiting...", [WorkerPid]),
    {'next_state', 'worker_timeout', WorkerPid};
working('info', Evt, State) ->
    handle_info(Evt, ?FUNCTION_NAME, State).

-spec worker_timeout(gen_statem:event_type(), statem_events(), kz_term:api_pid()) -> statem_reply().
worker_timeout('cast', 'worker_stop', _OldWorkerPid) ->
    WorkerPid = spawn_worker(?CRAWLER_CYCLE_MS),
    {'next_state', 'working', WorkerPid};
worker_timeout('info', Evt, State) ->
    handle_info(Evt, ?FUNCTION_NAME, State).

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
spawn_worker(Timeout) when Timeout >= 10 * ?MILLISECONDS_IN_SECOND ->
    _ = timer:apply_after(Timeout, 'gen_statem', 'cast', [self(), 'start_cycle']),
    kz_process:spawn_link(fun ecallmgr_balance_crawler_worker:start/0);
spawn_worker(_) -> spawn_worker(?MILLISECONDS_IN_MINUTE).
