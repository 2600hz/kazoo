%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, VoIP, INC
%%% @doc
%%% Jonny5 module (FSM) for disconnect calls when account
%%% balance drops below zero
%%% @end
%%% @contributors
%%%     Dinkor (Sergey Korobkov)
%%%-------------------------------------------------------------------
-module(j5_balance_crawler_fsm).

-behaviour(gen_fsm).

-export([start_link/0]).

%% gen_fsm callbacks
-export([init/1
         ,handle_event/3
         ,handle_sync_event/4
         ,handle_info/3
         ,terminate/3
         ,code_change/4
        ]).

-export([idle/2
         ,working/2
         ,worker_timeout/2
        ]).

-include("jonny5.hrl").

-define(SERVER, ?MODULE).
-define(IS_ENABLED, whapps_config:get_is_true(?APP_NAME, <<"balance_crawler_enabled">>, 'false')).
-define(CRAWLER_CYCLE_MS, whapps_config:get_integer(?APP_NAME, <<"balance_crawler_cycle_ms">>, ?MILLISECONDS_IN_MINUTE)).

-type fsm_events() :: 'start_cycle' | 'worker_stop'.
-type fsm_state() :: 'idle' | 'working' | 'worker_timeout'.
-type fsm_reply() :: {'next_state', fsm_state(), api_pid()} |
                    {'next_state', fsm_state(), api_pid(), 'hibernate'}.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    case ?IS_ENABLED of
        'true' -> gen_fsm:start_link(?MODULE, [], []);
        'false' -> 'ignore'
    end.

%%====================================================================
%% callbacks
%%====================================================================
init(_Args) ->
    process_flag('trap_exit', 'true'),
    wh_util:put_callid(?MODULE),
    gen_fsm:send_event(self(), 'start_cycle'),
    {'ok', 'idle', 'undefined'}.

handle_info({'EXIT', WorkerPid, Reason}, StateName, WorkerPid) ->
    lager:debug("worker: ~p exited with reason ~p", [WorkerPid, Reason]),
    gen_fsm:send_event(self(), 'worker_stop'),
    {'next_state', StateName, WorkerPid};

handle_info(_Info, StateName, State) ->
    lager:debug("unhandled msg in ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

handle_event(_Event, StateName, State) ->
    lager:debug("unhandled event in ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled sync_event in ~s: ~p", [StateName, _Event]),
    {'reply', {'error', 'not_implemented'}, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

-spec idle(fsm_events(), api_pid()) -> fsm_reply().
idle('start_cycle', 'undefined') ->
    WorkerPid = spawn_worker(),
    {'next_state', 'working', WorkerPid}.

-spec working(fsm_events(), api_pid()) -> fsm_reply().
working('worker_stop', _OldWorkerPid) ->
    {'next_state', 'idle', 'undefined', 'hibernate'};
working('start_cycle', WorkerPid) ->
    lager:debug("trying start new worker but old worker(~p) still alive, waiting...", [WorkerPid]),
    {'next_state', 'worker_timeout', WorkerPid}.

-spec worker_timeout(fsm_events(), api_pid()) -> fsm_reply().
worker_timeout('worker_stop', _OldWorkerPid) ->
    WorkerPid = spawn_worker(),
    {'next_state', 'working', WorkerPid}.

%%====================================================================
%% Internal functions
%%====================================================================
spawn_worker() ->
    gen_fsm:send_event_after(?CRAWLER_CYCLE_MS, 'start_cycle'),
    wh_util:spawn_link(fun j5_balance_crawler_worker:start/0).
