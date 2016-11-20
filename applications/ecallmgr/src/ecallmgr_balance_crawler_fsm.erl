%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%% Ecallmgr module (FSM) for disconnect calls when account
%%% balance drops below zero
%%% @end
%%% @contributors
%%%     Dinkor (Sergey Korobkov)
%%%-------------------------------------------------------------------
-module(ecallmgr_balance_crawler_fsm).

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

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(IS_ENABLED, ecallmgr_config:is_true(<<"balance_crawler_enabled">>, 'false')).
-define(CRAWLER_CYCLE_MS, ecallmgr_config:get_integer(<<"balance_crawler_cycle_ms">>, ?MILLISECONDS_IN_MINUTE)).

-type fsm_events() :: 'start_cycle' | 'worker_stop'.
-type fsm_state() :: 'idle' | 'working' | 'worker_timeout'.
-type fsm_reply() :: {'next_state', fsm_state(), api_pid()} |
                     {'next_state', fsm_state(), api_pid(), 'hibernate'}.

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    case ?IS_ENABLED of
        'true' -> gen_fsm:start_link(?SERVER, [], []);
        'false' -> 'ignore'
    end.

%%====================================================================
%% callbacks
%%====================================================================
-spec init(list()) -> {'ok', 'idle', 'undefined'}.
init(_Args) ->
    process_flag('trap_exit', 'true'),
    kz_util:put_callid(?MODULE),
    gen_fsm:send_event_after(?CRAWLER_CYCLE_MS, 'start_cycle'),
    {'ok', 'idle', 'undefined'}.

-spec handle_info(any(), atom(), fsm_state()) -> handle_fsm_ret(fsm_state()).
handle_info({'EXIT', WorkerPid, Reason}, StateName, WorkerPid) ->
    lager:debug("worker: ~p exited with reason ~p", [WorkerPid, Reason]),
    gen_fsm:send_event(self(), 'worker_stop'),
    {'next_state', StateName, WorkerPid};

handle_info(_Info, StateName, State) ->
    lager:debug("unhandled msg in ~s: ~p", [StateName, _Info]),
    {'next_state', StateName, State}.

-spec handle_event(any(), atom(), fsm_state()) -> handle_fsm_ret(fsm_state()).
handle_event(_Event, StateName, State) ->
    lager:debug("unhandled event in ~s: ~p", [StateName, _Event]),
    {'next_state', StateName, State}.

-spec handle_sync_event(any(), {pid(),any()}, atom(), fsm_state()) -> handle_sync_event_ret(fsm_state()).
handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled sync_event in ~s: ~p", [StateName, _Event]),
    {'reply', {'error', 'not_implemented'}, StateName, State}.

-spec terminate(any(), atom(), fsm_state()) -> 'ok'.
terminate(_Reason, _StateName, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

-spec code_change(any(), atom(), fsm_state(), any()) -> {'ok', atom(), fsm_state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

-spec idle(fsm_events(), api_pid()) -> fsm_reply().
idle('start_cycle', 'undefined') ->
    WorkerPid = spawn_worker(?CRAWLER_CYCLE_MS),
    {'next_state', 'working', WorkerPid}.

-spec working(fsm_events(), api_pid()) -> fsm_reply().
working('worker_stop', _OldWorkerPid) ->
    {'next_state', 'idle', 'undefined', 'hibernate'};
working('start_cycle', WorkerPid) ->
    lager:debug("trying start new worker but old worker(~p) still alive, waiting...", [WorkerPid]),
    {'next_state', 'worker_timeout', WorkerPid}.

-spec worker_timeout(fsm_events(), api_pid()) -> fsm_reply().
worker_timeout('worker_stop', _OldWorkerPid) ->
    WorkerPid = spawn_worker(?CRAWLER_CYCLE_MS),
    {'next_state', 'working', WorkerPid}.

%%====================================================================
%% Internal functions
%%====================================================================
spawn_worker(Timeout) when Timeout >= 10 * ?MILLISECONDS_IN_SECOND ->
    gen_fsm:send_event_after(Timeout, 'start_cycle'),
    kz_util:spawn_link(fun ecallmgr_balance_crawler_worker:start/0);
spawn_worker(_) -> spawn_worker(?MILLISECONDS_IN_MINUTE).
