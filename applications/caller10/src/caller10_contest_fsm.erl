%%%-------------------------------------------------------------------
%%% @copyright (C) 2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(caller10_contest_fsm).

-behaviour(gen_fsm).

%% API
-export([start_link/1]).

%% gen_fsm callbacks
-export([init/1

         ,prior/2, prior/3
         ,started/2, started/3
         ,winner/2, winner/3
         ,finished/2, finished/3

         ,handle_event/3
         ,handle_sync_event/4
         ,handle_info/3
         ,terminate/3
         ,code_change/4
        ]).

-include("caller10.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).
-type state() :: #state{}.

-type fsm_state() :: 'prior' |
                     'started' |
                     'winner' |
                     'finished'.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Creates a gen_fsm process which calls Module:init/1 to
%% initialize. To ensure a synchronized start-up procedure, this
%% function does not return until Module:init/1 has returned.
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(wh_json:object()) -> startlink_ret().
start_link(ContestJObj) ->
    gen_fsm:start_link(?MODULE, [ContestJObj], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a gen_fsm is started using gen_fsm:start/[3,4] or
%% gen_fsm:start_link/[3,4], this function is called by the new
%% process to initialize.
%%
%% @spec init(Args) -> {ok, StateName, State} |
%%                     {ok, StateName, State, Timeout} |
%%                     ignore |
%%                     {stop, StopReason}
%% @end
%%--------------------------------------------------------------------
-spec init(wh_json:objects()) -> {'ok', fsm_state(), state()}.
init([ContestJObj]) ->
    {'ok', 'prior', #state{}}.

-spec prior(term(), state()) -> {'next_state', fsm_state(), state()}.
prior(_Event, State) ->
    lager:debug("unhandled prior/2: ~p", [_Event]),
    {'next_state', 'prior', State}.

-spec started(term(), state()) -> {'next_state', fsm_state(), state()}.
started(_Event, State) ->
    lager:debug("unhandled started/2: ~p", [_Event]),
    {'next_state', 'started', State}.

-spec winner(term(), state()) -> {'next_state', fsm_state(), state()}.
winner(_Event, State) ->
    lager:debug("unhandled winner/2: ~p", [_Event]),
    {'next_state', 'winner', State}.

-spec finished(term(), state()) -> {'next_state', fsm_state(), state()}.
finished(_Event, State) ->
    lager:debug("unhandled finished/2: ~p", [_Event]),
    {'next_state', 'finished', State}.

-spec prior(term(), pid_ref(), state()) ->
                   {'reply', term(), fsm_state(), state()}.
prior(_Event, _From, State) ->
    lager:debug("unhandled prior/3: ~p", [_Event]),
    {'reply', {'error', 'not_implemented'}, 'prior', State}.

-spec started(term(), pid_ref(), state()) ->
                   {'reply', term(), fsm_state(), state()}.
started(_Event, _From, State) ->
    lager:debug("unhandled started/3: ~p", [_Event]),
    {'reply', {'error', 'not_implemented'}, 'started', State}.

-spec winner(term(), pid_ref(), state()) ->
                   {'reply', term(), fsm_state(), state()}.
winner(_Event, _From, State) ->
    lager:debug("unhandled winner/3: ~p", [_Event]),
    {'reply', {'error', 'not_implemented'}, 'winner', State}.

-spec finished(term(), pid_ref(), state()) ->
                   {'reply', term(), fsm_state(), state()}.
finished(_Event, _From, State) ->
    lager:debug("unhandled finished/3: ~p", [_Event]),
    {'reply', {'error', 'not_implemented'}, 'finished', State}.

-spec handle_event(term(), fsm_state(), state()) ->
                          {'next_state', fsm_state(), state()}.
handle_event(_Event, StateName, State) ->
    lager:debug("unhandled event/2: ~p", [_Event]),
    {'next_state', StateName, State}.

-spec handle_sync_event(term(), pid_ref(), fsm_state(), state()) ->
                               {'reply', term(), fsm_state(), state()}.
handle_sync_event(_Event, _From, StateName, State) ->
    lager:debug("unhandled event/3: ~p", [_Event]),
    {'reply', {'error', 'not_implemented'}, StateName, State}.

-spec handle_info(term(), fsm_state(), state()) ->
                         {'next_state', fsm_state(), state()}.
handle_info(_Info, StateName, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'next_state', StateName, State}.

-spec terminate(term(), fsm_state(), state()) -> 'ok'.
terminate(_Reason, _StateName, _State) ->
    lager:debug("contest going down while in ~s: ~p", [_StateName, _Reason]).

-spec code_change(term(), fsm_state(), state(), term()) ->
                         {'ok', fsm_state(), state()}.
code_change(_OldVsn, StateName, State, _Extra) ->
    {'ok', StateName, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
