%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Simple ETS-backed cache. Handles ETS operations
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_cache_processes).
-behaviour(gen_server).

%%------------------------------------------------------------------------------
%% @doc API functions
%% @end
%%------------------------------------------------------------------------------
-export([start_link/1
        ,monitor_processes/3
        ,unmonitor_key/2
        ,unmonitor_all/1
        ]).

-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,terminate/2
        ,code_change/3
        ]).

-include("kz_caches.hrl").

%% {CacheName, [{CacheKey, Pid, MonitorReference}]}
-type state() :: {atom(), [{any(), pid(), reference()}]}.

-spec start_link(atom()) -> kz_types:startlink_ret().
start_link(Name) ->
    gen_server:start_link({'local', processes_name(Name)}, ?MODULE, [Name], []).

%% @doc setup a monitor on a PID to remove Key if PID dies
-spec monitor_processes(atom(), any(), [pid()]) -> 'ok'.
monitor_processes(Name, Key, Pids) ->
    gen_server:cast(processes_name(Name), {'monitor', Key, Pids}).

%% @doc remove any monitors for PID(s) tracking Key
-spec unmonitor_key(atom(), any()) -> 'ok'.
unmonitor_key(Name, Key) ->
    gen_server:cast(processes_name(Name), {'unmonitor_key', Key}).

%% @doc remove all monitors
-spec unmonitor_all(atom()) -> 'ok'.
unmonitor_all(Name) ->
    gen_server:cast(processes_name(Name), 'unmonitor_all').

-spec processes_name(atom()) -> atom().
processes_name(Name) ->
    kz_term:to_atom(atom_to_list(Name) ++ "_processes", 'true').

-spec init(kz_terms:atoms()) -> {'ok', state()}.
init([Name]) ->
    kz_log:put_callid(processes_name(Name)),
    {'ok', {Name, []}}.

-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, Monitors) ->
    {'noreply', Monitors}.

-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast({'monitor', Key, Pids}, {Name, Monitors}) ->
    {'noreply', {Name, add_monitors(Key, Pids, Monitors)}};
handle_cast({'unmonitor_key', Key}, {Name, Monitors}) ->
    {'noreply', {Name, remove_key(Key, Monitors)}};
handle_cast('unmonitor_all', {Name, []}) ->
    {'noreply', {Name, []}};
handle_cast('unmonitor_all', {Name, Monitors}) ->
    _ = [demonitor(Ref, ['flush']) || {_Key, _Pid, Ref} <- Monitors],
    {'noreply', {Name, []}};
handle_cast(_Msg, Monitors) ->
    {'noreply', Monitors}.

-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'DOWN', Ref, 'process', Pid, _Reason}, {Name, Monitors}) ->
    {'noreply', {Name, unmonitor_pid(Name, Pid, Ref, Monitors)}};
handle_info(_Info, Name) ->
    ?LOG_INFO("unhandled msg: ~p", [_Info]),
    {'noreply', Name}.

-spec terminate(any(), state()) -> any().
terminate(_Reason, Name) ->
    ?LOG_DEBUG("terminating ~p", [Name]).

-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

remove_key(_Key, []) -> [];
remove_key(Key, Monitors) ->
    remove_key(Key, Monitors, lists:keytake(Key, 1, Monitors)).

remove_key(_Key, Monitors, 'false') -> Monitors;
remove_key(Key, _Monitors, {'value', {Key, _Pid, Ref}, Monitors}) ->
    demonitor(Ref, ['flush']),
    remove_key(Key, Monitors).

add_monitors(Key, Pids, Monitors) ->
    {Key, NewMonitors} = lists:foldl(fun add_monitor/2, {Key, Monitors}, Pids),
    NewMonitors.

add_monitor(NotPid, {Key, Monitors}) when not is_pid(NotPid) ->
    {Key, Monitors};
add_monitor(Pid, {Key, Monitors}) ->
    {Key, [{Key, Pid, monitor('process', Pid)} | Monitors]}.

unmonitor_pid(Name, Pid, Ref, Monitors) ->
    {Name, Pid, Ref, NewMonitors} = lists:foldl(fun unmonitor_pid_fold/2, {Name, Pid, Ref, []}, Monitors),
    NewMonitors.

unmonitor_pid_fold({Key, Pid, Ref}, {Name, Pid, Ref, Monitors}) ->
    ?LOG_DEBUG("pid ~p down, removing key ~p", [Pid, Key]),
    %% TODO: what if multiple PIDs are monitoring the Key?
    kz_cache:erase_local(Name, Key),
    {Name, Pid, Ref, Monitors};
unmonitor_pid_fold(Monitor, {Name, Pid, Ref, Monitors}) ->
    {Name, Pid, Ref, [Monitor | Monitors]}.
