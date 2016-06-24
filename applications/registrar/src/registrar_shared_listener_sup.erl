%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(registrar_shared_listener_sup).

-behaviour(supervisor).

-include_lib("registrar/src/reg.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0]).
-export([start_listeners/1, stop_listeners/1, set_listeners/1]).
-export([workers/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILDREN, [?WORKER('registrar_shared_listener')]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    R = supervisor:start_link({'local', ?SERVER}, ?MODULE, []),
    case R of
        {'ok', _} -> start_listeners();
        _Other -> lager:error("error starting registrar_listeners sup : ~p", [_Other])
    end,
    R.

-spec start_listeners() -> 'ok'.
start_listeners() ->
    start_listeners(kapps_config:get_integer(?CONFIG_CAT, <<"listeners">>, 1)).

-spec start_listeners(integer()) -> 'ok'.
start_listeners(Count) ->
    lager:debug("starting ~B registrar listeners", [Count]),
    _ = [supervisor:start_child(?SERVER, []) || _ <- lists:seq(1, Count)],
    'ok'.

-spec stop_listeners(integer()) -> 'ok'.
stop_listeners(Count) ->
    StopList = lists:sublist(workers(), Count),
    lager:debug("stopping ~B registrar listeners", [length(StopList)]),
    _ = [supervisor:terminate_child(?SERVER, Pid) || Pid <- StopList],
    'ok'.

-spec set_listeners(integer()) -> 'ok'.
set_listeners(Count) ->
    case length(workers()) of
        I when I < Count -> start_listeners(Count - I);
        I when I > Count -> stop_listeners(I - Count);
        _Else -> 'ok'
    end.

-spec workers() -> pids().
workers() ->
    [Pid || {_, Pid, 'worker', [_]} <- supervisor:which_children(?SERVER)].

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init(any()) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
