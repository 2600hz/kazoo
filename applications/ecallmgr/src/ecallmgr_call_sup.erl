%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(ecallmgr_call_sup).

-behaviour(supervisor).

-include("ecallmgr.hrl").

-export([start_link/0]).
-export([start_control_process/3
         ,start_control_process/5
        ]).
-export([start_event_process/2]).
-export([init/1]).

-define(CHILDREN, [?SUPER('ecallmgr_call_event_sup')
                   ,?SUPER('ecallmgr_call_control_sup')
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

start_event_process(Node, UUID) ->
    ecallmgr_call_event_sup:start_proc([Node, UUID]).

start_control_process(Node, CallId, FetchId) ->
    start_control_process(Node, CallId, FetchId, 'undefined', wh_json:new()).

start_control_process(Node, CallId, FetchId, ControllerQ, CCVs) ->
    ecallmgr_call_control_sup:start_proc([Node
                                          ,CallId
                                          ,FetchId
                                          ,ControllerQ
                                          ,CCVs
                                         ]).

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
-spec init(list()) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
