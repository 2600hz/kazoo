%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(callflow_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([listener_proc/0]).

%% Supervisor callbacks
-export([init/1]).

-include("callflow.hrl").

-define(ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                          ,[{'type', <<"user">>}]
                          ,[{'type', <<"device">>}]
                          ,[{'type', <<"parked_calls">>}]
                         ]).
-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}
                      ,'new_node_flush'
                      ,'channel_reconnect_flush'
                     ]).

-define(CHILDREN, [?CACHE_ARGS(?CALLFLOW_CACHE, ?CACHE_PROPS)
                   ,?WORKER('cf_shared_listener')
                   ,?WORKER('cf_listener')
                   ,?SUPER('cf_event_handler_sup')
                   ,?SUPER('cf_exe_sup')
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

-spec listener_proc() -> {'ok', pid()}.
listener_proc() ->
    [P] = [P || {Mod, P, _, _} <- supervisor:which_children(?MODULE),
                Mod =:= 'cf_listener'
          ],
    {'ok', P}.

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
-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.
