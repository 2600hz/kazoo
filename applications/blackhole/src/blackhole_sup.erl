%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2013, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(blackhole_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("blackhole.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?CACHE('blackhole_cache')
                   ,?WORKER('blackhole_events_sup')
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
	Dispatch = cowboy_router:compile([
		{'_', [{"/socket.io/1/[...]"
				,'socketio_handler'
				,[socketio_session:configure([{'heartbeat', 5000}
											  ,{'heartbeat_timeout', 30000}
											  ,{'session_timeout', 30000}
											  ,{'callback', 'blackhole_socket'}
											  ,{'protocol', 'socketio_data_protocol'}
											 ])]}
			  ]
        }
    ]),
	{ok, _} = cowboy:start_http('socketio_http_listener', 100, [{'port', 5555}],
									[{'env', [{'dispatch', Dispatch}]}]),
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

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
