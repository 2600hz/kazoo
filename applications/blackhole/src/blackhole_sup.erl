%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Karl Anderson
%%% James Aimonetti
%%% Peter Defebvre
%%% Ben Wann
%%%-------------------------------------------------------------------
-module(blackhole_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-include("blackhole.hrl").

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?WORKER('blackhole_listener')
                   ,?WORKER('blackhole_tracking')
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    Dispatch = cowboy_router:compile([
                                      {'_', [{"/socket.io/1/[...]"
                                             ,'socketio_handler'
                                             ,[socketio_session:configure([{'heartbeat', 5000}
                                                                          ,{'heartbeat_timeout', 30000}
                                                                          ,{'session_timeout', 30000}
                                                                          ,{'callback', 'blackhole_socket_callback'}
                                                                          ,{'protocol', 'socketio_data_protocol'}
                                                                          ])]}
                                            ,{"/", 'blackhole_default_handler', []}
                                            ]
                                      }
                                     ]),
    Port = whapps_config:get_integer(?APP_NAME, <<"port">>, 5555),
    {'ok', _} = cowboy:start_http('socketio_http_listener', 100, [{'port', Port}],
                                  [{'env', [{'dispatch', Dispatch}]}]),
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

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
    wh_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
