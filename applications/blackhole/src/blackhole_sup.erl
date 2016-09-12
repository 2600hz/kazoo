%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz Inc
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
-define(SOCKET_PORT, kapps_config:get_integer(?APP_NAME, <<"port">>, 5555)).
-define(SOCKET_ACCEPTORS, kapps_config:get_integer(?APP_NAME, <<"acceptors">>, 100)).
-define(SOCKET_OPTIONS, [{'port', ?SOCKET_PORT}]).
-define(COWBOY_ROUTER, cowboy_router:compile([{'_', [{"/", 'blackhole_socket_handler', []}]}])).
-define(COWBOY_OPTIONS, [{'env', [{'dispatch', ?COWBOY_ROUTER}]}]).

-define(RANCH_SPEC(Ref),
        ranch:child_spec(Ref, ?SOCKET_ACCEPTORS, ranch_tcp, ?SOCKET_OPTIONS, cowboy_protocol, ?COWBOY_OPTIONS)
       ).
-define(CHILDREN, [?WORKER('blackhole_listener')
                  ,?WORKER('blackhole_tracking')
                  ,?RANCH_SPEC('blackhole_http_listener')
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
    kz_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},


    {'ok', {SupFlags, ?CHILDREN}}.
