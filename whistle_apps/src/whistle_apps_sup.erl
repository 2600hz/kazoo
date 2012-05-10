%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(whistle_apps_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").
-include_lib("whistle_apps/src/whapps_call_command.hrl").
-include_lib("whistle_apps/src/whistle_apps.hrl").

-export([start_link/0]).
-export([initialize_whapps/1]).
-export([init/1]).

-define(SERVER, ?MODULE).

-define(CHILD(Name, Type), fun(N, cache) -> {N, {wh_cache, start_link, [N]}, permanent, 5000, worker, [wh_cache]};
                              (N, T) -> {N, {N, start_link, []}, permanent, 5000, T, [N]} end(Name, Type)).
-define(WHAPPS(Whapps), {whapps_sup, {whapps_sup, start_link, [Whapps]}, permanent, 5000, supervisor, [whapps_sup]}).
-define(CHILDREN, [{wh_alert, worker}
                   ,{wh_cache, worker}
                   ,{?WHAPPS_CONFIG_CACHE, cache}
                   ,{?WHAPPS_CALL_CACHE, cache}
                   ,{whistle_couch_sup, supervisor}
                   ,{whapps_controller, worker}
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
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec initialize_whapps/1 :: ([atom(),...]) -> {'error', term()} | 
                                               {'ok','undefined' | pid()} | 
                                               {'ok','undefined' | pid(), term()}.
initialize_whapps(Whapps) ->
    supervisor:start_child(?SERVER, ?WHAPPS(Whapps)).    

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
    RestartStrategy = one_for_one,
    MaxRestarts = 25,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Children = [?CHILD(Name, Type) || {Name, Type} <- ?CHILDREN],

    {ok, {SupFlags, Children}}.
