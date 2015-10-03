%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%     SIPLABS, LLC (Ilya Ashchepkov)
%%%-------------------------------------------------------------------
-module(ananke_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([start_supervisor/1]).
-export([init/1]).

-include("ananke.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILDREN, [?WORKER('ananke_listener')]).

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

-spec start_supervisor(any()) -> sup_startchild_ret().
start_supervisor(Id) ->
    supervisor:start_child(?MODULE, ?SUPER(Id)).

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
    wh_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    LeaderCronNodes = [node() | get_leader_cron_nodes()],
    lager:debug("starting leader cron on nodes ~p", [LeaderCronNodes]),
    LeaderCron = ?WORKER_ARGS('leader_cron', [LeaderCronNodes]),

    {'ok', {SupFlags, [LeaderCron | ?CHILDREN]}}.

%% This is only for simple configurations.
%% At the first start one node will be added to config.
%% After that all whistle_apps nodes will try to syncronize with it.
-spec get_leader_cron_nodes() -> atoms().
get_leader_cron_nodes() ->
    lists:map(fun(X) -> wh_util:to_atom(wh_util:to_list(X), 'true') end
              ,whapps_config:get(?CONFIG_CAT, <<"nodes">>, [node()])
              ) -- [node()].
