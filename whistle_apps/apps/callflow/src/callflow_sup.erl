%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(callflow_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").

%% API
-export([start_link/0]).
-export([listener_proc/0]).

%% Supervisor callbacks
-export([init/1]).

-include("callflow.hrl").

-define(ORIGIN_BINDINGS, [[{type, <<"account">>}]
                          ,[{type, <<"user">>}]
                          ,[{type, <<"device">>}]
                         ]).
-define(CACHE_PROPS, [{origin_bindings, ?ORIGIN_BINDINGS}]).
-define(CACHE(), {?CALLFLOW_CACHE, {wh_cache, start_link, [?CALLFLOW_CACHE, ?CACHE_PROPS]}
                  ,permanent, 5000, worker, [wh_cache]}).
-define(CHILD(Name, Type), {Name, {Name, start_link, []}, permanent, 5000, Type, [Name]}).
-define(CHILDREN, [?CACHE()
                   ,?CHILD(cf_shared_listener, worker)
                   ,?CHILD(cf_listener, worker)
                   ,?CHILD(cf_exe_sup, supervisor)
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
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec listener_proc() -> {'ok', pid()}.
listener_proc() ->
    [P] = [P || {Mod, P, _, _} <- supervisor:which_children(?MODULE),
                Mod =:= cf_listener],
    {ok, P}.

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
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, ?CHILDREN}}.
