%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created :  27 June 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(conference_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").

%% API
-export([start_link/0]).
-export([cache_proc/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Type), {Name, {Name, start_link, []}, permanent, 5000, Type, [Name]}).
-define(CACHE(Name), {Name, {wh_cache, start_link, [Name]}, permanent, 5000, worker, [wh_cache]}).

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

-spec cache_proc/0 :: () -> {'ok', pid()}.
cache_proc() ->
    [P] = [P || {Mod, P, _, _} <- supervisor:which_children(?MODULE),
                Mod =:= conference_cache],
    {ok, P}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/N,
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Children = [?CACHE(conference_cache)
                ,?CHILD(conf_discovery, worker)
                ,?CHILD(conf_participant_sup, supervisor)
               ],

    {ok, {SupFlags, Children}}.
