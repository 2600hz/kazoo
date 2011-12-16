%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Root supervisor tree for stepswitch routing WhApp
%%% @end
%%% Created :  14 June 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------

-module(stepswitch_sup).

-behaviour(supervisor).

-include_lib("whistle/include/wh_types.hrl").

%% API
-export([start_link/0, cache_proc/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Type), {Name, {Name, start_link, []}, permanent, 5000, Type, [Name]}).
-define(CACHE(Name), {Name, {wh_cache, start_link, [Name]}, permanent, 5000, worker, [wh_cache]}).
-define(CHILDREN, [{stepswitch_inbound, worker}, {ss_outbound_listener, worker}]).

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
		Mod =:= ss_cache],
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
-spec init(Args) -> sup_init_ret() when
      Args :: [].
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Children = [?CHILD(Name, Type) || {Name, Type} <- ?CHILDREN],

    {ok, {SupFlags, [?CACHE(ss_cache) | Children]}}.
