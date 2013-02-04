%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_auxiliary_sup).

-behaviour(supervisor).

-include("ecallmgr.hrl").
-include_lib("whistle/include/wh_databases.hrl").

-export([start_link/0]).
-export([cache_proc/0]).
-export([init/1]).

-define(CACHE_AUTHN_PROPS, [{origin_bindings, [[{type, <<"account">>}]
                                               ,[{type, <<"device">>}]
                                              ]}]).
-define(CACHE_UTIL_PROPS, [{origin_bindings, [[{db, ?WH_CONFIG_DB}]]}]).

-define(CHILD(Name, Type), fun(?ECALLMGR_REG_CACHE = N, cache) ->
                                   {N, {wh_cache, start_link, [N, ?CACHE_AUTHN_PROPS]}
                                    ,permanent, 5000, worker, [wh_cache]};
                              (?ECALLMGR_UTIL_CACHE = N, cache) ->
                                   {N, {wh_cache, start_link, [N, ?CACHE_UTIL_PROPS]}
                                    ,permanent, 5000, worker, [wh_cache]};
                              (N, cache) -> {N, {wh_cache, start_link, [N]}
                                             ,permanent, 5000, worker, [wh_cache]};
                              (N, T) -> {N, {N, start_link, []}, permanent, 5000, T, [N]}
                           end(Name, Type)).

-define(CHILDREN, [{?ECALLMGR_UTIL_CACHE, cache}
                   ,{?ECALLMGR_REG_CACHE, cache}
                   ,{?ECALLMGR_CALL_CACHE, cache}
                   ,{ecallmgr_query, worker}
                   ,{ecallmgr_conference_listener, worker}
                   ,{ecallmgr_originate_sup, supervisor}
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

cache_proc() ->
    ?ECALLMGR_UTIL_CACHE.

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
    Children = [?CHILD(Name, Type) || {Name, Type} <- ?CHILDREN],

    {ok, {SupFlags, Children}}.
