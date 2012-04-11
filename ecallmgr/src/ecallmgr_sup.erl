%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(ecallmgr_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([cache_proc/0]).
-export([registrar_proc/0]).
-export([init/1]).

-include("ecallmgr.hrl").

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(POOL(Mod), {Mod, {poolboy, start_link, [
                                                [{name, {local, Mod}}
                                                 ,{worker_module, wh_amqp_worker}
                                                 ,{size, 50}
                                                 ,{max_overflow, 50}
                                                ]
                                               ]}
                    ,permanent, 5000, worker, [poolboy]
                   }).
-define(CACHE(Name), {Name, {wh_cache, start_link, [Name]}, permanent, 5000, worker, [wh_cache]}).

%% ===================================================================
%% API functions
%% ===================================================================
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec cache_proc/0 :: () -> {'ok', pid()}.
cache_proc() ->
    [P] = find_procs(ecallmgr_cache),
    {ok, P}.

-spec registrar_proc/0 :: () -> {'ok', pid()}.
registrar_proc() ->
    [R] = find_procs(ecallmgr_registrar),
    {ok, R}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok
     ,{
       {one_for_one, 5, 10}
       ,[?CHILD(wh_alert, worker)
         ,?CACHE(ecallmgr_cache) % provides a cache

         ,?POOL(?AMQP_POOL_MGR) % handles the pool of AMQP queues

         ,?CHILD(ecallmgr_call_sup, supervisor) % handles dynamic call {event,control} processes
         ,?CHILD(ecallmgr_shout_sup, supervisor) % handles dynamic record streams from FreeSWITCH to local filesystem

         ,?CHILD(ecallmgr_fs_route_sup, supervisor)
         ,?CHILD(ecallmgr_fs_auth_sup, supervisor)
         ,?CHILD(ecallmgr_fs_config_sup, supervisor)

         ,?CHILD(ecallmgr_fs_sup, supervisor)

         ,?CHILD(ecallmgr_registrar, worker) % local cache for registrations

         ,?CHILD(ecallmgr_media_registry, worker) % handles tracking media names and files per-call
         ,?CHILD(ecallmgr_fs_query, worker) % handles queries for call information/location
         ,?CHILD(ecallmgr_conference_listener, worker)

         ,?CHILD(ecallmgr_fs_handler, worker) % handles starting FreeSWITCH handlers for a given FS node
        ]
      }
    }.

find_procs(Mod) ->
    [P || {M, P, _, _} <- supervisor:which_children(?MODULE), M =:= Mod].
