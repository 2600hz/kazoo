
-module(ecallmgr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, cache_proc/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).
-define(CACHE(Name), {Name, {wh_cache, start_link, [Name]}, permanent, 5000, worker, [wh_cache]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec cache_proc/0 :: () -> {'ok', pid()}.
cache_proc() ->
    [P] = [P || {Mod, P, _, _} <- supervisor:which_children(?MODULE),
		Mod =:= ecallmgr_cache],
    {ok, P}.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok
     ,{
       {one_for_one, 5, 10}
       ,[
	 ?CHILD(ecallmgr_fs_sup, supervisor)
	 ,?CHILD(ecallmgr_call_sup, supervisor) % handles dynamic call {event,control} processes
         ,?CHILD(ecallmgr_shout_sup, supervisor) % handles dynamic record streams from FreeSWITCH to local filesystem
	 ,?CHILD(ecallmgr_fs_route_sup, supervisor)
	 ,?CHILD(ecallmgr_fs_auth_sup, supervisor)
	 ,?CHILD(ecallmgr_amqp_pool_worker_sup, supervisor)

         ,?CACHE(ecallmgr_cache) % provides a cache

	 ,?CHILD(resource_mgr, worker) % handles resource requests / resource availability
         ,?CHILD(ecallmgr_registrar, worker) % local cache for registrations
	 ,?CHILD(ecallmgr_amqp_pool, worker) % pool of queues for sending msgs
	 ,?CHILD(ecallmgr_fs_handler, worker) % handles starting FreeSWITCH handlers for a given FS node
	 ,?CHILD(ecallmgr_media_registry, worker) % handles tracking media names and files per-call
	 ,?CHILD(ecallmgr_notify, worker) % handles notify-type API calls (like MWI, BLF, check-sync)
	]
      }
    }.
