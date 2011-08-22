%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011 James Aimonetti
%%% @doc
%%% 
%%% @end
%%% Created :  Thu, 13 Jan 2011 22:12:40 GMT: James Aimonetti <james@2600hz.org>
-module(registrar_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(SERVER, ?MODULE).
-define(CHILD(I, Type), {I, {I, start_link, []}, transient, 5000, Type, [I]}).
-define(CACHE(Name), {Name, {wh_cache, start_link, [Name]}, permanent, 5000, worker, [wh_cache]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [
				  ?CACHE(reg_cache)
				  ,?CHILD(registrar_listener, worker)
				  %% Put list of ?CHILD(registration_server, worker) or ?CHILD(registration_other_sup, supervisor)
				 ]} }.

