%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010 VoIP INC
%%% @doc
%%%
%%% @end
%%% Created :  15 Mar 2011 13:43:17 GMT: James Aimonetti <james@2600hz.org>
-module(dth_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

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

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Processes = [
		 ?CACHE(dth_cache)
		 ,?CHILD(dth_listener, worker)
		],
    {ok, { {one_for_one, 5, 10}, Processes} }.
