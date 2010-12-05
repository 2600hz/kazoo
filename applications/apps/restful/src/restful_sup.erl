%%% @author Karl Anderson <james@2600hz.org>
%%% @copyright (C) 2010 Karl Anderson
%%% @doc
%%% 
%%% @end
%%% Created :  Sun, 05 Dec 2010 07:58:36 GMT: Karl Anderson <james@2600hz.org>
-module(restful_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

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
				  %% Put list of ?CHILD(restful_server, worker) or ?CHILD(restful_other_sup, supervisor)
				 ]} }.

