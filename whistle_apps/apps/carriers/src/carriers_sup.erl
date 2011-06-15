%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2010-2011, VoIP INC
%%% @doc
%%% Root supervisor tree for carriers routing WhApp
%%% @end
%%% Created :  14 June 2011 by Karl Anderson <karl@2600hz.org>

-module(carriers_sup).

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
				  ?CHILD(carrier_inbound, worker)
				 ]} }.
