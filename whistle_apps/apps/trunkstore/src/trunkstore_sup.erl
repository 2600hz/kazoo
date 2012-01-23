%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 22 Jan 2012 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(trunkstore_sup).

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
-spec start_link/0 :: () -> {'ok', pid()} | 'ignore' | {'error', term()}.
start_link() ->
    trunkstore:start_deps(),
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}
           , [
              ?CHILD(ts_onnet_sup, supervisor) %% handles calls originating on-net (customer)
              ,?CHILD(ts_offnet_sup, supervisor) %% handles calls originating off-net (carrier)
              ,?CHILD(ts_responder, worker)
             ]} }.
