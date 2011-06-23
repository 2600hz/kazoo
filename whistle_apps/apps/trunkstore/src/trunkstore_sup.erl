
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
-spec(start_link/0 :: () -> tuple(ok, pid()) | ignore | tuple(error, term())).
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}
	   , [
	      ?CHILD(ts_call_sup, supervisor)
	      ,?CHILD(ts_responder_sup, supervisor)
	      ,?CHILD(ts_acctmgr, worker)
	      ,?CHILD(ts_carrier, worker)
	      ,?CHILD(ts_credit, worker)
              ,?CHILD(ts_cdr, worker)
	      ,?CHILD(ts_onnet_sup, supervisor)
	      ,?CHILD(ts_offnet_sup, supervisor)
	     ]} }.

