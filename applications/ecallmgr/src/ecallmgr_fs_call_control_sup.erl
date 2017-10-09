%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_fs_call_control_sup).

-behaviour(supervisor).

-include("ecallmgr.hrl").

-export([start_link/2]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom(), kz_proplist()) -> startlink_ret().
start_link(Node, Options) ->
    {'ok', Pid} = supervisor:start_link({'local', ?CALLCTL_SUPERVISOR_NAME(Node)}
                                       ,?MODULE
                                       ,[Node, Options]
                                       ),
    Workers = 5, %ecallmgr_config:get_integer(<<"fs_call_control_workers">>, 10),
    kz_util:spawn(fun() -> timer:sleep(1000),
                           [supervisor:start_child(Pid, [])
                              || _N <- lists:seq(1, Workers)
                           ]
                  end),
    {'ok', Pid}.

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
-spec init(list()) -> sup_init_ret().
init([Node, Props]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, [?WORKER_ARGS_TYPE('ecallmgr_fs_call_control', [Node, Props], 'temporary')]}}.

