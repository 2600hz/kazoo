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
-export([control_q/1]).
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
-spec start_link(atom(), kz_term:proplist()) -> kz_types:startlink_ret().
start_link(Node, Options) ->
    {'ok', Pid} = supervisor:start_link({'local', ?CALLCTL_SUPERVISOR_NAME(Node)}
                                       ,?MODULE
                                       ,[Node, Options]
                                       ),
    Workers = ecallmgr_config:get_integer(<<"fs_call_control_workers">>, 5),
    _ = kz_util:spawn(fun() -> [begin
                                    _ = supervisor:start_child(Pid, []),
                                    timer:sleep(250)
                                end || _N <- lists:seq(1, Workers)
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
-spec init(list()) -> kz_types:sup_init_ret().
init([Node, Props]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, [?WORKER_ARGS_TYPE('ecallmgr_fs_call_control', [Node, Props], 'temporary')]}}.

-spec control_q(map()) -> map().
control_q(#{node := Node}= Map) ->
    SupPid = whereis(?CALLCTL_SUPERVISOR_NAME(Node)),
    Listeners = supervisor:which_children(SupPid),
    Size = length(Listeners),
    Selected = rand:uniform(Size),
    {_, ControlP, _, _} = lists:nth(Selected, Listeners),
    {'ok', Q, Channel} = ecallmgr_fs_call_control:control_q(ControlP),
    lager:debug("fs call control sup control_q returning  ~p, ~p , ~p", [ControlP, Q, Channel]),
    Map#{control_q => Q, channel => Channel}.
