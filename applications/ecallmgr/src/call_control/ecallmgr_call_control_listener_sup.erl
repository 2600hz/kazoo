%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_call_control_listener_sup).

-behaviour(supervisor).

-include("ecallmgr.hrl").

-export([start_link/0]).
-export([control_q/1]).
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    {'ok', Pid} = supervisor:start_link({'local', ?MODULE}
                                       ,?MODULE
                                       ,[]
                                       ),
    Workers = kapps_config:get_integer(?APP_NAME, <<"call_control_listeners">>, 5),
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

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(list()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, [?WORKER_ARGS_TYPE('ecallmgr_call_control_listener', [], 'temporary')]}}.

-spec control_q(map()) -> map().
control_q(Map) ->
    Listeners = supervisor:which_children(?MODULE),
    Size = length(Listeners),
    Selected = rand:uniform(Size),
    {_, ControlP, _, _} = lists:nth(Selected, Listeners),
    {'ok', Q, Channel} = ecallmgr_call_control_listener:control_q(ControlP),
    lager:debug("fs call control sup control_q returning  ~p, ~p , ~p", [ControlP, Q, Channel]),
    Map#{control_q => Q
        ,channel => Channel
        ,init_fun => fun(#{channel := C}) ->
                             kz_amqp_channel:consumer_channel(C)
                     end
        ,exit_fun => fun(#{}) -> 'ok' end
        }.
