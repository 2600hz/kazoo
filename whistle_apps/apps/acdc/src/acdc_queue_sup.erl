%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_queue_sup).

-behaviour(supervisor).

-include("acdc.hrl").

%% API
-export([start_link/1
         ,start_link/2
         ,stop/1
        ]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Args, Type),
        {Name, {Name, start_link, Args}, one_for_one, 5000, Type, [Name]}).

%%%===================================================================
%%% api functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link/1 :: (wh_json:json_object()) -> startlink_ret().
-spec start_link/2 :: (ne_binary(), ne_binary()) -> startlink_ret().
start_link(QueueJObj) ->
    supervisor:start_link(?MODULE, [QueueJObj]).

start_link(AcctId, QueueId) ->
    supervisor:start_link(?MODULE, [AcctId, QueueId]).

-spec stop/1 :: (pid()) -> 'ok' | {'error', 'not_found'}.
stop(Super) ->
    supervisor:terminate_child(acdc_queues_sup, Super).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
-spec init/1 :: (list()) -> sup_init_ret().
init(Args) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 2,
    MaxSecondsBetweenRestarts = 2,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [
                     ?CHILD(acdc_queue_workers_sup, [], supervisor)
                     ,?CHILD(acdc_queue_manager, [self() | Args], worker)
                    ]
         }
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
