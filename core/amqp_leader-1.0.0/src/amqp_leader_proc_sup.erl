%%%-------------------------------------------------------------------
%%% @copyright (C) 2015, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(amqp_leader_proc_sup).

-behaviour(supervisor).

%% API functions
-export([start_link/6]).

%% Supervisor callbacks
-export([init/1]).

-include("amqp_leader.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(atom(), atoms(), list(), atom(), [], []) -> startlink_ret().
start_link(Name, Nodes, Opts, Module, [], []) ->
    supervisor:start_link(?MODULE, {Name, Nodes, Opts, Module, [], []}).

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
-spec init({atom(), atoms(), list(), atom(), [], []}) -> sup_init_ret().
init({Name, Nodes, Opts, Module, [], []}) ->
    {'ok', {{'rest_for_one', 5, 10}
            ,[?WORKER_ARGS('amqp_leader_listener', [Name])
              ,?WORKER_ARGS('amqp_leader_proc', [Name, Nodes, Opts, Module, [], []])
             ]
           }}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
