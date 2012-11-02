%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_queue_workers_sup).

-behaviour(supervisor).

-include("acdc.hrl").

%% API
-export([start_link/0
         ,new/1
         ,workers/0
        ]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(Name, Restart, Shutdown, Type),
        {Name, {Name, start_link, []}, Restart, Shutdown, Type, [Name]}).

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
-spec start_link/0 :: () -> startlink_ret().
start_link() ->
    supervisor:start_link(?MODULE, []).

-spec new/1 :: (wh_json:json_object()) -> sup_startchild_ret().
new(JObj) ->
    true = wh_json:is_json_object(JObj),
    supervisor:start_child(?MODULE, [JObj]).

-spec workers/0 :: () -> [pid(),...] | [].
workers() ->
    [ Pid || {_, Pid, worker, [_]} <- supervisor:which_children(?MODULE), is_pid(Pid)].

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
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [?CHILD(acdc_queue_sup, transient, 2000, worker)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
