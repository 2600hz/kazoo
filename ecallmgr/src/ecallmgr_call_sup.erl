%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2010, James Aimonetti
%%% @doc
%%% Manage call-specific processes (events and control)
%%% @end
%%% Created : 24 Nov 2010 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_call_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, add_call_process/3, add_call_process/4]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% only restart if they die abnormally - transient
-define(CHILD(I, Type, Args), {I, {I, start, Args}, transient, 5000, Type, [I]}).

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
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

add_call_process(Node, UUID, Amqp, CtlPid) ->
    supervisor:start_child(?MODULE, ?CHILD(ecallmgr_call_events, worker, [Node, UUID, Amqp, CtlPid])).

add_call_process(Node, UUID, Amqp) ->
    supervisor:start_child(?MODULE, ?CHILD(ecallmgr_call_control, worker, [Node, UUID, Amqp])).

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
    {ok, {one_for_one, 5, 10}, []}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
