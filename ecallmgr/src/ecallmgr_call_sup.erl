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
-export([start_link/0, start_control_process/2, start_event_process/3]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% supervisors should never die for good
-define(CHILD(Mod, Type), {Mod, {Mod, start_link, []}, permanent, 5000, Type, [Mod]}).

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

start_event_process(Node, UUID, CtlPid) ->
    ecallmgr_call_event_sup:start_proc([Node, UUID, CtlPid]).

start_control_process(Node, UUID) ->
    ecallmgr_call_control_sup:start_proc([Node, UUID]).

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
    {ok
     ,{
       {one_for_one, 5, 10}
       ,[?CHILD(ecallmgr_call_event_sup, supervisor)
	 ,?CHILD(ecallmgr_call_control_sup, supervisor)
	]
      }
    }.

%%%===================================================================
%%% Internal functions
%%%===================================================================
