%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Manage the event listeners for Crossbar clients
%%% @end
%%% Created :  4 Sep 2011 by James Aimonetti <james@260hz.org>
%%%-------------------------------------------------------------------
-module(cb_events_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, find_srv/2, start_srv/2]).

%% Supervisor callbacks
-export([init/1]).

-include_lib("whistle/include/wh_types.hrl").

-define(SERVER, ?MODULE).
-define(SRV_ID(AccountID, UserID), <<AccountID/binary, "-", UserID/binary>>).
-define(CHILD(AccountID, UserID), { ?SRV_ID(AccountID, UserID)
				   ,{cb_events_srv, start_link, [AccountID, UserID]}, temporary, 5000, worker, [cb_events_srv]}).

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

-spec start_srv/2 :: (AccountID, UserID) -> sup_startchild_ret() when
      AccountID :: binary(),
      UserID :: binary().
start_srv(AccountID, UserID) when is_binary(AccountID), is_binary(UserID) ->
    supervisor:start_child(?MODULE, ?CHILD(AccountID, UserID)).

-spec find_srv/2 :: (AccountID, UserID) -> sup_startchild_ret() when
      AccountID :: binary(),
      UserID :: binary().
find_srv(AccountID, UserID) when is_binary(AccountID), is_binary(UserID) ->
    case [ Pid || {ID, Pid, _, _} <- supervisor:which_children(?MODULE),
		  ?SRV_ID(AccountID, UserID) =:= ID ] of
	[undefined] ->
	    ok = supervisor:delete_child(?MODULE, ?SRV_ID(AccountID, UserID)),
	    start_srv(AccountID, UserID);
	[Pid] -> {ok, Pid};
	_Other -> start_srv(AccountID, UserID)
    end.

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
    RestartStrategy = one_for_one,
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
