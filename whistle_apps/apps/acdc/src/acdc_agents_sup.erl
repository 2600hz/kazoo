%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(acdc_agents_sup).

-behaviour(supervisor).

-include("acdc.hrl").

%% API
-export([start_link/0
         ,new/1
         ,workers/0
         ,find_acct_supervisors/1
         ,find_agent_supervisor/2
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
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec new/1 :: (wh_json:json_object()) -> sup_startchild_ret().
new(JObj) ->
    supervisor:start_child(?MODULE, [JObj]).

-spec workers/0 :: () -> [pid(),...] | [].
workers() ->
    [ Pid || {_, Pid, worker, [_]} <- supervisor:which_children(?MODULE)].

-spec find_acct_supervisors/1 :: (ne_binary()) -> [pid(),...] | [].
find_acct_supervisors(AcctId) ->
    [Super || Super <- workers(), is_agent_in_acct(Super, AcctId)].

-spec is_agent_in_acct/2 :: (pid(), ne_binary()) -> boolean().
is_agent_in_acct(Super, AcctId) ->
    case catch acdc_agent:config(acdc_agent_sup:agent(Super)) of
        {'EXIT', _} -> false;
        {AcctId, _} -> true;
        _ -> false
    end.

-spec find_agent_supervisor/2 :: (api_binary(), api_binary()) -> pid() | 'undefined'.
-spec find_agent_supervisor/3 :: (api_binary(), api_binary(), [pid(),...] | []) -> pid() | 'undefined'.
find_agent_supervisor(AcctId, AgentId) ->
    find_agent_supervisor(AcctId, AgentId, workers()).

find_agent_supervisor(_AcctId, _AgentId, []) -> undefined;
find_agent_supervisor(AcctId, AgentId, _) when AcctId =:= undefined orelse
                                               AgentId =:= undefined ->
    undefined;
find_agent_supervisor(AcctId, AgentId, [Super|Rest]) ->
    case catch acdc_agent:config(acdc_agent_sup:agent(Super)) of
        {'EXIT', _} -> find_agent_supervisor(AcctId, AgentId, Rest);
        {AcctId, AgentId} -> Super;
        _ -> find_agent_supervisor(AcctId, AgentId, Rest)
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
-spec init/1 :: (list()) -> sup_init_ret().
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [?CHILD(acdc_agent_sup, temporary, 2000, worker)]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
