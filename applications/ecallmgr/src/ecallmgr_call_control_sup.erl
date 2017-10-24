%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%% Simple-One-For-One strategy for restarting call event processes
%%% @end
%%%
%%% @contributors
%%% James Aimonetti <james@2600hz.org>
%%% Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(ecallmgr_call_control_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([start_proc/1]).
-export([start_control_process/6]).
-export([init/1]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?WORKER_TYPE('ecallmgr_call_control', 'temporary')]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec start_proc(map()) -> sup_startchild_ret().
start_proc(Map) ->
    supervisor:start_child(?SERVER, [control_q(Map)]).

-spec start_control_process(atom(), ne_binary(), ne_binary(), api_ne_binary(), api_pid(), kz_json:object()) ->
                                   sup_startchild_ret().
start_control_process(Node, CallId, FetchId, ControllerQ, ControllerP, CCVs) ->
    lager:debug("starting call control for ~s", [CallId]),
    start_proc(#{node => Node
                ,call_id => CallId
                ,fetch_id => FetchId
                ,controller_q => ControllerQ
                ,controller_p => ControllerP
                ,initial_ccvs => CCVs
                }).

control_q(#{control_q := _Queue}= Map) -> Map;
control_q(#{control_q_callback := Fun}= Map) ->
    Fun(Map);
control_q(Map) ->
    ecallmgr_fs_call_control_sup:control_q(Map).

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
%% @end
%%--------------------------------------------------------------------
-spec init(any()) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
