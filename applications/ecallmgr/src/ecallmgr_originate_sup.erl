%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz
%%% @doc
%%% Simple-One-For-One strategy for restarting call event processes
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(ecallmgr_originate_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([start_originate_proc/2]).
-export([init/1]).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-define(CHILDREN, [?WORKER_TYPE('ecallmgr_originate', 'transient')]).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec start_originate_proc(atom(), kz_json:object()) -> sup_startchild_ret().
start_originate_proc(Node, JObj) ->
    supervisor:start_child(?SERVER, [Node, JObj]).

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
