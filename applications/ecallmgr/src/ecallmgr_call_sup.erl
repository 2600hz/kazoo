%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2019, 2600Hz
%%% @doc
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(ecallmgr_call_sup).
-behaviour(supervisor).

-include("ecallmgr.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([start_control_process/3
        ,start_control_process/6
        ]).
-export([start_event_process/2]).
-export([init/1]).

-define(CHILDREN, [?SUPER('ecallmgr_call_event_sup')
                  ,?SUPER('ecallmgr_call_control_sup')
                  ]).

%%==============================================================================
%% API functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

-spec start_event_process(atom(), kz_term:ne_binary()) -> kz_types:sup_startchild_ret().
start_event_process(Node, UUID) ->
    ecallmgr_call_event_sup:start_proc([Node, UUID]).

-spec start_control_process(atom(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_types:sup_startchild_ret().
start_control_process(Node, CallId, FetchId) ->
    start_control_process(Node, CallId, FetchId, 'undefined', kz_json:new(), []).

-spec start_control_process(atom(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:api_ne_binary(), kz_json:object(), kz_term:proplist()) ->
                                   kz_types:sup_startchild_ret().
start_control_process(Node, CallId, FetchId, ControllerQ, CCVs, Options) ->
    ecallmgr_call_control_sup:start_proc([Node
                                         ,CallId
                                         ,FetchId
                                         ,ControllerQ
                                         ,CCVs
                                         ,Options
                                         ]).

%%==============================================================================
%% Supervisor callbacks
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init(any()) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.
