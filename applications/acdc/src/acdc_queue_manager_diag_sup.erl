%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2021-, Ooma Inc.
%%% @doc Supervises queue manager diagnostics receivers.
%%% @author Daniel Finke
%%% @end
%%%-----------------------------------------------------------------------------
-module(acdc_queue_manager_diag_sup).
-behaviour(supervisor).

-include("acdc.hrl").

-define(SERVER, ?MODULE).

%% API
-export([start_link/0
        ,start_diagnostics/2
        ,send_diagnostics/2
        ]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILDREN, [?WORKER_TYPE('acdc_queue_manager_diag', 'transient')]).

%%%=============================================================================
%%% api functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the supervisor.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%%------------------------------------------------------------------------------
%% @doc Start queue diagnostics for the specified queue.
%% @end
%%------------------------------------------------------------------------------
-spec start_diagnostics(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_types:sup_startchild_ret().
start_diagnostics(AccountId, QueueId) ->
    supervisor:start_child(?SERVER, [AccountId, QueueId, self()]).

%%------------------------------------------------------------------------------
%% @doc Send a diagnostics message to all the queue diagnostics receivers
%% specified by `DiagSrvs'.
%% @end
%%------------------------------------------------------------------------------
-spec send_diagnostics([pid()], iolist()) -> 'ok'.
send_diagnostics(DiagSrvs, Message) ->
    lists:foreach(fun(DiagSrv) ->
                          acdc_queue_manager_diag:send_diagnostics(DiagSrv, Message)
                  end
                 ,DiagSrvs
                 ).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Whenever a supervisor is started using `supervisor:start_link/[2,3]',
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> kz_types:sup_init_ret().
init([]) ->
    RestartStrategy = 'simple_one_for_one',
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 5,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
