%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%% @end
%%%-----------------------------------------------------------------------------
-module(callflow_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).
-export([listener_proc/0]).

%% Supervisor callbacks
-export([init/1]).

-include("callflow.hrl").

-define(SERVER, ?MODULE).

-define(ORIGIN_BINDINGS, [[{'type', <<"account">>}]
                         ,[{'type', <<"user">>}]
                         ,[{'type', <<"callflow">>}]
                         ,[{'type', <<"device">>}]
                         ,[{'type', <<"parked_calls">>}]
                         ,[{'doc_id', ?MANUAL_PRESENCE_DOC}]
                         ]).

-define(CACHE_PROPS, [{'origin_bindings', ?ORIGIN_BINDINGS}
                     ,'new_node_flush'
                     ,'channel_reconnect_flush'
                     ]).

-define(CHILDREN, [?CACHE_ARGS(?CACHE_NAME, ?CACHE_PROPS)
                  ,?WORKER('cf_shared_listener')
                  ,?WORKER('cf_listener')
                  ,?SUPER('cf_event_handler_sup')
                  ,?SUPER('cf_exe_sup')
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

-spec listener_proc() -> {'ok', pid()}.
listener_proc() ->
    [P] = [P || {Mod, P, _, _} <- supervisor:which_children(?SERVER),
                Mod =:= 'cf_listener'
          ],
    {'ok', P}.

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
    kz_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.
