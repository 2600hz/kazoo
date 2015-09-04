%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2014, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(omnip_sup).

-behaviour(supervisor).

-export([start_link/0
         ,start_module/1, stop_module/1
        ]).
-export([init/1]).

-include("omnipresence.hrl").


%% Helper macro for declaring children of supervisor
-define(DEFAULT_MODULES, [<<"omnip_dialog">>
                          ,<<"omnip_message_summary">>
                          ,<<"omnip_presence">>
                         ]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_module(ne_binary()) -> startchild_ret().
start_module(Module) ->
    supervisor:start_child(?MODULE, ?WORKER(wh_util:to_atom(Module, 'true'))).

-spec stop_module(ne_binary()) ->  'ok' | {'error', 'running' | 'restarting' | 'not_found' | 'simple_one_for_one'}.
stop_module(Module) ->
    _ = supervisor:terminate_child(?MODULE, wh_util:to_atom(Module, 'true')),
    supervisor:delete_child(?MODULE, wh_util:to_atom(Module, 'true')).

%% TODO
%% load / unload package
%% load default/configured packages
%%


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%% @end
%%--------------------------------------------------------------------
-spec init([]) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Children = [ ?WORKER(wh_util:to_atom(H, 'true'))
                 || H <- whapps_config:get(?CONFIG_CAT, <<"modules">>, ?DEFAULT_MODULES)
               ],

    {'ok', {SupFlags, Children}}.
