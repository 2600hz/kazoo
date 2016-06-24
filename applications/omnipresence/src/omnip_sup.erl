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

-include_lib("omnipresence/src/omnipresence.hrl").


-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(DEFAULT_MODULES, [<<"omnip_dialog_amqp">>
                          ,<<"omnip_message_summary_amqp">>
                          ,<<"omnip_presence_amqp">>
                         ]).

-define(CHILDREN, [ ?WORKER(module(Module))
                    || Module <- kapps_config:get(?CONFIG_CAT, <<"modules">>, ?DEFAULT_MODULES)
                  ]).

%% ===================================================================
%% API functions
%% ===================================================================

-spec start_module(ne_binary()) -> sup_startchild_ret().
start_module(Module) ->
    supervisor:start_child(?SERVER, ?WORKER(kz_util:to_atom(Module, 'true'))).

-spec stop_module(ne_binary()) -> 'ok' | {'error', any()}.
stop_module(Module) ->
    case supervisor:terminate_child(?SERVER, kz_util:to_atom(Module, 'true')) of
        'ok' ->
            _ = supervisor:delete_child(?SERVER, kz_util:to_atom(Module, 'true')),
            'ok';
        {'error', _}=E -> E
    end.

%% TODO
%% load / unload package
%% load default/configured packages
%%


%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

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
-spec init(any()) -> sup_init_ret().
init([]) ->
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {'ok', {SupFlags, ?CHILDREN}}.

-spec module(ne_binary()) -> atom().
module(<<"omnip_dialog">>) ->
    module(<<"omnip_dialog_amqp">>);
module(<<"omnip_presence">>) ->
    module(<<"omnip_presence_amqp">>);
module(<<"omnip_message_summary">>) ->
    module(<<"omnip_message_summary_amqp">>);
module(Module) ->
    kz_util:to_atom(Module, 'true').
