%%%-------------------------------------------------------------------
%%% @copyright (C) 2016, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%-------------------------------------------------------------------
-module(edr_backend_sup).

-behaviour(supervisor).

-export([init/1
        ,start_link/0
        ,start_backend/1, start_backend/4
        ,stop_backend/1
        ,get_running_backends/0
        ]).

-include("edr.hrl").

-define(CHILDREN, []).
-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================
-spec get_running_backends()-> [{Id :: ne_binary(), pid(), [module()]}].
get_running_backends()->
    [{Id, Pid, Module} || {Id, Pid, _Type, Module} <- supervisor:which_children(?SERVER), Pid =/= 'undefined'].

-spec start_backend(ne_binary()) -> {'error', 'not_registred'} | sup_startchild_ret().
-spec start_backend(ne_binary(), ne_binary(), kz_json:object(), kz_json:object()) -> sup_startchild_ret().
start_backend(Name)->
    Backends = kapps_config:get(<<"edr">>, <<"backends">>, kz_json:new()),
    case kz_json:get_value(Name, Backends) of
        'undefined' -> {'error', 'not_registred'};
        JBackend ->
            Type=kz_json:get_value(<<"Type">>, JBackend),
            JTags=kz_json:get_value(<<"Tags">>, JBackend),
            JOpts=kz_json:get_value(<<"Options">>, JBackend),
            start_backend(Name, Type, JTags, JOpts)
    end.
start_backend(Name, Type, Tags, Opts)->
    Module = kz_term:to_atom("edr_" ++ binary_to_list(Type)),
    Backend = #backend{name=Name
                      ,type=Type
                      ,tags=Tags
                      ,options=Opts
                      ,enabled='true'
                      },
    supervisor:start_child(?SERVER, ?WORKER_NAME_ARGS_TYPE(Name, Module, [Backend], 'transient')).

-spec stop_backend(ne_binary()) -> 'ok' | {'error', any()}.
stop_backend(Name)->
    _ = supervisor:terminate_child(?SERVER, Name),
    supervisor:delete_child(?SERVER, Name).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Starts the supervisor
%% @end
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
-spec init([]) -> sup_init_ret().
init([]) ->
    kz_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, ?CHILDREN}}.
