%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, 2600Hz
%%% @doc
%%%
%%% @end
%%% @contributors
%%%    SIPLABS, LLC (Vorontsov Nikita) <info@siplabs.ru>
%%%    Conversant Ltd (Max Lay)
%%%-------------------------------------------------------------------
-module(edr_backend_sup).

-behaviour(supervisor).

-export([init/1
        ,start_link/0
        ,start_backend/1
        ,stop_backend/1
        ,get_running_backends/0
        ]).

-include("edr.hrl").

-define(SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================
-spec get_running_backends() -> [{Id :: ne_binary(), pid(), [module()]}].
get_running_backends() ->
    [{Id, Pid, Module} || {Id, Pid, _Type, Module} <- supervisor:which_children(?SERVER), Pid =/= 'undefined'].

-spec registered_backends() -> [backend()].
registered_backends() ->
    [edr_util:backend_from_json(Backend) || Backend <- edr_maintenance:registered_backends()].

-spec start_backend(ne_binary() | backend()) -> {'error', 'not_registered'} | sup_startchild_ret().
start_backend(Name) when is_binary(Name) ->
    case [B || B <- registered_backends(), B#backend.name =:= Name] of
        [] -> {'error', 'not_registered'};
        [Backend] -> start_backend(Backend)
    end;
start_backend(#backend{name=Name}=Backend) ->
    lager:info("starting backend ~s", [Name]),
    supervisor:start_child(?SERVER, startup_child(Backend)).

-spec stop_backend(ne_binary()) -> 'ok' | {'error', any()}.
stop_backend(Name)->
    _ = supervisor:terminate_child(?SERVER, Name),
    supervisor:delete_child(?SERVER, Name).

-spec get_startup_children() -> [supervisor:child_spec()].
get_startup_children() ->
    [startup_child(B) || B <- registered_backends(), kz_term:is_true(B#backend.enabled)].

-spec startup_child(backend()) -> supervisor:child_spec().
startup_child(#backend{type=Type, name=Name}=Backend) ->
    Module = kz_term:to_atom("edr_be_" ++ binary_to_list(Type)),
    ?WORKER_NAME_ARGS_TYPE(Name, Module, [Backend], 'transient').

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
    {'ok', {SupFlags, get_startup_children()}}.
