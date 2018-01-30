%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Voyager Internet Ltd.
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Ben Partridge
%%%-------------------------------------------------------------------
-module(navi_notification_server_sup).

-behaviour(supervisor).

-include("navi.hrl").

%% API
-export([start_link/2
        ,get_living_children/1
        ,push/5
        ]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @public
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link(kz_json:object(), ne_binary()) -> startlink_ret().
start_link(ServerConfig, MyName) ->
    lager:debug("Initialising ~s supervisor", [kz_json:get_value(<<"notification_type">>, ServerConfig)]),
    Type = kz_json:get_value(<<"notification_type">>, ServerConfig),
    AppName = kz_json:get_value(<<"app_name">>, ServerConfig),
    supervisor:start_link({'local', kz_term:to_atom(MyName, 'true')}, ?MODULE, [Type, AppName, ServerConfig]).

-spec push(supervisor:sup_ref(), ne_binary(), ne_binary(), ne_binary(), kz_proplist()) -> 'ok' | 'error'.
push(Super, RegistrationId, NotificationType, Msg, ExtraParameters) ->
    %% Should be exactly one child if it has not crashed
    lager:debug("Request to push notification through ~s supervisor", [NotificationType]),
    [{Name, _Id, ChildPid, [ChildMod|[]]}|[]] = [{kz_term:to_binary(Id), Id, Pid, Mod} || {Id, Pid, Mod} <- get_living_children(Super)],
    lager:debug("Sending a notification through server: ~p, module: ~p", [Name, ChildMod]),
    ChildMod:push(ChildPid, RegistrationId, Msg, ExtraParameters),
    'ok'.

%%--------------------------------------------------------------------
%% @doc gets the supervisor's child. Can be used to determine
%% if the notification server is alive
%%--------------------------------------------------------------------
-spec get_living_children(supervisor:sup_ref()) -> [{supervisor:child_id(), supervisor:child(), supervisor:modules()}].
get_living_children(Super) ->
    [{Id, Pid, Modules} || {Id, Pid, _Type, Modules} <- supervisor:which_children(Super), Pid =/= 'undefined'].

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
init([Type, AppName, ServerConfig]) ->
    kz_util:set_startup(),
    RestartStrategy = 'one_for_one',
    MaxRestarts = 5,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Module = kz_term:to_atom(kz_term:to_binary(io_lib:format("nv_~s", [Type])), 'true'),
    WorkerName = kz_term:to_atom(kz_term:to_binary(io_lib:format("nv_~s_~s", [AppName, Type])), 'true'),
    lager:debug("Declared ~p worker: ~p", [Type, WorkerName]),

    {'ok', {SupFlags, [?WORKER_NAME_ARGS(Module, WorkerName, [WorkerName, ServerConfig])]}}.
