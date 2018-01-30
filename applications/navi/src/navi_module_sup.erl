%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Voyager Internet Ltd.
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Ben Partridge
%%%-------------------------------------------------------------------
-module(navi_module_sup).

-behaviour(supervisor).

%% API
-export([start_link/0
        ,get_living_children/0
        ,push/5
        ]).

%% Supervisor callbacks
-export([init/1]).

-include("navi.hrl").

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the supervisor
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({'local', ?SERVER}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc gets the supervisor's children. Can be used to determine
%% if a specific notification server is active
%%--------------------------------------------------------------------
-spec get_living_children() -> [{atom(),atom(),atom()}].
get_living_children() ->
    [{Id, Pid, Module} || {Id, Pid, _Type, Module} <- supervisor:which_children(?SERVER), Pid =/= 'undefined'].

%%--------------------------------------------------------------------
%% @doc Determines if there is a notification server for the supplied
%% notification type and app. If there is, send the notification.
%%--------------------------------------------------------------------
-spec push(ne_binary(), ne_binary(), ne_binary(), ne_binary(), kz_proplist()) -> 'ok' | 'error'.
push(RegistrationId, AppName, NotificationType, Msg, ExtraParameters) ->
    Name =  get_notification_server_name(AppName, NotificationType),
    lager:debug("Finding process to send push notification to: ~p", [Name]),
    Children = [{kz_term:to_binary(Id), Id, Pid} || {Id, Pid, _Module} <- get_living_children()],
    lager:debug("Children found: ~p", [Children]),
    case lists:keyfind(Name, 1, Children) of
        {Name, _Id, Pid} ->
            %% Child process exists
            lager:debug("Sending a ~p notification", [Name]),
            navi_notification_server_sup:push(Pid, RegistrationId, NotificationType, Msg, ExtraParameters),
            'ok';
        'false' ->
            %% Child process does not exit
            lager:error("No process found for notification server: ~p", [Name]),
            'error'
    end.

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
    RestartStrategy = 'one_for_one',
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 60,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {'ok', {SupFlags, get_children_specs()}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec get_children_specs() -> [supervisor:child_spec()].
get_children_specs() ->
    %% Get all notification service configs from db
    [ process_server_config(ServerConfig) || ServerConfig <- kapps_config:get_jsons(?CONFIG_CAT, <<"notification_servers">>)].


-spec process_server_config(kz_json:object()) -> supervisor:child_spec().
process_server_config(ServerConfig) ->
    %% Create supervisor process for every notification service required
    AppName = kz_json:get_value(<<"app_name">>, ServerConfig),
    Type = kz_json:get_value(<<"notification_type">>, ServerConfig),
    Name = get_notification_server_name(AppName, Type),
    Id = kz_term:to_atom(Name, 'true'),
    lager:debug("Initialising notification server: ~p", [Name]),
    ?SUPER_NAME_ARGS_TYPE(Id, 'navi_notification_server_sup', [ServerConfig, Name], 'temporary').

-spec get_notification_server_name(ne_binary(), ne_binary()) -> ne_binary().
get_notification_server_name(AppName, NotificationType) ->
    kz_term:to_binary(io_lib:format("nv_~s_~s_sup", [AppName, NotificationType])).
