%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2012, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(whapps_controller).

%% API
-export([start_link/0
         ,start_app/1
         ,stop_app/1
         ,restart_app/1
         ,running_apps/0, running_apps/1
        ]).

-include("whistle_apps.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    spawn(fun() -> put('callid', ?LOG_SYSTEM_ID), initialize_whapps() end),
    'ignore'.

-spec start_app(atom() | nonempty_string() | ne_binary()) ->
                       'ok' | 'error' | 'exists'.
start_app(App) when not is_atom(App) ->
    start_app(wh_util:to_atom(App, 'true'));
start_app(App) ->
    case lists:keyfind(App, 1, application:which_applications()) of
        {App, _, _} ->
            lager:info("Kazoo app ~s already running", [App]),
            'exists';
        'false' ->
            case application:start(App) of
                'ok' -> lager:info("Kazoo app ~s is now running", [App]);
                {'error', _R} ->
                    lager:error("Kazoo app ~s failed to start: ~p", [App, _R]),
                    'error'
            end
    end.

-spec stop_app(atom() | nonempty_string() | ne_binary()) ->
                      'ok' | {'error', 'running' | 'not_found' | 'simple_one_for_one'}.
stop_app(App) when not is_atom(App) ->
    stop_app(wh_util:to_atom(App));
stop_app(App) ->
    _ = application:stop(App),
    lager:info("stopped kazoo application ~s", [App]).

-spec restart_app(atom() | nonempty_string() | ne_binary()) ->
                         {'ok', pid() | 'undefined'} |
                         {'ok', pid() | 'undefined', term()} |
                         {'error', term()}.
restart_app(App) when not is_atom(App) ->
    restart_app(wh_util:to_atom(App));
restart_app(App) when is_atom(App) ->
    lager:info("restarting whistle application ~s", [App]),
    application:stop(App),
    application:start(App).

-define(HIDDEN_APPS, ['kernel', 'stdlib', 'ibrowse', 'cowboy', 'ranch', 'crypto'
                      ,'inets', 'ssl', 'public_key', 'whistle_apps', 'whistle_amqp'
                      ,'whistle_stats', 'sasl', 'lager', 'gproc', 'amqp_client'
                      ,'syslog'
                     ]).
-spec running_apps() -> atoms() | string().
-spec running_apps(boolean()) -> atoms() | string().
running_apps() ->
    running_apps('false').

running_apps(Verbose) ->
    case wh_util:is_true(Verbose) of
        'true' -> running_apps_verbose();
        'false' -> running_apps_list()
    end.

running_apps_verbose() ->
    case [wh_util:to_binary(io_lib:format("~s(~s): ~s~n", [App, Vsn, Desc]))
          || {App, Desc, Vsn} <- application:which_applications(),
             not lists:member(App, ?HIDDEN_APPS)
         ]
    of
        [] -> "whapps have not started yet, check that rabbitmq and bigcouch/haproxy are running at the configured addresses";
        Resp -> Resp
    end.

running_apps_list() ->
    case [App
          || {App, _, _} <- application:which_applications(),
             not lists:member(App, ?HIDDEN_APPS)
         ]
    of
        [] -> "whapps have not started yet, check that rabbitmq and bigcouch/haproxy are running at the configured addresses";
        Resp -> Resp
    end.

initialize_whapps() ->
    case couch_mgr:db_exists(?WH_ACCOUNTS_DB) of
        'false' -> whapps_maintenance:refresh();
        'true' -> 'ok'
    end,
    WhApps = whapps_config:get(?MODULE, <<"whapps">>, []),
    StartWhApps = [wh_util:to_atom(WhApp, 'true') || WhApp <- WhApps],
    %_ = whistle_apps_sup:initialize_whapps([]),
    [?MODULE:start_app(A) || A <- StartWhApps],
    lager:notice("auto-started whapps ~p", [StartWhApps]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
