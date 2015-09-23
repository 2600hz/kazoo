%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2015, 2600Hz
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
         ,list_apps/0
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
    _ = wh_util:spawn(fun initialize_whapps/0),
    'ignore'.

-spec start_app(atom() | nonempty_string() | ne_binary()) ->
                       'ok' | 'error' | 'exists'.
start_app(App) when not is_atom(App) ->
    start_app(wh_util:to_atom(App, 'true'));
start_app(App) ->
    case lists:keyfind(App, 1, application:which_applications()) of
        {App, _Desc, _Ver} ->
            lager:info("Kazoo app ~s already running (~s)", [App, _Ver]),
            'exists';
        'false' ->
            case application:start(App) of
                'ok' ->
                    lager:info("Kazoo app ~s is now running", [App]);
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
    case application:stop(App) of
        'ok' -> lager:info("stopped kazoo application ~s", [App]);
        {'error', {'not_started', App}} ->
            lager:error("~s is not currently running", [App]);
        {'error', _E}=Err ->
            lager:error("error stopping applicaiton ~s: ~p", [App, _E]),
            Err
    end.

-spec restart_app(atom() | nonempty_string() | ne_binary()) ->
                         'ok' | 'error' | 'exists'.
restart_app(App) when not is_atom(App) ->
    restart_app(wh_util:to_atom(App, 'true'));
restart_app(App) when is_atom(App) ->
    lager:info("restarting whistle application ~s", [App]),
    _ = stop_app(App),
    start_app(App).

-define(HIDDEN_APPS
        ,['amqp_client','asn1'
          ,'cowboy','cowlib','crypto'
          ,'folsom'
          ,'gproc'
          ,'ibrowse','inets'
          ,'kazoo_bindings','kazoo_caches','kazoo_token_buckets','kernel'
          ,'lager'
          ,'nksip'
          ,'public_key'
          ,'ranch'
          ,'sasl','socketio','ssl','stdlib','syslog'
          ,'webseq'
          ,'whistle_amqp','whistle_apps','whistle_couch','whistle_stats'
          ,'xmerl'
          ,'goldrush'
          ,'compiler', 'syntax_tools'
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

-spec running_apps_verbose() -> atoms() | string().
running_apps_verbose() ->
    case [wh_util:to_binary(io_lib:format("~s(~s): ~s~n", [App, Vsn, Desc]))
          || {App, Desc, Vsn} <- application:which_applications(),
             not lists:member(App, ?HIDDEN_APPS)
         ]
    of
        [] -> "whapps have not started yet, check that rabbitmq and bigcouch/haproxy are running at the configured addresses";
        Resp -> Resp
    end.

-spec running_apps_list() -> atoms() | string().
running_apps_list() ->
    case [App
          || {App, _, _} <- application:which_applications(),
             not lists:member(App, ?HIDDEN_APPS)
         ]
    of
        [] -> "whapps have not started yet, check that rabbitmq and bigcouch/haproxy are running at the configured addresses";
        Resp -> Resp
    end.

-spec initialize_whapps() -> 'ok'.
initialize_whapps() ->
    wh_util:put_callid(?LOG_SYSTEM_ID),
    case couch_mgr:db_exists(?WH_ACCOUNTS_DB) of
        'false' -> whapps_maintenance:refresh();
        'true' -> 'ok'
    end,
    WhApps = whapps_config:get(?MODULE, <<"whapps">>, ?DEFAULT_WHAPPS),
    StartWhApps = [wh_util:to_atom(WhApp, 'true') || WhApp <- WhApps],

    _ = [?MODULE:start_app(A) || A <- StartWhApps],
    lager:notice("auto-started whapps ~p", [StartWhApps]).

-spec list_apps() -> atoms().
list_apps() ->
    case [App
          || {App, _, _} <- application:which_applications(),
             not lists:member(App, ?HIDDEN_APPS)
         ]
    of
        [] ->
            WhApps = whapps_config:get(?MODULE, <<"whapps">>, ?DEFAULT_WHAPPS),
            [wh_util:to_atom(WhApp, 'true') || WhApp <- WhApps];
        Resp -> Resp
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
