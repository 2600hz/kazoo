%%%-------------------------------------------------------------------
%%% @copyright (C) 2010-2016, 2600Hz
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
         ,start_default_apps/0
         ,stop_app/1
         ,restart_app/1
         ,running_apps/0, running_apps/1
         ,list_apps/0
        ]).

-include("whistle_apps.hrl").


-define(HIDDEN_APPS
        ,['amqp_client','apns','asn1'
          ,'braintree'
          ,'compiler','cowboy','cowlib','couchbeam','crypto'
          ,'escalus','exml'
          ,'folsom'
          ,'gcm','gen_smtp','goldrush','gproc'
          ,'ibrowse','inets'
          ,'kazoo_bindings','kazoo_caches','kazoo_documents','kazoo_modb'
          ,'kazoo_oauth','kazoo_token_buckets','kernel'
          ,'lager'
          ,'nksip'
          ,'poolboy','public_key'
          ,'rabbit_common','ranch'
          ,'sasl','socketio','ssl','stdlib','syntax_tools','syslog'
          ,'webseq'
          ,'whistle','whistle_amqp','whistle_apps','whistle_config'
          ,'whistle_couch','whistle_services','whistle_stats'
          ,'xmerl'
         ]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Starts the server
%%--------------------------------------------------------------------
-spec start_link() -> startlink_ret().
start_link() ->
    _ = wh_util:spawn(fun initialize_whapps/0),
    'ignore'.

start_default_apps() ->
    [{App, start_app(App)} || App <- ?DEFAULT_WHAPPS].

-spec start_app(atom() | nonempty_string() | ne_binary()) -> 'ok' | {'error', any()}.
start_app(App) when not is_atom(App) ->
    start_app(wh_util:to_atom(App, 'true'));
start_app(App) when is_atom(App) ->
    application:ensure_started(App).

-spec stop_app(atom() | nonempty_string() | ne_binary()) -> 'ok' | {'error', any()}.
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

-spec restart_app(atom() | nonempty_string() | ne_binary()) -> 'ok' | {'error', any()}.
restart_app(App) when not is_atom(App) ->
    restart_app(wh_util:to_atom(App, 'true'));
restart_app(App) when is_atom(App) ->
    lager:info("restarting whistle application ~s", [App]),
    _ = stop_app(App),
    start_app(App).

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
    case get_running_apps() of
        [] -> "whapps have not started yet, check that rabbitmq and bigcouch/haproxy are running at the configured addresses";
        Resp ->
            [wh_util:to_binary(io_lib:format("~s(~s): ~s~n", [App, Vsn, Desc]))
             || {App, Desc, Vsn} <- Resp
            ]
    end.

-spec get_running_apps() -> [{atom(), string(), _}].
get_running_apps() ->
    [AppData
     || {App, _Desc, _Vsn}=AppData <- application:which_applications(),
        not lists:member(App, ?HIDDEN_APPS)
    ].

-spec running_apps_list() -> atoms() | string().
running_apps_list() ->
    case get_running_apps() of
        [] -> "whapps have not started yet, check that rabbitmq and bigcouch/haproxy are running at the configured addresses";
        Resp -> [App || {App, _Desc, _Vsn} <- Resp]
    end.

-spec initialize_whapps() -> 'ok'.
initialize_whapps() ->
    wh_util:put_callid(?LOG_SYSTEM_ID),
    case couch_mgr:db_exists(?WH_ACCOUNTS_DB) of
        'false' -> whapps_maintenance:refresh();
        'true' -> 'ok'
    end,
    WhApps = case os:getenv("KAZOO_APPS", "") of
                 "" ->
                     whapps_config:get(?MODULE, <<"whapps">>, ?DEFAULT_WHAPPS);
                 KAZOO_APPS ->
                     string:tokens(KAZOO_APPS, ", ")
             end,
    StartWhApps = [wh_util:to_atom(WhApp, 'true') || WhApp <- WhApps],

    _ = [start_app(A) || A <- StartWhApps],
    lager:notice("auto-started whapps ~p", [StartWhApps]).

-spec list_apps() -> atoms().
list_apps() ->
    case get_running_apps() of
        [] ->
            WhApps = whapps_config:get(?MODULE, <<"whapps">>, ?DEFAULT_WHAPPS),
            [wh_util:to_atom(WhApp, 'true') || WhApp <- WhApps];
        Resp -> [App || {App, _, _} <- Resp]
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
