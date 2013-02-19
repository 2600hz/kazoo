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
-export([start_link/0, start_app/1, stop_app/1, restart_app/1, running_apps/0]).

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
    spawn(fun() -> put(callid, ?LOG_SYSTEM_ID), initialize_whapps() end),
    ignore.

-spec start_app/1 :: (atom() | nonempty_string() | ne_binary()) -> 'ok' | 'error' | 'exists'.
start_app(App) when not is_atom(App) ->
    start_app(wh_util:to_atom(App, true));
start_app(App) when is_atom(App) ->
    lager:debug("attempting to start whapp ~s", [App]),
    case lists:keyfind(App, 1, supervisor:which_children(whapps_sup)) of
        {App, _, _, _} ->
            lager:notice("the ~s whapp is already running", [App]),
            exists;
        false ->
            try
                {ok, _} = whapps_sup:start_app(App),
                lager:notice("started successfully started whapp ~s", [App]),
                ok
            catch
                E:R ->
                    lager:critical("failed to start whapp ~s: ~p:~p", [App, E, R]),
                    error
            end
    end.

-spec stop_app/1 :: (atom() | nonempty_string() | ne_binary()) -> 'ok' | {'error', 'running' | 'not_found' | 'simple_one_for_one'}.
stop_app(App) when not is_atom(App) ->
    stop_app(wh_util:to_atom(App));
stop_app(App) ->
    lager:notice("stopping whistle application ~s", [App]),
    whapps_sup:stop_app(App).

-spec restart_app/1 :: (atom() | nonempty_string() | ne_binary()) -> {'ok', pid() | 'undefined'} | {'ok', pid() | 'undefined', term()} | {'error', term()}.
restart_app(App) when not is_atom(App) ->
    restart_app(wh_util:to_atom(App));
restart_app(App) when is_atom(App) ->
    lager:info("restarting whistle application ~s", [App]),
    whapps_sup:restart_app(App).

-spec running_apps/0 :: () -> [atom(),...] | [].
running_apps() ->
    try supervisor:which_children(whapps_sup) of
        Apps ->
            [ App || {App, _, _, _} <- Apps ]
    catch
        _:_ ->
            "whapps have not started yet, check that rabbitmq and bigcouch/haproxy are running at the configured addresses"
    end. 

initialize_whapps() ->
    case whapps_config:get(?MODULE, <<"cookie">>) of
        undefined -> ok;
        Cookie ->
            lager:debug("changing the erlang cookie to ~s", [Cookie]),
            erlang:set_cookie(node(), wh_util:to_atom(Cookie, true))
    end,
    case couch_mgr:db_exists(?WH_ACCOUNTS_DB) of
        false -> whapps_maintenance:refresh();
        true -> ok
    end,
    WhApps = whapps_config:get(?MODULE, <<"whapps">>, []),
    StartWhApps = [wh_util:to_atom(WhApp, true) || WhApp <- WhApps],
    _ = whistle_apps_sup:initialize_whapps(StartWhApps),
    lager:notice("auto-started whapps ~p", [StartWhApps]).

%%%===================================================================
%%% Internal functions
%%%===================================================================
