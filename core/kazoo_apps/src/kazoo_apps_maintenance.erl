%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_apps_maintenance).

-export([start/1
        ,stop/1
        ,restart/1
        ,make_autostart_distinct/0
        ,ready/0
        ,check/0
        ]).

-include("kazoo_apps.hrl").

-define(CONFIG_CAT, <<"kapps_controller">>).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec start(kz_term:text()) -> kz_term:sup_no_return().
start(Application) ->
    case kapps_controller:start_app(Application) of
        {'ok', _} ->
            _ = maybe_add_autostart_application(Application),
            'no_return';
        {'error', _R} ->
            io:format("unable to start kazoo application ~s: ~p", [Application, _R]),
            {'no_return', 2}
    end.

-spec stop(kz_term:text()) -> kz_term:sup_no_return().
stop(Application) ->
    case kapps_controller:stop_app(Application) of
        'ok' ->
            _ = maybe_remove_autostart_application(Application),
            'no_return';
        {'error', _R} ->
            io:format("unable to stop kazoo application ~s: ~p", [Application, _R]),
            {'no_return', 2}
    end.

-spec restart(kz_term:text()) -> kz_term:sup_no_return().
restart(Application) ->
    case kapps_controller:restart_app(Application) of
        {'ok', _} ->
            io:format("restarted kazoo application ~s~n", [Application]),
            'no_return';
        {'error', _R} ->
            io:format("unable to restart kazoo application ~s: ~p", [Application, _R]),
            {'no_return', 2}
    end.

-spec make_autostart_distinct() -> kz_term:sup_no_return().
make_autostart_distinct() ->
    Configuration = kapps_controller:running_apps(),
    _ = kapps_config:set_node(?CONFIG_CAT, <<"kapps">>, Configuration, node()),
    _ = kapps_config:flush(),
    'no_return'.

-spec ready() -> kz_term:sup_no_return().
ready() ->
    case kapps_controller:ready() of
        'true' ->
            io:format("all kazoo applications have been started~n", []),
            'no_return';
        'false' ->
            io:format("some kazoo applications are not running~n", []),
            {'no_return', 2}
    end.

-spec check() -> kz_term:sup_no_return().
check() ->
    Configured = kapps_controller:start_which_kapps(),
    Running = [kz_term:to_binary(Application)
               || Application <- kapps_controller:running_apps()
              ],
    Props = [{'unexpected', lists:subtract(Running, Configured)}
            ,{'missing', lists:subtract(Configured, Running)}
            ,{'running', Running}
            ,{'configured', Configured}
            ],
    check(Props, 'no_return').

-spec check(kz_term:proplist(), kz_term:sup_no_return()) -> kz_term:sup_no_return().
check([], Return) -> Return;
check([{_, []}|Tail], Return) ->
    check(Tail, Return);
check([{Type, Applications}| Tail], Return) ->
    List = kz_binary:join(lists:sort(Applications), $,),
    _ = case length(Applications) > 1 of
            'true' ->
                io:format("~10s applications: ~s~n", [Type, List]);
            'false' ->
                io:format("~11s application: ~s~n", [Type, List])
        end,
    case Type of
        'unexpected' -> check(Tail, {'no_return', 2});
        'missing' -> check(Tail, {'no_return', 2});
        _Else -> check(Tail, Return)
    end.

-spec maybe_add_autostart_application(kz_term:ne_binary()) -> 'ok'.
maybe_add_autostart_application(Application) ->
    Configured = kapps_controller:start_which_kapps(),
    case lists:member(Application, Configured) of
        'true' -> io:format("started application ~s~n", [Application]);
        'false' ->
            _ = persist_application([Application|Configured]),
            io:format("started and added ~s to autostart applications~n", [Application])
    end.

-spec maybe_remove_autostart_application(kz_term:ne_binary()) -> 'ok'.
maybe_remove_autostart_application(Application) ->
    Configured = kapps_controller:start_which_kapps(),
    case lists:member(Application, Configured) of
        'false' -> io:format("stopped kazoo application ~s~n", [Application]);
        'true' ->
            _ = persist_application(lists:delete(Application,Configured)),
            io:format("stopped and removed ~s from autostart applications~n", [Application])
    end.

-spec persist_application(kz_term:ne_binaries()) -> {'ok', kz_json:object()}.
persist_application(Configuration) ->
    case kapps_config:get_node_value(?CONFIG_CAT, <<"kapps">>) of
        'undefined' ->
            kapps_config:set_default(?CONFIG_CAT, <<"kapps">>, Configuration);
        _Else ->
            kapps_config:set(?CONFIG_CAT, <<"kapps">>, Configuration)
    end.
