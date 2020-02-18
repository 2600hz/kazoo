#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% API

main([KazooRoot, AppL]) ->
    App = list_to_atom(AppL),

    try calc(KazooRoot, App)
    catch
        _E:_R:_ST ->
            io:format('standard_error'
                     ,"failed to calculate used apps for ~s~n~s:~p~n~p~n"
                     ,[App, _E, _R, _ST]
                     ),
            halt(1)
    end.

calc(KazooRoot, App) ->
    _ = application:load(App),

    DepApps = get_dep_apps(KazooRoot, App),

    {KazooRoot, Apps} = lists:foldl(fun add_deps/2
                                   ,{KazooRoot, [A || A <- DepApps, is_kazoo_app(A)]}
                                   ,DepApps
                                   ),

    [io:format("~s ", [A]) || A <- Apps].

add_deps(App, {KazooRoot, Apps}) ->
    _ = application:load(App),

    ToAdd = [DepApp || DepApp <- get_dep_apps(KazooRoot, App),
                       not lists:member(DepApp, Apps),
                       is_kazoo_app(DepApp)
            ],

    lists:foldl(fun add_deps/2
               ,{KazooRoot, Apps ++ ToAdd}
               ,ToAdd
               ).

is_kazoo_app(App) when is_atom(App) ->
    is_kazoo_app(atom_to_list(App));
is_kazoo_app("kazoo" ++ _) -> 'true';
is_kazoo_app(_) -> 'false'.

get_dep_apps(KazooRoot, App) ->
    case application:get_key(App, 'applications') of
        'undefined' ->
            consult_for_app_deps(KazooRoot, App);
        {'ok', DepApps} -> DepApps
    end.

consult_for_app_deps(KazooRoot, App) ->
    AppL = atom_to_list(App),
    CoreOrApp = core_or_app(KazooRoot, AppL),
    AppFile = filename:join([KazooRoot, CoreOrApp, AppL, "src", AppL ++ ".app.src"]),
    {'ok', [{'application', _App, Config}]} = file:consult(AppFile),
    proplists:get_value('applications', Config, []).

core_or_app(KazooRoot, AppL) ->
    core_or_app(KazooRoot, AppL, filelib:wildcard(KazooRoot ++ "/{core,applications,deps}/" ++ AppL)).

core_or_app(KazooRoot, AppL, []) ->
    io:format('user', "failed to determine if ~s is core or dep in ~s~n", [AppL, KazooRoot]),
    throw({'error', 'not_found'});
core_or_app(_KazooRoot, _AppL, [Path]) ->
    filename:basename(filename:dirname(Path)).
