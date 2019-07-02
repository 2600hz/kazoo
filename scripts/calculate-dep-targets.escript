#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% API

main([AppL]) ->
    App = list_to_atom(AppL),
    _ = application:load(App),

    DepApps = get_dep_apps(App),

    Apps = lists:foldl(fun add_deps/2, [A || A <- DepApps, is_kazoo_app(A)], DepApps),
    [io:format("~s ", [A]) || A <- Apps].

add_deps(App, Apps) ->
    _ = application:load(App),

    ToAdd = [DepApp || DepApp <- get_dep_apps(App),
                       not lists:member(DepApp, Apps),
                       is_kazoo_app(DepApp)
            ],

    lists:foldl(fun add_deps/2
               ,Apps ++ ToAdd
               ,ToAdd
               ).

is_kazoo_app(App) when is_atom(App) ->
    is_kazoo_app(atom_to_list(App));
is_kazoo_app("kazoo" ++ _) -> 'true';
is_kazoo_app(_) -> 'false'.

get_dep_apps(App) ->
    case application:get_key(App, 'applications') of
        'undefined' ->
            consult_for_app_deps(App, code:lib_dir(App, 'src'));
        {'ok', DepApps} -> DepApps
    end.

consult_for_app_deps(App, Ebin) ->
    {'ok', CWD} = file:get_cwd(),

    AppFile = filename:join([CWD, Ebin, atom_to_list(App) ++ ".app.src"]),

    {'ok', [{'application', _App, Config}]} = file:consult(AppFile),
    proplists:get_value('applications', Config, []).
