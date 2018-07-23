#!/usr/bin/env escript
%%! +A0 -sname kazoo_xref
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main([TagsFile]) ->
    AppDirs = lists:foldl(fun add_app_dirs/2, [], kz_ast_util:project_apps()),
    Paths = [app_path(App) || App <- lists:usort(AppDirs)],
    tags:subdirs(Paths, [{'outfile', TagsFile}]).

add_app_dirs(App, Dirs) ->
    case application:load(App) of
        'ok' -> add_app_dirs(App, Dirs, application:get_key(App, 'applications'));
        {'error', {'already_loaded', App}} ->
            Dirs;
        {'error', _E} ->
            io:format("failed to load app ~p: ~p~n", [App, _E]),
            Dirs
    end.

add_app_dirs(App, Dirs, {'ok', DepApps}) ->
    _ = [application:load(DepApp)
         || DepApp <- DepApps,
            not lists:member(DepApp, Dirs)
        ],
    Dirs ++ [App | DepApps];
add_app_dirs(_App, Dirs, _Else) ->
    io:format("failed to list dep apps for ~s: ~p~n", [_App, _Else]),
    Dirs.

app_path(App) ->
    {'ok', [M | _]} = application:get_key(App, 'modules'),
    filename:dirname(filename:dirname(code:which(M))).
