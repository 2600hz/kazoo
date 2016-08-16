#!/usr/bin/env escript
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main(_) ->
    case kast_app_deps:process_project() of
        [] -> 'ok';
        BadModules ->
            [output_bad_app(A, Apps) || {A, Apps} <- BadModules],
            erlang:halt(1)
    end.

output_bad_app(App, [DepApp]) ->
    io:format("application ~s is missing dependent app ~s in ~s.app~n", [App, DepApp, App]);
output_bad_app(App, DepApps) ->
    DepAppsBin = kz_util:join_binary(DepApps),
    io:format("application ~s is missing dependent apps ~s in ~s.app~n", [App, DepAppsBin, App]).
