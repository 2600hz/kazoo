#!/usr/bin/env escript
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main(_) ->
    case kast_app_deps:process_project() of
        [] -> 'ok';
        BadApps ->
            [output_bad_app(A, Missing, Needed) || {A, Missing, Needed} <- BadApps],
            erlang:halt(1)
    end.

output_bad_app(App, MissingApps, UnneededApps) ->
    io:format("application ~s has discrepancies in its ~s.app file:~n", [App, App]),
    MissingApps =/= []
        andalso io:format("  apps missing from the list: ~s~n", [kz_util:join_binary(MissingApps)]),
    UnneededApps =/= []
        andalso io:format("  unnecessary apps: ~s~n", [kz_util:join_binary(UnneededApps)]),
    io:format("~n").
