#!/usr/bin/env escript
%%!
%% -*- coding: utf-8 -*-

%% Reads a running release's cookie from config.ini

-mode('compile').

-export([main/1]).

%% API

main(_) ->
    "-name "++Release = os:getenv("KZname"),
    REL = list_to_atom(lists:takewhile(fun ($@) -> 'false'; (_) -> 'true' end, Release)),
    INI = whistle_config_init:load_file(),
    {_, RelEnv} = lists:keyfind(REL, 1, INI),
    {_, Cookie} = lists:keyfind('cookie', 1, RelEnv),
    io:format("~s\n", [Cookie]).

%% End of Module
