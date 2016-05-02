#!/usr/bin/env escript
%%! -hidden
%% -*- coding: utf-8 -*-

%% Usage:
%%   sup -? | --help
%%   sup -h | --host <host>
%%   
%%
%% Options:
%%   -? --help      Show the program options
%%   -h --host      System hostname, defaults to system hostname
%% SUP -?, --help  Show 

-mode('compile').

-export([main/1]).

%% API

main(_) ->
    "-name "++Release = os:getenv("KZname"),
    REL = list_to_atom(lists:takewhile(fun ($@) -> 'false'; (_) -> 'true' end, Release)),
    INI = kazoo_config_init:load_file(),
    {_, RelEnv} = lists:keyfind(REL, 1, INI),
    {_, Cookie} = lists:keyfind('cookie', 1, RelEnv),
    io:format("~s\n", [Cookie]).

%% End of Module
