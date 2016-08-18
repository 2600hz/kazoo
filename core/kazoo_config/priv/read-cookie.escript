#!/usr/bin/env escript
%%!
%% -*- coding: utf-8 -*-

%% Reads a running release's cookie from config.ini

-mode('compile').

-export([main/1]).

%% API

main(_) ->
    "-name "++Release = os:getenv("KZname"),
    REL = lists:takewhile(fun ($@) -> 'false'; (_) -> 'true' end, Release),
    [Cookie] = kazoo_config_init:read_cookie(list_to_atom(REL)),
    io:format("~s\n", [Cookie]).

%% End of Module
