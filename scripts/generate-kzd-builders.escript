#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main([]) ->
    kzd_builders:build_accessors();
main([Schema]) ->
    kzd_builders:build_accessor(Schema).
