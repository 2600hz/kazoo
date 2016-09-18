#!/usr/bin/env escript
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main(_) ->
    fs_prop_usage:to_header_file().
