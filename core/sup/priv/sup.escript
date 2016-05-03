#!/usr/bin/env escript
%%! -hidden
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

%% API

main(Args) ->
    sup:main(Args).

%% End of Module
