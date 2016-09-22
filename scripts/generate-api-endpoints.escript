#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main(_) ->
    cb_api_endpoints:to_ref_doc(),
    cb_api_endpoints:to_swagger_json().
