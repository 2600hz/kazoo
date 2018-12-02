#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main(_) ->
    _ = application:ensure_all_started(yamerl),
    cb_api_endpoints:to_ref_doc('crossbar_filter'),
    cb_api_endpoints:to_ref_doc(),
    cb_api_endpoints:to_swagger_file().
