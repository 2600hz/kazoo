#!/usr/bin/env escript
%%! -sname kazoo_xref
%% -*- coding: utf-8 -*-

-module(cb_api_endpoints).
-mode('compile').
-compile([debug_info]).
-compile({'no_auto_import', [get/0]}).

-export([main/1]).

main(_) ->
    cb_api_endpoints:to_ref_doc(),
    cb_api_endpoints:to_swagger_json().
