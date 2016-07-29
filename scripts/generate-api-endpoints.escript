#!/usr/bin/env escript
%%! -sname kazoo_xref
%% -*- coding: utf-8 -*-

-module(api_endpoints).
-mode('compile').
-compile([debug_info]).
-compile({'no_auto_import', [get/0]}).

-export([main/1]).

main(_) ->
    {'ok', CWD} = file:get_cwd(),
    Root = filename:dirname(CWD),
    CorePAs = [filename:join([Root, "core", "kazoo", "ebin"])
              ,filename:join([Root, "core", "kazoo_ast", "ebin"])
              ],

    'ok' = code:add_pathsa(CorePAs),

    cb_api_endpoints:to_ref_doc(),
    cb_api_endpoints:to_swagger_json().
