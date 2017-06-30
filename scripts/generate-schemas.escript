#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode('compile').

-export([main/1]).

main(_) ->
    cf_data_usage:to_schema_docs(),
    kapps_config_usage:to_schema_docs(),
    conference_schema_builder:to_schema(),
    kapi_schemas:to_schemas().
