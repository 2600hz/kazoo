#!/usr/bin/env escript
%%! +A0
%% -*- coding: utf-8 -*-

-mode(compile).
-export([main/1]).

-define(OUT, "priv/couchdb/schemas/port_requests.to_scheduled.json").
-define(IN, ?OUT ++ ".src").

-include_lib("erlang_localtime/include/tz_index.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").

main(_) ->
    {ok, SchemaBin} = file:read_file(?IN),
    SchemaJObj = kz_json:decode(SchemaBin),
    TZEnumPath = [<<"properties">>, <<"schedule_on">>, <<"properties">>, <<"timezone">>, <<"enum">>],
    [<<"America/Los_Angeles">>] = kz_json:get_list_value(TZEnumPath, SchemaJObj),
    Enum = unique_values(?tz_index),
    NewSchemaJObj = kz_json:set_value(TZEnumPath, lists:sort(sets:to_list(Enum)), SchemaJObj),
    ok = file:write_file(?OUT, kz_json:encode(NewSchemaJObj)).

-spec unique_values(dict:dict(nonempty_string(), [nonempty_string()])) -> enum().
unique_values(Dict) ->
    dict:fold(fun unique_values_fold/3, sets:new(), Dict).

-type enum() :: sets:set(kz_term:ne_binary()).
-spec unique_values_fold(nonempty_string(), [nonempty_string()], enum()) -> enum().
unique_values_fold(_Key, Value, Acc) ->
    Values = [kz_term:to_binary(V) || V <- Value],
    sets:union(Acc, sets:from_list(Values)).
