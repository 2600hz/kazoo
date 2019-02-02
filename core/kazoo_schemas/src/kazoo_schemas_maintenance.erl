%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2019-, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kazoo_schemas_maintenance).

-export([diff_schemas/0, diff_schema/1]).

%% Prints differences between the in-db schema and the on-disk schema
-spec diff_schemas() -> 'ok'.
diff_schemas() ->
    kz_json_schema:diff('schema'),
    'ok'.

-spec diff_schema(kz_term:ne_binary()) -> 'ok'.
diff_schema(Schema) ->
    kz_json_schema:diff_schema(Schema, 'schema'),
    'ok'.
