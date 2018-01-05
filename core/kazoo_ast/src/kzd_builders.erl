-module(kzd_builders).

-export([build_accessors/0
        ,build_accessor/1
        ]).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-spec build_accessors() -> 'ok'.
build_accessors() ->
    [build_accessor(Schema) || Schema <- schemas()],
    'ok'.

-spec build_accessor(ne_binary()) -> 'ok'.
build_accessor(_Schema) ->
    'ok'.

-spec schemas() -> [file:filename()].
schemas() ->
    filelib:wildcard(kz_ast_util:schema_path(<<>>)).
