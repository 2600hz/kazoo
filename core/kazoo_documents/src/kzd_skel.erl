%% Base document module to use when creating new document modules
-module(kzd_skel).

-export([new/0
        ,type/0, type/1
        ]).

-include("kz_documents.hrl").

-define(PVT_TYPE, <<"skel">>).
-define(SCHEMA, <<"skels">>).

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json_schema:default_object(?SCHEMA), type()).

-spec type() -> ne_binary().
-spec type(doc()) -> ne_binary().
type() -> ?PVT_TYPE.

type(Doc) ->
    kz_doc:type(Doc, ?PVT_TYPE).
