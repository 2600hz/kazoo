-module(kzd_resource).

-export([new/0
        ,type/0, type/1
        ]).

-export([flat_rate_whitelist/1, flat_rate_whitelist/2
        ,flat_rate_blacklist/1, flat_rate_blacklist/2
        ]).

-include("kz_documents.hrl").

-define(PVT_TYPE, <<"resource">>).
-define(SCHEMA, <<"resources">>).

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

-spec flat_rate_whitelist(doc()) -> api_ne_binary().
-spec flat_rate_whitelist(doc(), Default) -> ne_binary() | Default.
flat_rate_whitelist(Doc) ->
    flat_rate_whitelist(Doc, 'undefined').
flat_rate_whitelist(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"flat_rate_whitelist">>, Doc, Default).

-spec flat_rate_blacklist(doc()) -> api_ne_binary().
-spec flat_rate_blacklist(doc(), Default) -> ne_binary() | Default.
flat_rate_blacklist(Doc) ->
    flat_rate_blacklist(Doc, 'undefined').
flat_rate_blacklist(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"flat_rate_blacklist">>, Doc, Default).
