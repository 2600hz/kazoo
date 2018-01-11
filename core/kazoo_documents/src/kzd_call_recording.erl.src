-module(kzd_call_recording).

-export([new/0]).
-export([any/1, any/2, set_any/2]).
-export([inbound/1, inbound/2, set_inbound/2]).
-export([outbound/1, outbound/2, set_outbound/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec any(doc()) -> kz_term:api_object().
-spec any(doc(), Default) -> kz_json:object() | Default.
any(Doc) ->
    any(Doc, 'undefined').
any(Doc, Default) ->
    kz_json:get_json_value([<<"any">>], Doc, Default).

-spec set_any(doc(), kz_json:object()) -> doc().
set_any(Doc, Any) ->
    kz_json:set_value([<<"any">>], Any, Doc).

-spec inbound(doc()) -> kz_term:api_object().
-spec inbound(doc(), Default) -> kz_json:object() | Default.
inbound(Doc) ->
    inbound(Doc, 'undefined').
inbound(Doc, Default) ->
    kz_json:get_json_value([<<"inbound">>], Doc, Default).

-spec set_inbound(doc(), kz_json:object()) -> doc().
set_inbound(Doc, Inbound) ->
    kz_json:set_value([<<"inbound">>], Inbound, Doc).

-spec outbound(doc()) -> kz_term:api_object().
-spec outbound(doc(), Default) -> kz_json:object() | Default.
outbound(Doc) ->
    outbound(Doc, 'undefined').
outbound(Doc, Default) ->
    kz_json:get_json_value([<<"outbound">>], Doc, Default).

-spec set_outbound(doc(), kz_json:object()) -> doc().
set_outbound(Doc, Outbound) ->
    kz_json:set_value([<<"outbound">>], Outbound, Doc).
