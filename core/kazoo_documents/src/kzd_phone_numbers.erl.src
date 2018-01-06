-module(kzd_phone_numbers).

-export([new/0]).
-export([carrier_name/1, carrier_name/2, set_carrier_name/2]).
-export([cnam/1, cnam/2, set_cnam/2]).
-export([create_with_state/1, create_with_state/2, set_create_with_state/2]).
-export([e911/1, e911/2, set_e911/2]).
-export([porting/1, porting/2, set_porting/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec carrier_name(doc()) -> api_ne_binary().
-spec carrier_name(doc(), Default) -> ne_binary() | Default.
carrier_name(Doc) ->
    carrier_name(Doc, 'undefined').
carrier_name(Doc, Default) ->
    kz_json:get_ne_binary_value(<<"carrier_name">>, Doc, Default).

-spec set_carrier_name(doc(), ne_binary()) -> doc().
set_carrier_name(Doc, CarrierName) ->
    kz_json:set_value(<<"carrier_name">>, CarrierName, Doc).

-spec cnam(doc()) -> api_object().
-spec cnam(doc(), Default) -> kz_json:object() | Default.
cnam(Doc) ->
    cnam(Doc, 'undefined').
cnam(Doc, Default) ->
    kz_json:get_json_value(<<"cnam">>, Doc, Default).

-spec set_cnam(doc(), kz_json:object()) -> doc().
set_cnam(Doc, Cnam) ->
    kz_json:set_value(<<"cnam">>, Cnam, Doc).

-spec create_with_state(doc()) -> api_binary().
-spec create_with_state(doc(), Default) -> binary() | Default.
create_with_state(Doc) ->
    create_with_state(Doc, 'undefined').
create_with_state(Doc, Default) ->
    kz_json:get_binary_value(<<"create_with_state">>, Doc, Default).

-spec set_create_with_state(doc(), binary()) -> doc().
set_create_with_state(Doc, CreateWithState) ->
    kz_json:set_value(<<"create_with_state">>, CreateWithState, Doc).

-spec e911(doc()) -> api_object().
-spec e911(doc(), Default) -> kz_json:object() | Default.
e911(Doc) ->
    e911(Doc, 'undefined').
e911(Doc, Default) ->
    kz_json:get_json_value(<<"e911">>, Doc, Default).

-spec set_e911(doc(), kz_json:object()) -> doc().
set_e911(Doc, E911) ->
    kz_json:set_value(<<"e911">>, E911, Doc).

-spec porting(doc()) -> api_object().
-spec porting(doc(), Default) -> kz_json:object() | Default.
porting(Doc) ->
    porting(Doc, 'undefined').
porting(Doc, Default) ->
    kz_json:get_json_value(<<"porting">>, Doc, Default).

-spec set_porting(doc(), kz_json:object()) -> doc().
set_porting(Doc, Porting) ->
    kz_json:set_value(<<"porting">>, Porting, Doc).
