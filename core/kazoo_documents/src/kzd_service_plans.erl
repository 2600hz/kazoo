-module(kzd_service_plans).

-export([new/0]).
-export([bookkeepers/1, bookkeepers/2, set_bookkeepers/2]).
-export([category/1, category/2, set_category/2]).
-export([description/1, description/2, set_description/2]).
-export([manual_recurring/1, manual_recurring/2, set_manual_recurring/2]).
-export([name/1, name/2, set_name/2]).
-export([plan/1, plan/2, set_plan/2]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?MODULE_STRING).

-spec bookkeepers(doc()) -> api_object().
-spec bookkeepers(doc(), Default) -> kz_json:object() | Default.
bookkeepers(Doc) ->
    bookkeepers(Doc, 'undefined').
bookkeepers(Doc, Default) ->
    kz_json:get_json_value([<<"bookkeepers">>], Doc, Default).

-spec set_bookkeepers(doc(), kz_json:object()) -> doc().
set_bookkeepers(Doc, Bookkeepers) ->
    kz_json:set_value([<<"bookkeepers">>], Bookkeepers, Doc).

-spec category(doc()) -> api_binary().
-spec category(doc(), Default) -> binary() | Default.
category(Doc) ->
    category(Doc, 'undefined').
category(Doc, Default) ->
    kz_json:get_binary_value([<<"category">>], Doc, Default).

-spec set_category(doc(), binary()) -> doc().
set_category(Doc, Category) ->
    kz_json:set_value([<<"category">>], Category, Doc).

-spec description(doc()) -> api_binary().
-spec description(doc(), Default) -> binary() | Default.
description(Doc) ->
    description(Doc, 'undefined').
description(Doc, Default) ->
    kz_json:get_binary_value([<<"description">>], Doc, Default).

-spec set_description(doc(), binary()) -> doc().
set_description(Doc, Description) ->
    kz_json:set_value([<<"description">>], Description, Doc).

-spec manual_recurring(doc()) -> api_objects().
-spec manual_recurring(doc(), Default) -> kz_json:objects() | Default.
manual_recurring(Doc) ->
    manual_recurring(Doc, 'undefined').
manual_recurring(Doc, Default) ->
    kz_json:get_list_value([<<"manual_recurring">>], Doc, Default).

-spec set_manual_recurring(doc(), kz_json:objects()) -> doc().
set_manual_recurring(Doc, ManualRecurring) ->
    kz_json:set_value([<<"manual_recurring">>], ManualRecurring, Doc).

-spec name(doc()) -> api_ne_binary().
-spec name(doc(), Default) -> ne_binary() | Default.
name(Doc) ->
    name(Doc, 'undefined').
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec plan(doc()) -> api_object().
-spec plan(doc(), Default) -> kz_json:object() | Default.
plan(Doc) ->
    plan(Doc, 'undefined').
plan(Doc, Default) ->
    kz_json:get_json_value([<<"plan">>], Doc, Default).

-spec set_plan(doc(), kz_json:object()) -> doc().
set_plan(Doc, Plan) ->
    kz_json:set_value([<<"plan">>], Plan, Doc).
