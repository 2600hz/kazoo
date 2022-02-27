%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2022, 2600Hz
%%% @doc Accessors for `service_plans' document.
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_service_plans).

-export([new/0]).
-export([bookkeepers/1, bookkeepers/2, set_bookkeepers/2]).
-export([category/1, category/2, set_category/2]).
-export([description/1, description/2, set_description/2]).
-export([manual_recurring/1, manual_recurring/2, set_manual_recurring/2]).
-export([merge/1, merge/2, set_merge/2]).
-export([merge_priority/1, merge_priority/2, set_merge_priority/2]).
-export([merge_strategy/1, merge_strategy/2, set_merge_strategy/2]).
-export([name/1, name/2, set_name/2]).
-export([plan/1, plan/2, set_plan/2]).
-export([plan_name/2, plan_name/3, set_plan_name/3]).


-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(SCHEMA, <<"service_plans">>).

-spec new() -> doc().
new() ->
    kz_json_schema:default_object(?SCHEMA).

-spec bookkeepers(doc()) -> kz_term:api_object().
bookkeepers(Doc) ->
    bookkeepers(Doc, 'undefined').

-spec bookkeepers(doc(), Default) -> kz_json:object() | Default.
bookkeepers(Doc, Default) ->
    kz_json:get_json_value([<<"bookkeepers">>], Doc, Default).

-spec set_bookkeepers(doc(), kz_json:object()) -> doc().
set_bookkeepers(Doc, Bookkeepers) ->
    kz_json:set_value([<<"bookkeepers">>], Bookkeepers, Doc).

-spec category(doc()) -> kz_term:api_binary().
category(Doc) ->
    category(Doc, 'undefined').

-spec category(doc(), Default) -> binary() | Default.
category(Doc, Default) ->
    kz_json:get_binary_value([<<"category">>], Doc, Default).

-spec set_category(doc(), binary()) -> doc().
set_category(Doc, Category) ->
    kz_json:set_value([<<"category">>], Category, Doc).

-spec description(doc()) -> kz_term:api_binary().
description(Doc) ->
    description(Doc, 'undefined').

-spec description(doc(), Default) -> binary() | Default.
description(Doc, Default) ->
    kz_json:get_binary_value([<<"description">>], Doc, Default).

-spec set_description(doc(), binary()) -> doc().
set_description(Doc, Description) ->
    kz_json:set_value([<<"description">>], Description, Doc).

-spec manual_recurring(doc()) -> kz_term:api_objects().
manual_recurring(Doc) ->
    manual_recurring(Doc, 'undefined').

-spec manual_recurring(doc(), Default) -> kz_json:objects() | Default.
manual_recurring(Doc, Default) ->
    kz_json:get_list_value([<<"manual_recurring">>], Doc, Default).

-spec set_manual_recurring(doc(), kz_json:objects()) -> doc().
set_manual_recurring(Doc, ManualRecurring) ->
    kz_json:set_value([<<"manual_recurring">>], ManualRecurring, Doc).

-spec merge(doc()) -> kz_term:api_object().
merge(Doc) ->
    merge(Doc, 'undefined').

-spec merge(doc(), Default) -> kz_json:object() | Default.
merge(Doc, Default) ->
    kz_json:get_json_value([<<"merge">>], Doc, Default).

-spec set_merge(doc(), kz_json:object()) -> doc().
set_merge(Doc, Merge) ->
    kz_json:set_value([<<"merge">>], Merge, Doc).

-spec merge_priority(doc()) -> kz_term:api_integer().
merge_priority(Doc) ->
    merge_priority(Doc, 'undefined').

-spec merge_priority(doc(), Default) -> integer() | Default.
merge_priority(Doc, Default) ->
    kz_json:get_integer_value([<<"merge">>, <<"priority">>], Doc, Default).

-spec set_merge_priority(doc(), integer()) -> doc().
set_merge_priority(Doc, MergePriority) ->
    kz_json:set_value([<<"merge">>, <<"priority">>], MergePriority, Doc).

-spec merge_strategy(doc()) -> kz_term:api_binary().
merge_strategy(Doc) ->
    merge_strategy(Doc, 'undefined').

-spec merge_strategy(doc(), Default) -> binary() | Default.
merge_strategy(Doc, Default) ->
    kz_json:get_binary_value([<<"merge">>, <<"strategy">>], Doc, Default).

-spec set_merge_strategy(doc(), binary()) -> doc().
set_merge_strategy(Doc, MergeStrategy) ->
    kz_json:set_value([<<"merge">>, <<"strategy">>], MergeStrategy, Doc).

-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

-spec plan(doc()) -> kz_term:api_object().
plan(Doc) ->
    plan(Doc, 'undefined').

-spec plan(doc(), Default) -> kz_json:object() | Default.
plan(Doc, Default) ->
    kz_json:get_json_value([<<"plan">>], Doc, Default).

-spec set_plan(doc(), kz_json:object()) -> doc().
set_plan(Doc, Plan) ->
    kz_json:set_value([<<"plan">>], Plan, Doc).

-spec plan_name(doc(), kz_json:key()) -> kz_term:api_object().
plan_name(Doc, PlanName) ->
    plan_name(Doc, PlanName, 'undefined').

-spec plan_name(doc(), kz_json:key(), Default) -> kz_json:object() | Default.
plan_name(Doc, PlanName, Default) ->
    kz_json:get_json_value([<<"plan">>, PlanName], Doc, Default).

-spec set_plan_name(doc(), kz_json:key(), kz_json:object()) -> doc().
set_plan_name(Doc, PlanName, Value) ->
    kz_json:set_value([<<"plan">>, PlanName], Value, Doc).
