%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_service_plan).

-export([all_items_key/0
        ,merge_overrides/2
        ]).
-export([new/0]).
-export([type/0
        ,type/1
        ,set_type/1
        ]).
-export([bookkeeper/1
        ,bookkeeper/2
        ,set_bookkeeper/2
        ]).
-export([bookkeeper_vendor_id/1
        ,bookkeeper_vendor_id/2
        ,set_bookkeeper_vendor_id/2
        ]).
-export([bookkeeper_id/1
        ,bookkeeper_id/2
        ,set_bookkeeper_id/2
        ]).
-export([bookkeeper_type/1
        ,bookkeeper_type/2
        ,set_bookkeeper_type/2
        ]).
-export([ratedeck_id/1
        ,ratedeck_id/2
        ,set_ratedeck_id/2
        ]).
-export([ratedeck_name/1
        ,ratedeck_name/2
        ,set_ratedeck_name/2
        ]).
-export([asr/1
        ,asr/2
        ]).
-export([im/1
        ,im/2
        ]).
-export([applications/1
        ,applications/2
        ,set_applications/2
        ]).
-export([grouping_category/1
        ,grouping_category/2
        ,set_grouping_category/2
        ]).
-export([merge_strategy/1
        ,merge_strategy/2
        ,set_merge_strategy/2
        ]).
-export([merge_priority/1
        ,merge_priority/2
        ,set_merge_priority/2
        ]).
-export([plan/1
        ,plan/2
        ,set_plan/2
        ]).
-export([categories/1
        ,category/2
        ,category/3
        ,category_plan/1
        ,category_plan/2
        ]).
-export([items/2
        ,item/3
        ,item/4
        ]).
-export([limits/1
        ,limits/2
        ,set_limits/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(ALL, <<"_all">>).
-define(BOOKKEEPER, <<"bookkeeper">>).
-define(BOOKKEEPER_ID, [?BOOKKEEPER, <<"id">>]).
-define(BOOKKEEPER_VENDOR, [?BOOKKEEPER, <<"vendor_id">>]).
-define(BOOKKEEPER_TYPE, [?BOOKKEEPER, <<"type">>]).
-define(RATEDECK, <<"ratedeck">>).
-define(RATEDECK_ID, [?RATEDECK, <<"id">>]).
-define(RATEDECK_NAME, [?RATEDECK, <<"name">>]).
-define(APPLICATIONS, <<"applications">>).
-define(CATEGORY, <<"category">>).
-define(MERGE, <<"merge">>).
-define(MERGE_STRATEGY, [?MERGE, <<"strategy">>]).
-define(MERGE_PRIORITY, [?MERGE, <<"priority">>]).
-define(PLAN, <<"plan">>).

-define(PVT_TYPE, <<"service_plan">>).

-define(DEFAULT_MERGE_PRIORITY, 0).
-define(DEFAULT_MERGE_STRATEGY, <<"simple">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec all_items_key() -> kz_term:ne_binary().
all_items_key() -> ?ALL.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec merge_overrides(doc(), kz_json:object()) -> doc().
merge_overrides(JObj, Overrides) ->
    kz_json:merge(JObj, kz_json:from_list([{?PLAN, Overrides}])).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new() -> doc().
new() ->
    kz_doc:set_type(kz_json:new(), type()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec type() -> kz_term:ne_binary().
type() -> ?PVT_TYPE.

-spec type(kz_json:object()) -> kz_term:ne_binary().
type(JObj) ->
    kz_doc:type(JObj, type()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_type(doc()) -> doc().
set_type(JObj) ->
    kz_doc:set_type(JObj, type()).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper(doc()) -> kz_term:api_object().
bookkeeper(JObj) ->
    bookkeeper(JObj, 'undefined').

-spec bookkeeper(doc(), Default) -> kz_json:object() | Default.
bookkeeper(JObj, Default) ->
    kz_json:get_ne_json_value(?BOOKKEEPER, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper(doc(), kz_json:object()) -> doc().
set_bookkeeper(JObj, Bookkeeper) ->
    kz_json:set_value(?BOOKKEEPER, Bookkeeper, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_vendor_id(doc()) -> kz_term:api_binary().
bookkeeper_vendor_id(JObj) ->
    bookkeeper_vendor_id(JObj, 'undefined').

-spec bookkeeper_vendor_id(doc(), Default) -> kz_term:ne_binary() | Default.
bookkeeper_vendor_id(JObj, Default) ->
    kz_json:get_ne_binary_value(?BOOKKEEPER_VENDOR, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper_vendor_id(doc(), kz_term:ne_binary()) -> doc().
set_bookkeeper_vendor_id(JObj, VendorId) ->
    kz_json:set_value(?BOOKKEEPER_VENDOR, VendorId, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_id(doc()) -> kz_term:ne_binary().
bookkeeper_id(JObj) ->
    bookkeeper_id(JObj, kzd_services:default_bookkeeper_id()).

-spec bookkeeper_id(doc(), Default) -> Default | kz_term:ne_binary().
bookkeeper_id(JObj, Default) ->
    kz_json:get_ne_binary_value(?BOOKKEEPER_ID, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper_id(doc(), kz_term:ne_binary()) -> doc().
set_bookkeeper_id(JObj, BookkeeperId) ->
    kz_json:set_value(?BOOKKEEPER_ID, BookkeeperId, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_type(doc()) -> kz_term:ne_binary().
bookkeeper_type(JObj) ->
    bookkeeper_type(JObj, kzd_services:default_bookkeeper_type()).

-spec bookkeeper_type(doc(), Default) -> Default | kz_term:ne_binary().
bookkeeper_type(JObj, Default) ->
    kz_json:get_ne_binary_value(?BOOKKEEPER_TYPE, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_bookkeeper_type(doc(), kz_term:ne_binary()) -> doc().
set_bookkeeper_type(JObj, BookkeeperType) ->
    kz_json:set_value(?BOOKKEEPER_TYPE, BookkeeperType, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ratedeck_id(kz_json:object()) -> kz_term:api_binary().
ratedeck_id(JObj) ->
    ratedeck_id(JObj, 'undefined').

-spec ratedeck_id(doc(), Default) -> kz_term:ne_binary() | Default.
ratedeck_id(JObj, Default) ->
    kz_json:get_ne_binary_value(?RATEDECK_ID, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_ratedeck_id(doc(), kz_term:ne_binary()) -> doc().
set_ratedeck_id(JObj, RatedeckId) ->
    kz_json:set_value(?RATEDECK_ID, RatedeckId, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec ratedeck_name(kz_json:object()) -> kz_term:api_binary().
ratedeck_name(JObj) ->
    ratedeck_name(JObj, 'undefined').

-spec ratedeck_name(doc(), Default) -> kz_term:ne_binary() | Default.
ratedeck_name(JObj, Default) ->
    kz_json:get_ne_binary_value(?RATEDECK_NAME, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_ratedeck_name(doc(), kz_term:ne_binary()) -> doc().
set_ratedeck_name(JObj, RatedeckName) ->
    kz_json:set_value(?RATEDECK_NAME, RatedeckName, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec applications(doc()) -> kz_json:object().
applications(JObj) ->
    applications(JObj, kz_json:new()).

-spec applications(doc(), Default) -> Default | kz_json:object().
applications(JObj, Default) ->
    kz_json:get_ne_json_value(?APPLICATIONS, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_applications(doc(), kz_json:object()) -> doc().
set_applications(JObj, Applications) ->
    kz_json:set_value(?APPLICATIONS, Applications, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec asr(doc()) -> kz_json:object().
asr(JObj) ->
    asr(JObj, kz_json:new()).

-spec asr(doc(), Default) -> Default | kz_json:object().
asr(JObj, Default) ->
    kz_json:get_ne_json_value(<<"asr">>, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec im(doc()) -> kz_json:object().
im(JObj) ->
    im(JObj, kz_json:new()).

-spec im(doc(), Default) -> Default | kz_json:object().
im(JObj, Default) ->
    kz_json:get_ne_json_value(<<"im">>, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec limits(doc()) -> kz_json:object().
limits(JObj) ->
    limits(JObj, kz_json:new()).

-spec limits(doc(), Default) -> Default | kz_json:object().
limits(JObj, Default) ->
    kz_json:get_ne_json_value(<<"limits">>, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_limits(doc(), kz_json:object()) -> doc().
set_limits(JObj, Limits) ->
    kz_json:set_value(<<"limits">>, Limits, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec grouping_category(doc()) -> kz_term:api_ne_binary().
grouping_category(JObj) ->
    grouping_category(JObj, 'undefined').

-spec grouping_category(doc(), Default) -> kz_term:ne_binary() | Default.
grouping_category(JObj, Default) ->
    kz_json:get_ne_binary_value(?CATEGORY, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_grouping_category(doc(), kz_term:ne_binary()) -> doc().
set_grouping_category(JObj, Category) ->
    kz_json:set_value(?CATEGORY, Category, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec merge_strategy(doc()) -> kz_term:ne_binary().
merge_strategy(JObj) ->
    merge_strategy(JObj, ?DEFAULT_MERGE_STRATEGY).

-spec merge_strategy(doc(), Default) -> kz_term:ne_binary() | Default.
merge_strategy(JObj, Default) ->
    kz_json:get_ne_binary_value(?MERGE_STRATEGY, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_merge_strategy(doc(), kz_term:ne_binary()) -> doc().
set_merge_strategy(JObj, Strategy) ->
    kz_json:set_value(?MERGE_STRATEGY, Strategy, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec merge_priority(doc()) -> kz_term:api_integer().
merge_priority(JObj) ->
    merge_priority(JObj, 'undefined').

-spec merge_priority(doc(), Default) -> integer() | Default.
merge_priority(JObj, Default) ->
    kz_json:get_integer_value(?MERGE_PRIORITY, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_merge_priority(doc(), integer()) -> doc().
set_merge_priority(JObj, Priority) ->
    kz_json:set_value(?MERGE_PRIORITY, Priority, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec plan(doc()) -> kz_json:object().
plan(JObj) ->
    plan(JObj, kz_json:new()).

-spec plan(doc(), Default) -> kz_json:object() | Default.
plan(JObj, Default) ->
    kz_json:get_json_value(?PLAN, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_plan(doc(), kz_json:object()) -> doc().
set_plan(JObj, Plan) ->
    kz_json:set_value(?PLAN, Plan, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec categories(doc()) -> kz_term:ne_binaries().
categories(JObj) ->
    kz_json:get_keys(?PLAN, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec category(doc(), kz_term:ne_binary()) -> kz_term:api_object().
category(JObj, CategoryId) ->
    category(JObj, CategoryId, 'undefined').

-spec category(doc(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
category(JObj, CategoryId, Default) ->
    kz_json:get_json_value([?PLAN, CategoryId], JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec category_plan(doc()) -> kz_term:api_object().
category_plan(JObj) ->
    category_plan(JObj, 'undefined').

-spec category_plan(doc(), Default) -> kz_json:object() | Default.
category_plan(JObj, Default) ->
    category(JObj, ?ALL, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec items(doc(), kz_term:ne_binary()) -> kz_term:ne_binaries().
items(JObj, Category) ->
    kz_json:get_keys([?PLAN, Category], JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec item(doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:api_object().
item(JObj, CategoryId, ItemId) ->
    item(JObj, CategoryId, ItemId, 'undefined').

-spec item(doc(), kz_term:ne_binary(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
item(JObj, CategoryId, ItemId, Default) ->
    kz_json:get_json_value([?PLAN, CategoryId, ItemId], JObj, Default).
