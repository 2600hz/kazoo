%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc Accessors for `kzd_activation_item' document.
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_activation_item).

-export([new/0]).
-export([billable/1, billable/2, set_billable/2]).
-export([category/1, category/2, set_category/2]).
-export([item/1, item/2, set_item/2]).
-export([name/1, name/2, set_name/2]).
-export([rate/1, rate/2, set_rate/2]).
-export([total/1, total/2, set_total/2]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec new() -> doc().
new() ->
    kz_json:new().

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec billable(doc()) -> non_neg_integer().
billable(Doc) ->
    billable(Doc, 0).

-spec billable(doc(), Default) -> non_neg_integer() | Default.
billable(Doc, Default) ->
    kz_json:get_integer_value([<<"billable">>], Doc, Default).

-spec set_billable(doc(), non_neg_integer()) -> doc().
set_billable(Doc, Billable) ->
    kz_json:set_value([<<"billable">>], Billable, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec category(doc()) -> kz_term:api_ne_binary().
category(Doc) ->
    category(Doc, 'undefined').

-spec category(doc(), Default) -> kz_term:ne_binary() | Default.
category(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"category">>], Doc, Default).

-spec set_category(doc(), kz_term:api_ne_binary()) -> doc().
set_category(Doc, Category) ->
    kz_json:set_value([<<"category">>], Category, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec item(doc()) -> kz_term:api_ne_binary().
item(Doc) ->
    item(Doc, 'undefined').

-spec item(doc(), Default) -> kz_term:ne_binary() | Default.
item(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"item">>], Doc, Default).

-spec set_item(doc(), kz_term:api_ne_binary()) -> doc().
set_item(Doc, Item) ->
    kz_json:set_value([<<"item">>], Item, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec name(doc()) -> kz_term:api_ne_binary().
name(Doc) ->
    name(Doc, 'undefined').

-spec name(doc(), Default) -> kz_term:ne_binary() | Default.
name(Doc, Default) ->
    kz_json:get_ne_binary_value([<<"name">>], Doc, Default).

-spec set_name(doc(), kz_term:api_ne_binary()) -> doc().
set_name(Doc, Name) ->
    kz_json:set_value([<<"name">>], Name, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec rate(doc()) -> float().
rate(Doc) ->
    rate(Doc, 0.0).

-spec rate(doc(), Default) -> float() | Default.
rate(Doc, Default) ->
    kz_json:get_float_value([<<"rate">>], Doc, Default).

-spec set_rate(doc(), kz_term:api_float()) -> doc().
set_rate(Doc, Rate) ->
    kz_json:set_value([<<"rate">>], Rate, Doc).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec total(doc()) -> float().
total(Doc) ->
    total(Doc, 0.0).

-spec total(doc(), Default) -> float() | Default.
total(Doc, Default) ->
    kz_json:get_float_value([<<"total">>], Doc, Default).

-spec set_total(doc(), kz_term:api_float()) -> doc().
set_total(Doc, Total) ->
    kz_json:set_value([<<"total">>], Total, Doc).
