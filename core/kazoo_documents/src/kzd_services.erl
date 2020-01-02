%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_services).

-export([new/0]).
-export([type/0
        ,type/1
        ,set_type/1
        ]).
-export([account_quantities/1
        ,account_quantities/2
        ,set_account_quantities/2
        ]).
-export([cascade_quantities/1
        ,cascade_quantities/2
        ,set_cascade_quantities/2
        ]).
-export([manual_quantities/1
        ,manual_quantities/2
        ,set_manual_quantities/2
        ]).
-export([overrides/1
        ,overrides/2
        ,set_overrides/2
        ]).
-export([ratedeck_id/1
        ,ratedeck_id/2
        ,set_ratedeck_id/2
        ]).
-export([ratedeck_name/1
        ,ratedeck_name/2
        ,set_ratedeck_name/2
        ]).
-export([plans/1
        ,plans/2
        ,set_plans/2
        ]).
-export([plan_ids/1
        ,plan/2
        ,plan/3
        ,set_plan/3
        ]).
-export([plan_vendor_id/2
        ,plan_vendor_id/3
        ,set_plan_vendor_id/3
        ]).
-export([plan_overrides/2
        ,plan_overrides/3
        ,set_plan_overrides/3
        ]).
-export([reseller_id/1
        ,reseller_id/2
        ,set_reseller_id/2
        ,is_reseller/1
        ,is_reseller/2
        ,set_is_reseller/2
        ]).
-export([tree/1
        ,tree/2
        ,set_tree/2
        ]).
-export([bookkeeper/1
        ,bookkeeper/2
        ,set_bookkeeper/2
        ,default_bookkeeper_type/0
        ]).
-export([bookkeeper_vendor_id/1
        ,bookkeeper_vendor_id/2
        ,set_bookkeeper_vendor_id/2
        ]).
-export([bookkeeper_id/1
        ,bookkeeper_id/2
        ,set_bookkeeper_id/2
        ,default_bookkeeper_id/0
        ]).
-export([bookkeeper_type/1
        ,bookkeeper_type/2
        ,set_bookkeeper_type/2
        ]).
-export([status_good/0
        ,status_delinquent/0
        ,status/1
        ,status/2
        ,set_status/2
        ]).
-export([payment_tokens/1
        ,payment_tokens/2
        ,set_payment_tokens/2

        ,payment_token/2
        ,payment_token/3
        ,set_payment_token/3
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(QUANTITIES, <<"quantities">>).
-define(ACCOUNT_QUANTITIES, [?QUANTITIES, <<"account">>]).
-define(CASCADE_QUANTITIES, [?QUANTITIES, <<"cascade">>]).
-define(MANUAL_QUANTITIES, [?QUANTITIES, <<"manual">>]).
-define(PLANS, <<"plans">>).
-define(PLAN(PlanId), [?PLANS, PlanId]).
-define(PLAN_VENDOR_ID(PlanId), [?PLANS, PlanId, <<"vendor_id">>]).
-define(PLAN_OVERRIDES(PlanId), [?PLANS, PlanId, <<"overrides">>]).
-define(OVERRIDES, <<"overrides">>).
-define(RATEDECK, <<"ratedeck">>).
-define(RATEDECK_ID, [?RATEDECK, <<"id">>]).
-define(RATEDECK_NAME, [?RATEDECK, <<"name">>]).
-define(RESELLER_ID, <<"pvt_reseller_id">>).
-define(IS_RESELLER, <<"pvt_reseller">>).
-define(TREE, <<"pvt_tree">>).
-define(BOOKKEEPER, <<"bookkeeper">>).
-define(BOOKKEEPER_ID, [?BOOKKEEPER, <<"id">>]).
-define(BOOKKEEPER_VENDOR, [?BOOKKEEPER, <<"vendor_id">>]).
-define(BOOKKEEPER_TYPE, [?BOOKKEEPER, <<"type">>]).
-define(STATUS, <<"pvt_status">>).
-define(IS_DELETED, <<"pvt_deleted">>).

-define(PVT_TYPE, <<"service">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec status_good() -> kz_term:ne_binary().
status_good() -> <<"good_standing">>.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec status_delinquent() -> kz_term:ne_binary().
status_delinquent() -> <<"delinquent">>.

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
-spec account_quantities(doc()) -> kz_json:object().
account_quantities(JObj) ->
    account_quantities(JObj, kz_json:new()).

-spec account_quantities(doc(), Default) -> kz_json:object() | Default.
account_quantities(JObj, Default) ->
    kz_json:get_json_value(?ACCOUNT_QUANTITIES, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_account_quantities(doc(), kz_json:object()) -> kz_json:object().
set_account_quantities(JObj, AccountQuantities) ->
    kz_json:set_value(?ACCOUNT_QUANTITIES, AccountQuantities, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cascade_quantities(doc()) -> kz_json:object().
cascade_quantities(JObj) ->
    cascade_quantities(JObj, kz_json:new()).

-spec cascade_quantities(doc(), Default) -> kz_json:object() | Default.
cascade_quantities(JObj, Default) ->
    kz_json:get_json_value(?CASCADE_QUANTITIES, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_cascade_quantities(doc(), kz_json:object()) -> kz_json:object().
set_cascade_quantities(JObj, CascadeQuantities) ->
    kz_json:set_value(?CASCADE_QUANTITIES, CascadeQuantities, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec manual_quantities(doc()) -> kz_json:object().
manual_quantities(JObj) ->
    manual_quantities(JObj, kz_json:new()).

-spec manual_quantities(doc(), Default) -> kz_json:object() | Default.
manual_quantities(JObj, Default) ->
    kz_json:get_json_value(?MANUAL_QUANTITIES, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_manual_quantities(doc(), kz_json:object()) -> kz_json:object().
set_manual_quantities(JObj, ManualQuantities) ->
    kz_json:set_value(?MANUAL_QUANTITIES, ManualQuantities, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec overrides(doc()) -> kz_json:object().
overrides(JObj) ->
    overrides(JObj, kz_json:new()).

-spec overrides(doc(), Default) -> kz_json:object() | Default.
overrides(JObj, Default) ->
    kz_json:get_json_value(?OVERRIDES, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_overrides(doc(), kz_json:object()) -> doc().
set_overrides(JObj, Overrides) ->
    kz_json:set_value(?OVERRIDES, Overrides, JObj).

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
-spec plans(doc()) -> kz_json:object().
plans(JObj) ->
    plans(JObj, kz_json:new()).

-spec plans(doc(), Default) -> kz_json:object() | Default.
plans(JObj, Default) ->
    kz_json:get_json_value(?PLANS, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_plans(doc(), kz_json:object()) -> doc().
set_plans(JObj, Plans) ->
    kz_json:set_value(?PLANS, Plans, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec plan_ids(doc()) -> kz_term:ne_binaries().
plan_ids(JObj) ->
    kz_json:get_keys(?PLANS, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec plan(doc(), kz_term:ne_binary()) -> kz_json:object().
plan(JObj, PlanId) ->
    plan(JObj, PlanId, kz_json:new()).

-spec plan(doc(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
plan(JObj, PlanId, Default) ->
    kz_json:get_ne_json_value([?PLANS, PlanId], JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_plan(doc(), kz_term:ne_binary(), kz_term:api_object()) -> doc().
set_plan(JObj, PlanId, 'undefined') ->
    kz_json:delete_key(?PLAN(PlanId), JObj);
set_plan(JObj, PlanId, Plan) ->
    kz_json:set_value(?PLAN(PlanId), Plan, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec plan_vendor_id(doc(), kz_term:ne_binary()) -> kz_term:api_binary().
plan_vendor_id(JObj, PlanId) ->
    plan_vendor_id(JObj, PlanId, 'undefined').

-spec plan_vendor_id(doc(), kz_term:ne_binary(), Default) -> kz_term:ne_binary() | Default.
plan_vendor_id(JObj, PlanId, Default) ->
    kz_json:get_ne_binary_value(?PLAN_VENDOR_ID(PlanId), JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_plan_vendor_id(doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> doc().
set_plan_vendor_id(JObj, PlanId, VendorId) ->
    kz_json:set_value(?PLAN_VENDOR_ID(PlanId), VendorId, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec plan_overrides(doc(), kz_term:ne_binary()) -> kz_json:object().
plan_overrides(JObj, PlanId) ->
    plan_overrides(JObj, PlanId, kz_json:new()).

-spec plan_overrides(doc(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
plan_overrides(JObj, PlanId, Default) ->
    kz_json:get_ne_json_value(?PLAN_OVERRIDES(PlanId), JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_plan_overrides(doc(), kz_term:ne_binary(), kz_json:object()) -> doc().
set_plan_overrides(JObj, PlanId, Overrides) ->
    kz_json:set_value(?PLAN_OVERRIDES(PlanId), Overrides, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec reseller_id(doc()) -> kz_term:api_binary().
reseller_id(JObj) ->
    reseller_id(JObj, 'undefined').

-spec reseller_id(doc(), Default) -> kz_term:ne_binary() | Default.
reseller_id(JObj, Default) ->
    kz_json:get_value(?RESELLER_ID, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_reseller_id(doc(), kz_term:api_binary()) -> doc().
set_reseller_id(JObj, ResellerId) ->
    kz_json:set_value(?RESELLER_ID, ResellerId, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_reseller(doc()) -> boolean().
is_reseller(JObj) ->
    is_reseller(JObj, 'false').

-spec is_reseller(doc(), Default) -> boolean() | Default.
is_reseller(JObj, Default) ->
    kz_json:is_true(?IS_RESELLER, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_is_reseller(doc(), boolean()) -> doc().
set_is_reseller(JObj, IsReseller) ->
    kz_json:set_value(?IS_RESELLER, IsReseller, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec tree(doc()) -> kz_term:ne_binaries().
tree(JObj) ->
    tree(JObj, []).

-spec tree(doc(), Default) -> kz_term:ne_binaries() | Default.
tree(JObj, Default) ->
    kz_json:get_value(?TREE, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_tree(doc(), kz_term:ne_binaries()) -> doc().
set_tree(JObj, Tree) ->
    kz_json:set_value(?TREE, Tree, JObj).

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
    bookkeeper_id(JObj, default_bookkeeper_id()).

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
-spec default_bookkeeper_id() -> kz_term:ne_binary().
default_bookkeeper_id() ->
    <<"default_bookkeeper">>.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec bookkeeper_type(doc()) -> kz_term:ne_binary().
bookkeeper_type(JObj) ->
    bookkeeper_type(JObj, default_bookkeeper_type()).

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
-spec default_bookkeeper_type() -> kz_term:ne_binary().
default_bookkeeper_type() ->
    <<"default">>.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec status(doc()) -> kz_term:ne_binary().
status(JObj) ->
    status(JObj, status_good()).

-spec status(doc(), Default) -> kz_term:ne_binary() | Default.
status(JObj, Default) ->
    kz_json:get_value(?STATUS, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_status(doc(), kz_term:api_binary()) -> doc().
set_status(JObj, Status) ->
    kz_json:set_value(?STATUS, Status, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec payment_tokens(doc()) -> kz_term:api_object().
payment_tokens(JObj) ->
    payment_tokens(JObj, 'undefined').

-spec payment_tokens(doc(), Default) -> kz_term:api_object() | Default.
payment_tokens(JObj, Default) ->
    kz_json:get_ne_json_value(<<"payment_tokens">>, JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_payment_tokens(doc(), kz_term:api_object()) -> doc().
set_payment_tokens(JObj, PaymentTokens) ->
    kz_json:set_value(<<"payment_tokens">>, PaymentTokens, JObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec payment_token(kz_term:ne_binary(), doc()) -> kz_term:api_object().
payment_token(TokenId, JObj) ->
    payment_token(TokenId, JObj, 'undefined').

-spec payment_token(kz_term:ne_binary(), doc(), Default) -> kz_term:api_object() | Default.
payment_token(TokenId, JObj, Default) ->
    kz_json:get_ne_json_value([<<"payment_tokens">>, TokenId], JObj, Default).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec set_payment_token(doc(), kz_term:ne_binary(), kz_term:api_object()) -> doc().
set_payment_token(JObj, ?NE_BINARY=TokenId, PaymentTokens) ->
    kz_json:set_value([<<"payment_tokens">>, TokenId], PaymentTokens, JObj).
