%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2015-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(kzd_services).

-export([billing_id/1, billing_id/2
        ,is_reseller/1, is_reseller/2
        ,reseller_id/1, reseller_id/2
        ,is_dirty/1, is_dirty/2
        ,is_deleted/1, is_deleted/2
        ,status/1, status/2
        ,tree/1, tree/2
        ,reason/1, reason/2
        ,reason_code/1, reason_code/2

        ,type/0, type/1
        ,status_good/0
        ,status_delinquent/0

        ,plans/1, plans/2, plan_ids/1
        ,plan/2, plan/3
        ,plan_account_id/2, plan_account_id/3
        ,plan_overrides/2, plan_overrides/3
        ,quantities/1, quantities/2
        ,category_quantities/2, category_quantities/3
        ,item_quantity/3, item_quantity/4
        ,transactions/1, transactions/2
        ]).

-export([set_billing_id/2
        ,set_is_reseller/2
        ,set_reseller_id/2
        ,set_is_dirty/2
        ,set_status/2
        ,set_tree/2
        ,set_type/1
        ,set_plans/2
        ,set_plan/3
        ,set_quantities/2
        ,set_transactions/2
        ,set_reason/2
        ,set_reason_code/2
        ]).

-include("kz_documents.hrl").

-type doc() :: kz_json:object().
-export_type([doc/0]).

-define(BILLING_ID, <<"billing_id">>).
-define(IS_RESELLER, <<"pvt_reseller">>).
-define(RESELLER_ID, <<"pvt_reseller_id">>).
-define(IS_DIRTY, <<"pvt_dirty">>).
-define(IS_DELETED, <<"pvt_deleted">>).
-define(STATUS, <<"pvt_status">>).
-define(TREE, <<"pvt_tree">>).
-define(PLANS, <<"plans">>).
-define(QUANTITIES, <<"quantities">>).
-define(TRANSACTIONS, <<"transactions">>).
-define(REASON, <<"pvt_status_reason">>).
-define(REASON_CODE, <<"pvt_status_reason_code">>).

-spec billing_id(doc()) -> kz_term:api_binary().
billing_id(JObj) ->
    billing_id(JObj, kz_doc:account_id(JObj)).

-spec billing_id(doc(), Default) -> kz_term:ne_binary() | Default.
billing_id(JObj, Default) ->
    kz_json:get_value(?BILLING_ID, JObj, Default).

-spec set_billing_id(doc(), kz_term:api_binary()) -> doc().
set_billing_id(JObj, BillingId) ->
    kz_json:set_value(?BILLING_ID, BillingId, JObj).

-spec is_reseller(doc()) -> boolean().
is_reseller(JObj) ->
    is_reseller(JObj, 'false').

-spec is_reseller(doc(), Default) -> boolean() | Default.
is_reseller(JObj, Default) ->
    kz_json:is_true(?IS_RESELLER, JObj, Default).

-spec reseller_id(doc()) -> kz_term:api_binary().
reseller_id(JObj) ->
    reseller_id(JObj, 'undefined').

-spec reseller_id(doc(), Default) -> kz_term:ne_binary() | Default.
reseller_id(JObj, Default) ->
    kz_json:get_value(?RESELLER_ID, JObj, Default).

-spec is_dirty(doc()) -> boolean().
is_dirty(JObj) ->
    is_dirty(JObj, 'false').

-spec is_dirty(doc(), Default) -> boolean() | Default.
is_dirty(JObj, Default) ->
    kz_json:is_true(?IS_DIRTY, JObj, Default).

-spec is_deleted(doc()) -> boolean().
is_deleted(JObj) ->
    is_deleted(JObj, 'false').

-spec is_deleted(doc(), Default) -> boolean() | Default.
is_deleted(JObj, Default) ->
    kz_json:is_true(?IS_DELETED, JObj, Default).

-spec status(doc()) -> kz_term:ne_binary().
status(JObj) ->
    status(JObj, status_good()).

-spec status(doc(), Default) -> kz_term:ne_binary() | Default.
status(JObj, Default) ->
    kz_json:get_value(?STATUS, JObj, Default).

-spec tree(doc()) -> kz_term:ne_binaries().
tree(JObj) ->
    tree(JObj, []).

-spec tree(doc(), Default) -> kz_term:ne_binaries() | Default.
tree(JObj, Default) ->
    kz_json:get_value(?TREE, JObj, Default).

-spec reason(doc()) -> kz_term:ne_binaries().
reason(JObj) ->
    reason(JObj, 'undefined').

-spec reason(doc(), Default) -> kz_term:ne_binaries() | Default.
reason(JObj, Default) ->
    kz_json:get_value(?REASON, JObj, Default).

-spec reason_code(doc()) -> kz_term:ne_binaries().
reason_code(JObj) ->
    reason_code(JObj, 'undefined').

-spec reason_code(doc(), Default) -> kz_term:ne_binaries() | Default.
reason_code(JObj, Default) ->
    kz_json:get_value(?REASON_CODE, JObj, Default).

-spec type() -> kz_term:ne_binary().
type() -> <<"service">>.

-spec type(kz_json:object()) -> kz_term:ne_binary().
type(JObj) ->
    kz_doc:type(JObj, type()).

-spec status_good() -> kz_term:ne_binary().
status_good() -> <<"good_standing">>.

-spec status_delinquent() -> kz_term:ne_binary().
status_delinquent() -> <<"delinquent">>.

-spec plans(doc()) -> kz_json:object().
plans(JObj) ->
    plans(JObj, kz_json:new()).

-spec plans(doc(), Default) -> kz_json:object() | Default.
plans(JObj, Default) ->
    kz_json:get_json_value(?PLANS, JObj, Default).

-spec plan_ids(doc()) -> kz_term:ne_binaries().
plan_ids(JObj) ->
    kz_json:get_keys(?PLANS, JObj).

-spec plan(doc(), kz_term:ne_binary()) -> kz_json:object().
plan(JObj, PlanId) ->
    plan(JObj, PlanId, kz_json:new()).

-spec plan(doc(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
plan(JObj, PlanId, Default) ->
    kz_json:get_json_value([?PLANS, PlanId], JObj, Default).

-spec plan_account_id(doc(), kz_term:ne_binary()) -> kz_term:api_binary().
plan_account_id(JObj, PlanId) ->
    plan_account_id(JObj, PlanId, 'undefined').

-spec plan_account_id(doc(), kz_term:ne_binary(), Default) -> kz_term:api_binary() | Default.
plan_account_id(JObj, PlanId, Default) ->
    kzd_service_plan:account_id(plan(JObj, PlanId), Default).

-spec plan_overrides(doc(), kz_term:ne_binary()) -> kz_json:object().
plan_overrides(JObj, PlanId) ->
    plan_overrides(JObj, PlanId, kz_json:new()).

-spec plan_overrides(doc(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
plan_overrides(JObj, PlanId, Default) ->
    kzd_service_plan:overrides(plan(JObj, PlanId), Default).

-spec quantities(doc()) -> kz_json:object().
quantities(JObj) ->
    quantities(JObj, kz_json:new()).

-spec quantities(doc(), Default) -> kz_json:object() | Default.
quantities(JObj, Default) ->
    kz_json:get_json_value(?QUANTITIES, JObj, Default).

-spec category_quantities(doc(), kz_term:ne_binary()) -> kz_json:object().
category_quantities(JObj, CategoryId) ->
    category_quantities(JObj, CategoryId, kz_json:new()).

-spec category_quantities(doc(), kz_term:ne_binary(), Default) -> kz_json:object() | Default.
category_quantities(JObj, CategoryId, Default) ->
    kz_json:get_json_value([?QUANTITIES, CategoryId], JObj, Default).

-spec item_quantity(doc(), kz_term:ne_binary(), kz_term:ne_binary()) -> integer().
item_quantity(JObj, CategoryId, ItemId) ->
    item_quantity(JObj, CategoryId, ItemId, 0).

-spec item_quantity(doc(), kz_term:ne_binary(), kz_term:ne_binary(), Default) -> integer() | Default.
item_quantity(JObj, CategoryId, ItemId, Default) ->
    kz_json:get_integer_value([?QUANTITIES, CategoryId, ItemId], JObj, Default).

-spec transactions(doc()) -> kz_json:objects().
transactions(JObj) ->
    transactions(JObj, []).

-spec transactions(doc(), Default) -> kz_json:objects() | Default.
transactions(JObj, Default) ->
    kz_json:get_value(?TRANSACTIONS, JObj, Default).

-spec set_is_reseller(doc(), boolean()) -> doc().
set_is_reseller(JObj, IsReseller) ->
    kz_json:set_value(?IS_RESELLER, IsReseller, JObj).

-spec set_reseller_id(doc(), kz_term:api_binary()) -> doc().
set_reseller_id(JObj, ResellerId) ->
    kz_json:set_value(?RESELLER_ID, ResellerId, JObj).

-spec set_is_dirty(doc(), boolean()) -> doc().
set_is_dirty(JObj, IsDirty) ->
    kz_json:set_value(?IS_DIRTY, IsDirty, JObj).

-spec set_status(doc(), kz_term:api_binary()) -> doc().
set_status(JObj, Status) ->
    kz_json:set_value(?STATUS, Status, JObj).

-spec set_tree(doc(), kz_term:ne_binaries()) -> doc().
set_tree(JObj, Tree) ->
    kz_json:set_value(?TREE, Tree, JObj).

-spec set_reason(doc(), kz_term:ne_binary()) -> doc().
set_reason(JObj, Reason) ->
    kz_json:set_value(?REASON, Reason, JObj).

-spec set_reason_code(doc(), non_neg_integer()) -> doc().
set_reason_code(JObj, Code) when is_integer(Code) ->
    kz_json:set_value(?REASON_CODE, Code, JObj).

-spec set_type(doc()) -> doc().
set_type(JObj) ->
    kz_doc:set_type(JObj, type()).

-spec set_plans(doc(), kz_json:object()) -> doc().
set_plans(JObj, Plans) ->
    kz_json:set_value(?PLANS, Plans, JObj).

-spec set_plan(doc(), kz_term:ne_binary(), kz_term:api_object()) -> doc().
set_plan(JObj, PlanId, 'undefined') ->
    kz_json:delete_key([?PLANS, PlanId], JObj);
set_plan(JObj, PlanId, Plan) ->
    kz_json:set_value([?PLANS, PlanId], Plan, JObj).

-spec set_quantities(doc(), kz_json:object()) -> kz_json:object().
set_quantities(JObj, Quantities) ->
    kz_json:set_value(?QUANTITIES, Quantities, JObj).

-spec set_transactions(doc(), kz_json:objects()) -> doc().
set_transactions(JObj, Transactions) ->
    kz_json:set_value(?TRANSACTIONS, Transactions, JObj).
