%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_services_plans).

-export([empty/0]).
-export([fetch/1
        ,fetch/2
        ]).
-export([foldl/3]).

-export([public_json/1]).

-export([assigned/1]).
-export([overrides/1
        ,override/2
        ,override/3
        ]).

-export([merge/1]).

-export([editable_fields/0
        ,editable_fields/1
        ]).

-export([is_empty/1]).

-include("services.hrl").

-opaque plans() :: dict:dict().

-type plans_list() :: [kz_services_plan:plan()].
-type fold_fun() :: fun((kz_term:ne_binary(), plans_list(), Acc) -> Acc).
-type merge_strategy_plan() :: {non_neg_integer(), kz_json:object()}.
-type merge_strategy_plans() :: [merge_strategy_plan()].
-type merge_strategy_group() :: {kz_term:ne_binary(), merge_strategy_plans()}.
-type merge_strategy_groups() :: [merge_strategy_groups()].

-export_type([plans/0
             ,plans_list/0
             ,fold_fun/0
             ]).

-define(DEFAULT_PRIORITIES
       ,kz_json:from_list(
          [{<<"simple">>, 10}
          ,{<<"recursive">>, 25}
          ,{<<"cumulative">>, 50}
          ])
       ).
-define(MERGE_STRATEGY_PRIORITIES
       ,kapps_config:get_json(?CONFIG_CAT
                             ,<<"merge_strategy_priority">>
                             ,?DEFAULT_PRIORITIES
                             )
       ).

%%------------------------------------------------------------------------------
%% @doc Create an empty service plans data structure.
%% @end
%%------------------------------------------------------------------------------
-spec empty() -> plans().
empty() -> dict:new().

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type fetched_plans() :: dict:dict(). %% a dictionary of the plan json from the db
-type fetch_context() :: {fetched_plans(), plans()}.
-type fetch_options() :: #{services_jobj => kz_json:object()
                          ,modified_object_plans => kz_json:objects()
                          }.
-spec fetch(kz_services:services()) -> plans().
fetch(Services) ->
    fetch(Services, #{}).

-spec fetch(kz_services:services(), fetch_options()) -> plans().
fetch(Services, Options) ->
    lager:debug("fetching service plan documents"),
    Routines = [fun get_services_plan/3
               ,fun get_object_plans/3
               ],
    {_, Plans} =
        lists:foldl(fun(F, FetchContext) ->
                            F(Services, FetchContext, Options)
                    end
                   ,{dict:new(), empty()}
                   ,Routines
                   ),
    Plans.

-spec get_services_plan(kz_services:services(), fetch_context(), fetch_options()) -> fetch_context().
get_services_plan(Services, FetchContext, #{services_jobj := ServicesJObj}) ->
    lists:foldl(get_service_plans_fold(Services)
               ,FetchContext
               ,kzd_services:plan_ids(ServicesJObj)
               );
get_services_plan(Services, FetchContext, Options) ->
    ServicesJObj = kz_services:services_jobj(Services),
    get_services_plan(Services, FetchContext, Options#{services_jobj => ServicesJObj}).

-type service_plans_fold() :: fun((kz_term:ne_binary(), fetch_context()) -> fetch_context()).
-spec get_service_plans_fold(kz_services:services()) -> service_plans_fold().
get_service_plans_fold(Services) ->
    fun(PlanId, FetchContext) ->
            get_service_plan(Services, PlanId, FetchContext)
    end.

-spec get_service_plan(kz_services:services(), kz_term:ne_binary(), fetch_context()) -> fetch_context().
get_service_plan(Services, PlanId, FetchContext) ->
    Overrides = get_services_plan_overrides(Services, PlanId),
    VendorId = kzd_services:plan_vendor_id(kz_services:services_jobj(Services)
                                          ,PlanId
                                          ,kz_services:bookkeeper_vendor_id(Services)
                                          ),
    maybe_append_plan(Services, PlanId, VendorId, Overrides, FetchContext).

-spec get_services_plan_overrides(kz_services:services(), kz_term:ne_binary()) -> kz_json:object().
get_services_plan_overrides(Services, PlanId) ->
    ServicesJObj = kz_services:services_jobj(Services),
    PlansOverrides = kzd_services:overrides(ServicesJObj),
    PlanOverrides = kzd_services:plan_overrides(ServicesJObj, PlanId),
    kz_json:merge_recursive(PlansOverrides, PlanOverrides).

-spec get_object_plans(kz_services:services(), fetch_context(), fetch_options()) -> fetch_context().
get_object_plans(Services, FetchContext, Options) ->
    AccountId = kz_services:account_id(Services),
    case kz_datamgr:get_results(AccountId, <<"services/object_plans">>) of
        {'error', _Reason} ->
            lager:info("unable to list object plans for account ~s: ~p", [AccountId, _Reason]),
            FetchContext;
        {'ok', ObjectPlans} ->
            lager:debug("found ~p references to object plans", [length(ObjectPlans)]),
            build_object_plan(Services
                             ,FetchContext
                             ,handle_modified_object_plans(ObjectPlans, Options)
                             )
    end.

-spec handle_modified_object_plans(kz_json:objects(), fetch_options()) -> kz_json:objects().
handle_modified_object_plans(ObjectPlans, #{modified_object_plans := ModifiedObjectPlans}) ->
    ModifiedObjectPlanIds =
        [kz_doc:id(ModifiedObjectPlan)
         || ModifiedObjectPlan <- ModifiedObjectPlans
        ],
    FilteredObjectPlans =
        [ObjectPlan
         || ObjectPlan <- ObjectPlans
                ,not lists:member(kz_doc:id(ObjectPlan), ModifiedObjectPlanIds)
        ],
    UpdatedObjectPlans = FilteredObjectPlans ++ ModifiedObjectPlans,
    lager:debug("modified ~p object plans"
               ,[length(ModifiedObjectPlans)]
               ),
    UpdatedObjectPlans;
handle_modified_object_plans(ObjectPlans, _Options) ->
    ObjectPlans.

-spec build_object_plan(kz_services:services(), fetch_context(), kz_json:objects()) -> fetch_context().
build_object_plan(Services, FetchContext, ObjectPlans) ->
    Props = [{PlanId, kz_json:get_value([<<"value">>, PlanId], ObjectPlan)}
             || ObjectPlan <- ObjectPlans,
                PlanId <- kz_json:get_keys(<<"value">>, ObjectPlan)
            ],
    lists:foldl(get_object_plans_fold(Services)
               ,FetchContext
               ,Props
               ).

-type object_plans_fold() :: fun(({kz_term:ne_binary(), kz_json:object()}, fetch_context()) -> fetch_context()).
-spec get_object_plans_fold(kz_services:services()) -> object_plans_fold().
get_object_plans_fold(Services) ->
    fun({PlanId, JObj}, FetchContext) ->
            get_object_plan(Services, PlanId, JObj, FetchContext)
    end.

-spec get_object_plan(kz_services:services(), kz_term:ne_binary(), kz_json:object(), fetch_context()) -> fetch_context().
get_object_plan(Services, PlanId, JObj, FetchContext) ->
    DefaultVendorId = kz_services:bookkeeper_vendor_id(Services),
    VendorId = kz_json:get_ne_value(<<"vendor_id">>, JObj, DefaultVendorId),
    Overrides = kz_json:get_ne_value(<<"overrides">>, JObj, kz_json:new()),
    maybe_append_plan(Services, PlanId, VendorId, Overrides, FetchContext).

-spec maybe_append_plan(kz_services:services(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), fetch_context()) ->
          fetch_context().
maybe_append_plan(Services, PlanId, VendorId, Overrides, {FetchedPlans, ServicePlans}) ->
    case maybe_fetch_plan(PlanId, VendorId, FetchedPlans) of
        {'undefined', _} -> {FetchedPlans, ServicePlans};
        {Plan, UpdatedFetchedPlans} ->
            UpdatedPlan = prepare_plan(Services, Overrides, Plan),
            BookkeeperHash = kz_services_plan:bookkeeper_hash(UpdatedPlan),
            lager:debug("adding plan ~s/~s for bookkeeper ~s"
                       ,[kz_services_plan:vendor_id(UpdatedPlan)
                        ,kz_services_plan:id(UpdatedPlan)
                        ,kz_services_plan:bookkeeper_id(UpdatedPlan)
                        ]
                       ),
            {UpdatedFetchedPlans
            ,dict:append(BookkeeperHash, UpdatedPlan, ServicePlans)
            }
    end.

-spec prepare_plan(kz_services:services(), kz_json:object(), kz_services_plan:plan()) -> kz_services_plan:plan().
prepare_plan(Services, Overrides, Plan) ->
    Routines = [{fun kz_services_plan:set_overrides/2, Overrides}
               ,{fun kz_services_plan:set_default_bookkeeper/2
                ,default_bookkeeper(Services)
                }
               ],
    kz_services_plan:setters(Plan, Routines).

-spec default_bookkeeper(kz_services:services()) -> kz_json:object().
default_bookkeeper(Services) ->
    Type = kz_services:bookkeeper_type(Services),
    VendorId = kz_services:bookkeeper_vendor_id(Services),
    Id = kzd_services:bookkeeper_id(
           kz_services:services_jobj(Services)
          ),
    Routines = [{fun kzd_services:set_bookkeeper_type/2, Type}
               ,{fun kzd_services:set_bookkeeper_vendor_id/2, VendorId}
               ,{fun kzd_services:set_bookkeeper_id/2, Id}
               ],
    kzd_services:bookkeeper(kz_doc:setters(Routines), kz_json:new()).

-spec maybe_fetch_plan(kz_term:ne_binary(), kz_term:ne_binary(), dict:dict()) ->
          {kz_services_plan:plan() | 'undefined', fetched_plans()}.
maybe_fetch_plan(PlanId, VendorId, FetchedPlans) ->
    Key = plan_jobjs_key(VendorId, PlanId),
    case dict:find(Key, FetchedPlans) of
        {'ok', Plan} -> {Plan, FetchedPlans};
        'error' ->
            Plan = kz_services_plan:fetch(VendorId, PlanId),
            {Plan, maybe_append_plan_jobjs(Key, Plan, FetchedPlans)}
    end.

-type plan_jobjs_key() :: {kz_term:ne_binary(), kz_term:ne_binary()}.
-spec maybe_append_plan_jobjs(plan_jobjs_key(), kz_services_plan:plan()|'undefined', fetched_plans()) -> fetched_plans().
maybe_append_plan_jobjs(_Key, 'undefined', FetchedPlans) -> FetchedPlans;
maybe_append_plan_jobjs(Key, Plan, FetchedPlans) ->
    dict:store(Key, Plan, FetchedPlans).

-spec plan_jobjs_key(kz_term:ne_binary(), kz_term:ne_binary()) -> plan_jobjs_key().
plan_jobjs_key(VendorId, PlanId) ->
    {VendorId, PlanId}.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec foldl(fold_fun(), Acc, plans()) -> Acc.
foldl(FoldFun, Acc, Plans) ->
    lists:foldl(fun({BookkeeperHash, PlansList}, A) ->
                        FoldFun(BookkeeperHash, PlansList, A)
                end
               ,Acc
               ,dict:to_list(Plans)
               ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec public_json(plans()) -> kz_json:object().
public_json(Plans) ->
    kz_json:from_list([{BookkeeperHash, kz_services_plan:public_json(merge(PlansList))}
                       || {BookkeeperHash, PlansList} <- dict:to_list(Plans)
                      ]
                     ).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec assigned(kz_services:services()) -> kz_json:object().
assigned(Services) ->
    ServicesJObj = kz_services:services_jobj(Services),
    kzd_services:plans(ServicesJObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec overrides(kz_services:services()) -> kz_json:object().
overrides(Services) ->
    ServicesJObj = kz_services:services_jobj(Services),
    kzd_services:overrides(ServicesJObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec override(kz_services:services(), kz_json:object()) -> kz_services:services().
override(Services, Overrides) ->
    override(Services, Overrides, []).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec override(kz_services:services(), kz_json:object(), kz_term:proplist()) -> kz_services:services().
override(Services, Overrides, Options) ->
    ServicesJObj = kz_services:services_jobj(Services),
    kz_services:set_services_jobj(Services
                                 ,set_or_merge_override(ServicesJObj, Overrides, Options)
                                 ).

-spec set_or_merge_override(kz_json:object(), kz_json:object(), kz_term:proplist()) -> kz_json:object().
set_or_merge_override(ServicesJObj, Overrides, Options) ->
    case props:get_is_true('merge', Options, 'false') of
        'false' -> set_override(ServicesJObj, Overrides);
        'true' -> merge_override(ServicesJObj, Overrides)
    end.

-spec set_override(kz_json:object(), kz_json:object()) -> kz_json:object().
set_override(ServicesJObj, Overrides) ->
    lager:debug("updating overrides via set", []),
    kzd_services:set_overrides(ServicesJObj, Overrides).

-spec merge_override(kz_json:object(), kz_json:object()) -> kz_json:object().
merge_override(ServicesJObj, Overrides) ->
    lager:debug("updating overrides via merge", []),
    Overriden = kz_json:merge([kzd_services:overrides(ServicesJObj), Overrides]),
    kzd_services:set_overrides(ServicesJObj, Overriden).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-type mergable() :: plans() | plans_list().
-spec merge(mergable()) -> kz_services_plan:plan().
merge([Head|Tail]=Plans) ->
    VendorId = kz_services_plan:bookkeeper_vendor_id(Head),
    'true' = lists:all(fun(Plan) ->
                               VendorId =:= kz_services_plan:bookkeeper_vendor_id(Plan)
                       end
                      ,Tail
                      ),
    PlansJObjs = [kz_services_plan:jobj(Plan)
                  || Plan <- Plans
                 ],
    PlanJObj = do_merge(PlansJObjs),
    PlanId = kz_doc:id(PlanJObj),
    kz_services_plan:create(VendorId, PlanId, PlanJObj);
merge(Plans) ->
    PlansList = [Plan
                 || {_, Plan} <- dict:to_list(Plans)
                ],
    merge(PlansList).

-spec do_merge(kz_json:objects()) -> kz_json:object() | kz_json:objects().
do_merge(PlansJObjs) ->
%%% TODO: set _all exceptions to any other keys in the category automatically
    Dict = lists:foldl(fun(PlanJObj, D) ->
                               Strategy = kzd_service_plan:merge_strategy(PlanJObj),
                               Priority = kzd_service_plan:merge_priority(PlanJObj),
                               JObj = kz_doc:public_fields(PlanJObj),
                               dict:append(Strategy, {Priority, JObj}, D)
                       end
                      ,dict:new()
                      ,PlansJObjs
                      ),
    Sorted = lists:sort(fun merge_strategy_sort/2
                       ,dict:to_list(Dict)
                       ),
    merge_to_single(
      merge_plans_by_strategy(Sorted, [])
     ).

-spec merge_to_single(kz_json:objects()) -> kz_json:object().
merge_to_single(PlansJObjs) ->
    merge_to_single(PlansJObjs, kz_json:new()).

-spec merge_to_single(kz_json:objects(), kz_json:object()) -> kz_json:object().
merge_to_single(PlansJObjs, JObj) ->
    PlanJObj = lists:foldl(fun merge_to_single_fold/2, JObj, PlansJObjs),
    kz_json:set_value(<<"_id">>, <<"single">>, PlanJObj).

-spec merge_to_single_fold(kz_json:object(), kz_json:object()) -> kz_json:object().
merge_to_single_fold(JObj, Merged) ->
    kz_json:merge_recursive(Merged, JObj).

-spec merge_strategy_sort(merge_strategy_group(), merge_strategy_group()) -> boolean().
merge_strategy_sort({A, _}, {B, _}) ->
    merge_strategy_priority(A) > merge_strategy_priority(B).

-spec merge_strategy_priority(kz_term:ne_binary()) -> non_neg_integer().
merge_strategy_priority(Strategy) ->
    kz_json:get_integer_value(Strategy, ?MERGE_STRATEGY_PRIORITIES, 0).

-spec merge_plans_by_strategy(merge_strategy_groups(), kz_json:objects()) -> kz_json:objects().
merge_plans_by_strategy([], MergedPlansJObjs) -> MergedPlansJObjs;
merge_plans_by_strategy([{<<"simple">>, Group}|Tail], MergedPlansJObjs) ->
    Sorted = lists:sort(fun merge_plans_sort/2, Group),
    RecursiveMerge = recursive_merge_plans(Sorted),
    Setters = [{fun kz_doc:set_id/2, <<"simple">>}
              ,{fun kzd_service_plan:set_plan/2
               ,kzd_service_plan:plan(simple_merge_plans(Sorted))
               }
              ],
    MergedPlanJObj = kz_doc:setters(RecursiveMerge, Setters),
    merge_plans_by_strategy(Tail, [MergedPlanJObj|MergedPlansJObjs]);
merge_plans_by_strategy([{<<"recursive">>, Group}|Tail], MergedPlansJObjs) ->
    Sorted = lists:sort(fun merge_plans_sort/2, Group),
    JObj = recursive_merge_plans(Sorted),
    Setters = [{fun kz_doc:set_id/2, <<"recursive">>}],
    MergedPlanJObj = kz_doc:setters(JObj, Setters),
    merge_plans_by_strategy(Tail, [MergedPlanJObj|MergedPlansJObjs]);
merge_plans_by_strategy([{<<"cumulative">>, Group}|Tail], MergedPlansJObjs) ->
    Sorted = lists:sort(fun merge_plans_sort/2, Group),
    JObj = recursive_merge_plans(Sorted),
    Props = [{[CategoryName, ItemName], kzd_service_plan:item(PlanJObj, CategoryName, ItemName)}
             || {_, PlanJObj} <- lists:reverse(Sorted)
                    ,CategoryName <- kzd_service_plan:categories(PlanJObj)
                    ,ItemName <- kzd_service_plan:items(PlanJObj, CategoryName)
            ],
    Dict = lists:foldl(fun({Key, Value}, D) ->
                               dict:append(Key, Value, D)
                       end, dict:new(), Props),
    Values = [{cumulative_merge_keys(Root, Key), Fun(Key, JObjs)}
              || {Root, JObjs} <- dict:to_list(Dict)
                     ,{Key, Fun} <- kzd_item_plan:cumulative_merge_scheme()
             ],
    Setters = [{fun kz_doc:set_id/2, <<"cumulative">>}
              ,{fun kzd_service_plan:set_plan/2
               ,kz_json:set_values(Values, kz_json:new())
               }
              ],
    MergedPlanJObj = kz_doc:setters(JObj, Setters),
    merge_plans_by_strategy(Tail, [MergedPlanJObj|MergedPlansJObjs]).

-spec merge_plans_sort(merge_strategy_plan(), merge_strategy_plan()) -> boolean().
merge_plans_sort({A, _}, {B, _}) ->
    A < B.

-spec cumulative_merge_keys(kz_term:ne_binary() | kz_term:ne_binaries()
                           ,kz_term:ne_binary() | kz_term:ne_binaries()
                           ) -> kz_term:ne_binaries().
cumulative_merge_keys(Root, Key) ->
    lists:flatten([Root, Key]).

-spec simple_merge_plans(merge_strategy_plans()) -> kz_json:object().
simple_merge_plans([]) ->
    kz_json:new();
simple_merge_plans([{_, Head}|Tail]) ->
    lists:foldl(fun simple_merge_plans/2, Head, Tail).

-spec simple_merge_plans(merge_strategy_plan(), kz_json:object()) -> kz_json:object().
simple_merge_plans({_, PlanJObj}, Merged) ->
    kz_json:merge(Merged, PlanJObj).

-spec recursive_merge_plans(merge_strategy_plans()) -> kz_json:object().
recursive_merge_plans([]) ->
    kz_json:new();
recursive_merge_plans([{_, Head}|Tail]) ->
    lists:foldl(fun recursive_merge_plans/2, Head, Tail).

-spec recursive_merge_plans(merge_strategy_plan(), kz_json:object()) -> kz_json:object().
recursive_merge_plans({_, PlanJObj}, Merged) ->
    kz_json:merge_recursive(Merged, PlanJObj).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec editable_fields() -> kz_json:object().
editable_fields() ->
    case kapps_util:get_master_account_id() of
        {'ok', ResellerId} -> editable_fields(ResellerId);
        {'error', _Reason} -> editable_fields('undefined')
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec editable_fields(kz_term:api_binary()) -> kz_json:object().
editable_fields(ResellerId) ->
    lager:debug("listing editable fields for ~s", [ResellerId]),
    Routines = [fun editable_phone_numbers_fields/2
               ,fun editable_number_carriers_fields/2
               ,fun editable_number_services_fields/2
               ,fun editable_monster_apps_fields/2
               ,fun editable_qubicle_fields/2
               ],
    editable_fields_to_json(
      lists:foldl(fun(F, Props) ->
                          F(Props, ResellerId)
                  end
                 ,?DEFAULT_QUANTIFIERS
                 ,Routines
                 )
     ).

-spec editable_phone_numbers_fields(kz_term:proplist(), kz_term:api_binary()) -> kz_term:proplist().
editable_phone_numbers_fields(Fields, _ResellerId) ->
    Classifiers = kz_json:get_keys(knm_converters:available_classifiers()),
    case kz_term:is_empty(Classifiers) of
        'true' -> props:delete(<<"phone_numbers">>, Fields);
        'false' -> props:set_value(<<"phone_numbers">>, Classifiers, Fields)
    end.

-spec editable_number_carriers_fields(kz_term:proplist(), kz_term:api_binary()) -> kz_term:proplist().
editable_number_carriers_fields(Fields, ResellerId) ->
    Carriers = [kz_term:to_binary(Carrier)
                || Carrier <- knm_carriers:available_carriers([{'reseller_id', ResellerId}])
               ],
    case kz_term:is_empty(Carriers) of
        'true' -> props:delete(<<"number_carriers">>, Fields);
        'false' -> props:set_value(<<"number_carriers">>, Carriers, Fields)
    end.

-spec editable_number_services_fields(kz_term:proplist(), kz_term:api_binary()) -> kz_term:proplist().
editable_number_services_fields(Fields, ResellerId) ->
    Services = knm_providers:reseller_allowed_features(ResellerId),
    case kz_term:is_empty(Services) of
        'true' -> props:delete(<<"number_services">>, Fields);
        'false' -> props:set_value(<<"number_services">>, Services, Fields)
    end.

-spec editable_monster_apps_fields(kz_term:proplist(), kz_term:api_binary()) -> kz_term:proplist().
editable_monster_apps_fields(Fields, 'undefined') ->
    {'ok', MasterAccountId} = kapps_util:get_master_account_id(),
    editable_monster_apps_fields(Fields, MasterAccountId);
editable_monster_apps_fields(Fields, ResellerId) ->
    case kzd_apps_store:fetch(ResellerId) of
        {'error', _R} -> Fields;
        {'ok', JObj} ->
            Fun = fun(_App, AppJObj, Acc) ->
                          case kzd_app:name(AppJObj) of
                              ?NE_BINARY=Name ->
                                  [Name|Acc];
                              _ -> Acc
                          end
                  end,
            Apps = kz_json:foldl(Fun, [], kzd_apps_store:apps(JObj)),
            [{<<"account_apps">>, Apps}
            ,{<<"user_apps">>, Apps}
             | Fields
            ]
    end.

-spec editable_qubicle_fields(kz_term:proplist(), kz_term:api_binary()) -> kz_term:proplist().
editable_qubicle_fields(Fields, _ResellerId) ->
    case kz_nodes:whapp_count('qubicle') > 0 of
        'true' -> Fields;
        'false' ->
            props:delete(<<"qubicle">>, Fields)
    end.

-spec editable_fields_to_json(kz_term:proplist()) -> kz_json:object().
editable_fields_to_json(Fields) ->
    {'ok', Schema} = kz_datamgr:open_doc(?KZ_SCHEMA_DB, <<"service_plan.item">>),
    Properties = kz_json:get_ne_json_value(<<"properties">>, Schema),
    editable_fields_to_json(Fields, Properties, kz_json:new()).

-spec editable_fields_to_json(kz_term:proplist(), kz_json:object(), kz_json:object()) -> kz_json:object().
editable_fields_to_json([], _Properties, JObj) -> JObj;
editable_fields_to_json([{Category, Items}|Fields], Properties, JObj) ->
    Props = [{[Category, kzd_service_plan:all_items_key()], Properties}
             | [{[Category, Item], kz_json:delete_key(<<"as">>, Properties)}
                || Item <- Items
               ]
            ],
    editable_fields_to_json(Fields, Properties, kz_json:set_values(Props, JObj)).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec is_empty(plans()) -> boolean().
is_empty(Plans) ->
    dict:is_empty(Plans).
