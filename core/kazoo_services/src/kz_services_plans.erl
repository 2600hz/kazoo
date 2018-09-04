%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2012-2018, 2600Hz
%%% @doc
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
-define(ITEM_FIELDS,
        kz_json:from_list(
          [{<<"activation_charge">>, kz_json:new()}
          ,{<<"discounts">>
           ,kz_json:from_list(
              [{<<"maximum">>, kz_json:new()}
              ,{<<"rate">>, kz_json:new()}
              ])
           }
          ,{<<"minimum">>, kz_json:new()}
          ,{<<"rate">>, kz_json:new()}
          ])
       ).
-define(UNDERSCORE_ALL_FIELDS,
        kz_json:set_values([{<<"as">>, kz_json:new()}
                           ,{<<"exceptions">>, kz_json:new()}
                           ]
                          ,?ITEM_FIELDS
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
-spec fetch(kz_services:services()) -> plans().
fetch(Services) ->
    ServicesJObj = kz_services:services_jobj(Services),
    fetch(Services, ServicesJObj).

-spec fetch(kz_services:services(), kz_json:object()) -> plans().
fetch(_Services, ServicesJObj) ->
    lager:debug("fetching service plan documents"),
    Routines = [fun get_services_plan/2
               ,fun get_object_plans/2
               ],
    {_, Plans} =
        lists:foldl(fun(F, FetchContext) ->
                            F(ServicesJObj, FetchContext)
                    end
                   ,{dict:new(), empty()}
                   ,Routines
                   ),
    Plans.

-spec get_services_plan(kz_json:object(), fetch_context()) -> fetch_context().
get_services_plan(ServicesJObj, FetchContext) ->
    lists:foldl(get_service_plans_fold(ServicesJObj)
               ,FetchContext
               ,kzd_services:plan_ids(ServicesJObj)
               ).

-type service_plans_fold() :: fun((kz_term:ne_binary(), fetch_context()) -> fetch_context()).
-spec get_service_plans_fold(kz_json:object()) -> service_plans_fold().
get_service_plans_fold(ServicesJObj) ->
    fun(PlanId, FetchContext) ->
            get_services_plan(PlanId, ServicesJObj, FetchContext)
    end.

-spec get_services_plan(kz_term:ne_binary(), kz_json:object(), fetch_context()) -> fetch_context().
get_services_plan(PlanId, ServicesJObj, FetchContext) ->
    VendorId = kzd_services:plan_vendor_id(ServicesJObj
                                          ,PlanId
                                          ,default_plan_vendor_id(ServicesJObj)
                                          ),
    Overrides = get_services_plan_overrides(ServicesJObj, PlanId),
    maybe_append_plan(PlanId, VendorId, Overrides, FetchContext).

-spec get_services_plan_overrides(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
get_services_plan_overrides(ServicesJObj, PlanId) ->
    PlansOverrides = kzd_services:overrides(ServicesJObj),
    PlanOverrides = kzd_services:plan_overrides(ServicesJObj, PlanId),
    kz_json:merge_recursive(PlansOverrides, PlanOverrides).

-spec get_object_plans(kz_json:object(), fetch_context()) -> fetch_context().
get_object_plans(ServicesJObj, FetchContext) ->
    AccountId = kz_doc:id(ServicesJObj),
    AccountDb = kz_util:format_account_db(AccountId),
    case kz_datamgr:get_results(AccountDb, <<"services/object_plans">>) of
        {'error', _Reason} ->
            lager:info("unable to list object plans: ~p", [_Reason]),
            FetchContext;
        {'ok', ObjectPlans} ->
            lager:debug("found ~p references to object plans", [length(ObjectPlans)]),
            build_object_plan(ServicesJObj, FetchContext, ObjectPlans)
    end.

-spec build_object_plan(kz_json:object(), fetch_context(), kz_json:objects()) -> fetch_context().
build_object_plan(ServicesJObj, FetchContext, ObjectPlans) ->
    Props = [{PlanId, kz_json:get_value([<<"value">>, PlanId], ObjectPlan)}
             || ObjectPlan <- ObjectPlans
                    ,PlanId <- kz_json:get_keys(<<"value">>, ObjectPlan)
            ],
    DefaultPlanVendorId = default_plan_vendor_id(ServicesJObj),
    lists:foldl(get_object_plans_fold(DefaultPlanVendorId)
               ,FetchContext
               ,Props
               ).

-type object_plans_fold() :: fun(({kz_term:ne_binary(), kz_json:object()}, fetch_context()) -> fetch_context()).
-spec get_object_plans_fold(kz_term:api_binary()) -> object_plans_fold().
get_object_plans_fold(DefaultPlanVendorId) ->
    fun({PlanId, JObj}, FetchContext) ->
            get_object_plan(PlanId, DefaultPlanVendorId, JObj, FetchContext)
    end.

-spec get_object_plan(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), fetch_context()) -> fetch_context().
get_object_plan(PlanId, DefaultPlanVendorId, JObj, FetchContext) ->
    VendorId = kz_json:get_ne_value(<<"vendor_id">>, JObj, DefaultPlanVendorId),
    Overrides = kz_json:get_ne_value(<<"overrides">>, JObj, kz_json:new()),
    maybe_append_plan(PlanId, VendorId, Overrides, FetchContext).

-spec maybe_append_plan(kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), fetch_context()) -> fetch_context().
maybe_append_plan(PlanId, VendorId, Overrides, {FetchedPlans, ServicePlans}) ->
    case maybe_fetch_plan(PlanId, VendorId, FetchedPlans) of
        {'undefined', _} -> {FetchedPlans, ServicePlans};
        {Plan, UpdatedFetchedPlans} ->
            UpdatedPlan = kz_services_plan:set_overrides(Plan, Overrides),
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

-spec default_plan_vendor_id(kzd_services:doc()) -> kz_term:api_ne_binary().
default_plan_vendor_id(ServicesJObj) ->
    case kzd_services:reseller_id(ServicesJObj) of
        'undefined' -> kz_json:get_ne_binary_value(<<"reseller_id">>, ServicesJObj);
        ResellerId -> ResellerId
    end.

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
-spec override(kz_services:services(), kz_json:object()) -> kz_serivces:services().
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
merge([Head|Tail]=Plans) when is_list(Plans) ->
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
                       end, dict:new(), PlansJObjs),
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
    kz_json:merge(Merged, JObj).

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

-spec simple_merge_plans(merge_strategy_plans()) -> kz_json:objcet().
simple_merge_plans([]) ->
    kz_json:new();
simple_merge_plans([{_, Head}|Tail]) ->
    lists:foldl(fun simple_merge_plans/2, Head, Tail).

-spec simple_merge_plans(merge_strategy_plan(), kz_json:object()) -> kz_json:object().
simple_merge_plans({_, PlanJObj}, Merged) ->
    kz_json:merge(Merged, PlanJObj).

-spec recursive_merge_plans(merge_strategy_plans()) -> kz_json:objcet().
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
    JObj = read_service_plan_editable(),
    UIApps = kz_json:from_list(get_ui_apps(ResellerId)),
    kz_json:set_value(<<"ui_apps">>, UIApps, JObj).

-spec read_service_plan_editable() -> kz_json:object().
read_service_plan_editable() ->
    Path = filename:join([code:priv_dir(?APP), "service_plan_editable_fields.json"]),
    case file:read_file(Path) of
        {'ok', Bin} -> kz_json:decode(Bin);
        {'error', _Reason} ->
            lager:debug("failed to read file ~s: ~p", [Path, _Reason]),
            kz_json:new()
    end.

-spec get_ui_apps(kz_term:api_binary()) -> kz_term:proplist().
get_ui_apps('undefined') ->
    [{<<"_all">>, ?UNDERSCORE_ALL_FIELDS}];
get_ui_apps(ResellerId) ->
    case kzd_apps_store:fetch(ResellerId) of
        {'ok', JObj} ->
            Apps = kzd_apps_store:apps(JObj),
            Fun = fun(_App, AppJObj, Acc) ->
                          case kzd_app:name(AppJObj) of
                              ?NE_BINARY=Name ->
                                  [{Name, ?ITEM_FIELDS}|Acc];
                              _ -> Acc
                          end
                  end,
            kz_json:foldl(Fun, [{<<"_all">>, ?UNDERSCORE_ALL_FIELDS}], Apps);
        {'error', _Reason} ->
            lager:debug("failed to read master's app_store: ~p", [_Reason]),
            [{<<"_all">>, ?UNDERSCORE_ALL_FIELDS}]
    end.
