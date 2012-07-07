%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_plan).

-export([fetch/2, fetch/3]).
-export([get_id/1]).
-export([get_plan_ids/1]).
-export([add_plan_id/3]).
-export([set_service_plans/3]).
-export([is_valid_plan_id/2]).
-export([get_category_addons/2]).
-export([get_recurring_plan/3]).
-export([get_activation_charge/3]).
-export([get_base_mrc/1]).
-export([get_item/3]).

-include("wh_service.hrl").

-record(wh_service_plan, {id = 'undefined' :: 'undefined' | ne_binary()
                          ,reseller = 'undefined' :: 'undefined' | ne_binary()
                          ,cascade_only = false :: boolean()
                          ,plan = wh_json:new() :: wh_json:json_object()
                         }).

-type(plan() :: #wh_service_plan{}).
-export_type([plan/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a service plan id and a reseller attempt to fetch the plan
%% drawing from the common cache if possible.
%% @end
%%--------------------------------------------------------------------
-spec fetch/2 :: (ne_binary(), ne_binary()) -> {'ok', plan()} | {'error', _}.
-spec fetch/3 :: (ne_binary(), boolean(), ne_binary()) -> {'ok', plan()} | {'error', _}.

fetch(Reseller, PlanId) ->
    fetch(Reseller, false, PlanId).

fetch(Reseller, CascadeOnly, PlanId) ->
    lager:debug("fetching service plan ~s from reseller ~s (cascades only ~s)", [PlanId, Reseller, CascadeOnly]),
    ResellerDb = wh_util:format_account_id(Reseller, encoded),    
    case couch_mgr:open_cache_doc(ResellerDb, PlanId) of
        {ok, JObj} ->
            {ok, #wh_service_plan{id=PlanId, reseller=Reseller, plan=JObj, cascade_only=CascadeOnly}};
        {error, _R}=E ->
            lager:debug("unable to open reseller ~s service plan ~s: ~p", [Reseller, PlanId, _R]),
            E
    end.
            
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an account definition retreive the service plan ids off of
%% it.
%% @end
%%--------------------------------------------------------------------
-spec get_plan_ids/1 :: (wh_json:json_object()) -> [ne_binary(),...] | [].
get_plan_ids(JObj) ->
    wh_json:get_value(?WH_SERVICE_PLANS_FIELD, JObj, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given a service plan retreive the id
%% @end
%%--------------------------------------------------------------------
-spec get_id/1 :: (#wh_service_plan{}) -> ne_binary().
get_id(#wh_service_plan{id=Id}) -> Id.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec add_plan_id/3 :: (wh_json:json_object(), ne_binary(), ne_binary()) -> wh_json:json_object().
add_plan_id(JObj, PlanId, Reseller) ->
    case is_valid_plan_id(PlanId, Reseller) of
        false -> JObj;
        true ->
            lager:debug("adding service plan id ~s", [PlanId]),
            Plans = [PlanId|lists:delete(PlanId, get_plan_ids(JObj))],
            wh_json:set_value(?WH_SERVICE_PLANS_FIELD, Plans, JObj)
    end.


%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec set_service_plans/3 :: (wh_json:json_object(), 'undefined' | ne_binary() | [ne_binary(),...], ne_binary()) 
                            -> wh_json:json_object().
set_service_plans(JObj, undefined, Reseller) ->
    case wh_reseller:get_default_service_plan(Reseller) of
        undefined -> JObj;
        ServicePlan -> set_service_plans(JObj, [ServicePlan], Reseller)
    end;
set_service_plans(JObj, ServicePlan, Reseller) when not is_list(ServicePlan) ->
    set_service_plans(JObj, [ServicePlan], Reseller);
set_service_plans(JObj, ServicePlans, Reseller) ->
    lists:foldl(fun(P, J) -> add_plan_id(J, P, Reseller) end, JObj, ServicePlans).

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec is_valid_plan_id/2 :: (ne_binary(), ne_binary()) -> boolean().
is_valid_plan_id(PlanId, Reseller) ->
    case fetch(Reseller, PlanId) of
        {ok, _} -> true;
        {error, _} -> false
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Given an service category get a listing of all the addons
%% @end
%%--------------------------------------------------------------------
-spec get_category_addons/2 :: (ne_binary(), #wh_service_plan{}) -> [{ne_binary(), ne_binary()},...] | [].
get_category_addons(Category, #wh_service_plan{plan=JObj, cascade_only=CascadeOnly}) ->
    Plan = wh_json:get_value(Category, JObj, wh_json:new()),
    [{PlanId, AddOnId}
     || Key <- wh_json:get_keys(Plan)
            ,(AddOnId = wh_json:get_ne_value([Key, <<"add_on">>], Plan)) =/= undefined
            ,(PlanId = wh_json:get_ne_value([Key, <<"plan">>], Plan)) =/= undefined
            ,((not CascadeOnly) orelse wh_json:is_true([Key, <<"cascade">>], Plan))
    ].
    
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find the subscription name on a service plan for a given service 
%% element
%% @end
%%--------------------------------------------------------------------
-spec get_recurring_plan/3 :: (ne_binary(), ne_binary(), plan()) -> 'undefined' | {ne_binary(), ne_binary()}. 
get_recurring_plan(Category, Name, Plan) ->
    case get_item(Category, Name, Plan) of
        undefined -> undefined;
        Item -> 
            PlanId = wh_json:get_value(<<"plan">>, Item),
            AddOnId = wh_json:get_value(<<"add_on">>, Item),
            case wh_util:is_empty(PlanId) orelse wh_util:is_empty(AddOnId) of
                true -> undefined;
                false ->  {PlanId, AddOnId}
            end
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find the subscription name on a service plan for a given service 
%% element
%% @end
%%--------------------------------------------------------------------
-spec get_activation_charge/3 :: (ne_binary(), ne_binary(), plan()) -> 'undefined' | ne_binary(). 
get_activation_charge(Category, Name, Plan) ->
    case get_item(Category, Name, Plan) of
        undefined -> undefined;
        Item -> wh_json:get_ne_value(<<"activation_charge">>, Item)
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec get_base_mrc/1 :: (#wh_service_plan{}) -> wh_json:json_objects().
get_base_mrc(#wh_service_plan{plan=JObj}) ->
    wh_json:get_value(<<"base_mrc">>, JObj, []).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find the subscription name on a service plan for a given service 
%% element
%% @end
%%--------------------------------------------------------------------
-spec get_item/3 :: (ne_binary(), ne_binary(), #wh_service_plan{}) -> 'undefined' | wh_json:json_object().
get_item(<<"phone_numbers">>, PhoneNumber, #wh_service_plan{plan=JObj, cascade_only=CascadeOnly}) ->
    case wh_json:get_value(<<"phone_numbers">>, JObj) of
        undefined -> undefined;
        PhoneNumbers ->
            Regexs = wh_json:get_keys(PhoneNumbers),
            case [wh_json:get_value(Regex, PhoneNumbers)
                  || Regex <- Regexs
                         ,re:run(PhoneNumber, Regex) =/= nomatch
                 ]
            of
                [] -> undefined;
                [Item|_] ->
                    case (not CascadeOnly) orelse wh_json:is_true(<<"cascade">>, Item) of
                        false -> 
                            lager:debug("ignoring none cascade phone_number ~s", [PhoneNumber]),
                            undefined;
                        true ->
                            wh_json:set_values([{<<"category">>, <<"phone_numbers">>}
                                                ,{<<"item">>, PhoneNumber}
                                                ,{<<"id">>, wh_json:get_value(<<"_id">>, JObj)}
                                               ], Item)
                    end
            end
    end;
get_item(Category, Name, #wh_service_plan{plan=JObj, cascade_only=CascadeOnly}) ->
    case wh_json:get_ne_value([Category, Name], JObj) of
        undefined -> undefined;
        Item -> 
            case (not CascadeOnly) orelse wh_json:is_true(<<"cascade">>, Item) of
                false -> 
                    lager:debug("ignoring none cascade item ~s ~s", [Category, Name]),
                    undefined;
                true ->
                    wh_json:set_values([{<<"category">>, Category}
                                        ,{<<"item">>, Name}
                                        ,{<<"id">>, wh_json:get_value(<<"_id">>, JObj)}
                                       ], Item)
            end
    end.
