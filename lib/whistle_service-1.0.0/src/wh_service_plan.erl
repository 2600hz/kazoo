%%%-------------------------------------------------------------------
%%% @copyright (C) 2012, VoIP INC
%%% @doc
%%% 
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(wh_service_plan).

-export([fetch/2]).
-export([get_plan_ids/1]).
-export([add_plan_id/3]).
-export([set_service_plans/3]).
-export([is_valid_plan_id/2]).
-export([get_category_addons/2]).
-export([get_recurring_plan/3]).
-export([get_activation_charge/3]).

-include("wh_service.hrl").

-record(wh_service_plan, {id = 'undefined' :: 'undefined' | ne_binary()
                          ,reseller = 'undefined' :: 'undefined' | ne_binary()
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
fetch(Reseller, PlanId) ->
    lager:debug("fetching service plan ~s from reseller ~s", [PlanId, Reseller]),
    ResellerDb = wh_util:format_account_id(Reseller, encoded),    
    case couch_mgr:open_cache_doc(ResellerDb, PlanId) of
        {ok, JObj} ->
            {ok, #wh_service_plan{id=PlanId, reseller=Reseller, plan=JObj}};
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
get_category_addons(Category, #wh_service_plan{plan=JObj}) ->
    Plan = wh_json:get_value(Category, JObj, wh_json:new()),
    [{PlanId, AddOnId}
     || Key <- wh_json:get_keys(Plan)
            ,(AddOnId = wh_json:get_ne_value([Key, <<"add_on">>], Plan)) =/= undefined
            ,(PlanId = wh_json:get_ne_value([Key, <<"plan">>], Plan)) =/= undefined
    ].
    
%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find the subscription name on a service plan for a given service 
%% element
%% @end
%%--------------------------------------------------------------------
-spec get_recurring_plan/3 :: (ne_binary(), ne_binary(), plan()) -> 'undefined' | {ne_binary(), ne_binary()}. 
get_recurring_plan(<<"phone_numbers">>, PhoneNumber, #wh_service_plan{plan=JObj}) ->
    case wh_json:get_value(<<"phone_numbers">>, JObj) of
        undefined -> undefined;
        PhoneNumbers ->
            Regexs = wh_json:get_keys(PhoneNumbers),
            case [{PlanId, AddOnId}
                  || Regex <- Regexs
                         ,re:run(PhoneNumber, Regex) =/= nomatch
                         ,(PlanId = wh_json:get_value([<<"phone_numbers">>, Regex, <<"plan">>], JObj)) =/= undefined
                         ,(AddOnId = wh_json:get_value([<<"phone_numbers">>, Regex, <<"add_on">>], JObj)) =/= undefined
                 ]
            of
                [] -> undefined;
                [{P, A}=Recurring|_] -> 
                    lager:debug("found plan ~s addon ~s for phone number ~s", [P, A, PhoneNumber]),
                    Recurring
            end
    end;
get_recurring_plan(Category, Name, #wh_service_plan{plan=JObj}) ->
    PlanId = wh_json:get_value([Category, Name, <<"plan">>], JObj),
    AddOnId = wh_json:get_value([Category, Name, <<"add_on">>], JObj),
    case wh_util:is_empty(PlanId) orelse wh_util:is_empty(AddOnId) of
        true -> undefined;
        false ->  {PlanId, AddOnId}
    end.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find the subscription name on a service plan for a given service 
%% element
%% @end
%%--------------------------------------------------------------------
-spec get_activation_charge/3 :: (ne_binary(), ne_binary(), plan()) -> 'undefined' | {ne_binary(), ne_binary()}. 
get_activation_charge(<<"phone_numbers">>, PhoneNumber, #wh_service_plan{plan=JObj}) ->
    case wh_json:get_value(<<"phone_numbers">>, JObj) of
        undefined -> undefined;
        PhoneNumbers ->
            Regexs = wh_json:get_keys(PhoneNumbers),
            case [Amount
                  || Regex <- Regexs
                         ,re:run(PhoneNumber, Regex) =/= nomatch
                         ,(Amount = wh_json:get_value([<<"phone_numbers">>, Regex, <<"activation_charge">>], JObj)) =/= undefined
                 ]
            of
                [] -> undefined;
                [Amount|_] -> 
                    lager:debug("found activation charge $~s for phone number ~s", [Amount, PhoneNumber]),
                    Amount
            end
    end;
get_activation_charge(Category, Name, #wh_service_plan{plan=JObj}) ->
    case wh_json:get_ne_value([Category, Name, <<"activation_charge">>], JObj) of
        undefined -> undefined;
        Amount ->
            lager:debug("found activation charge $~s for ~s ~s", [Amount, Category, Name]),
            Amount
    end.
             
