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
-export([get_plan_id/3]).
-export([get_addon_id/3]).

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
%% Find the subscription name on a service plan for a given service 
%% element
%% @end
%%--------------------------------------------------------------------
-spec get_plan_id/3 :: (ne_binary(), ne_binary(), plan()) -> 'undefined' | ne_binary(). 
get_plan_id(Category, Name, #wh_service_plan{plan=JObj}) ->
    wh_json:get_value([Category, Name, <<"plan">>], JObj).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Find the add on name on a service plan for a given service 
%% element
%% @end
%%--------------------------------------------------------------------
-spec get_addon_id/3 :: (ne_binary(), ne_binary(), plan()) -> 'undefined' | ne_binary().
get_addon_id(Category, Name, #wh_service_plan{plan=JObj}) ->
    wh_json:get_value([Category, Name, <<"add_on">>], JObj).
