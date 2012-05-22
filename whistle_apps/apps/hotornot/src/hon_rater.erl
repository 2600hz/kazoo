%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP, INC
%%% @doc
%%% Given a rate_req, find appropriate rate for the call
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(hon_rater).

-export([init/0, handle_req/2]).

-include("hotornot.hrl").

init() ->
    couch_mgr:db_create(?WH_RATES_DB),
    couch_mgr:load_doc_from_file(?WH_RATES_DB, hotornot, <<"fixtures/us-1.json">>),
    couch_mgr:revise_doc_from_file(?WH_RATES_DB, crossbar, <<"views/rates.json">>). %% only load it (will fail if exists)

-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    true = wapi_rate:req_v(JObj),
    wh_util:put_callid(JObj),
    lager:debug("valid rating request"),
    case get_rate_data(JObj) of
        {error, no_rate_found} -> ok;
        {ok, Resp} ->
            wapi_rate:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj), Resp)
    end.

-spec get_rate_data/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_objects()} |
                                                    {'error', 'no_rate_found'}.
get_rate_data(JObj) ->
    ToDID = wh_json:get_value(<<"To-DID">>, JObj),
    FromDID = wh_json:get_value(<<"From-DID">>, JObj),
    case hon_util:candidate_rates(ToDID, FromDID) of
        {ok, []} -> 
            wh_notify:system_alert("no rate found for ~s to ~s", [FromDID, ToDID]),
            lager:debug("rate lookup had no results"), {error, no_rate_found};
        {error, _E} -> 
            wh_notify:system_alert("no rate found for ~s to ~s", [FromDID, ToDID]),
            lager:debug("rate lookup error: ~p", [_E]), {error, no_rate_found};
        {ok, Rates} ->
            RouteOptions = wh_json:get_value(<<"Options">>, JObj, []),
            Direction = wh_json:get_value(<<"Direction">>, JObj),
            Matching = hon_util:matching_rates(Rates, ToDID, Direction, RouteOptions),
            case hon_util:sort_rates(Matching) of
                [] -> 
                    lager:debug("no rates left after filter"),
                    {error, no_rate_found};
                [Rate|_] ->
                    {ok, rate_resp(Rate, JObj)}
            end
    end.

-spec rate_resp/2 :: (wh_json:json_object(), wh_json:json_object()) -> proplist().
rate_resp(Rate, JObj) ->
    lager:debug("using rate definition ~s", [wh_json:get_value(<<"rate_name">>, Rate)]),
    BaseCost = wapi_money:base_call_cost(wh_json:get_float_value(<<"rate_cost">>, Rate, 0.01)
                                         ,wh_json:get_integer_value(<<"rate_minimum">>, Rate, 60)
                                         ,wh_json:get_float_value(<<"rate_surcharge">>, Rate, 0.0)),

    lager:debug("base cost for a minute call: ~p", [BaseCost]),
    [{<<"Rate">>, wh_json:get_binary_value(<<"rate_cost">>, Rate)}
     ,{<<"Rate-Increment">>, wh_json:get_binary_value(<<"rate_increment">>, Rate)}
     ,{<<"Rate-Minimum">>, wh_json:get_binary_value(<<"rate_minimum">>, Rate)}
     ,{<<"Surcharge">>, wh_json:get_binary_value(<<"rate_surcharge">>, Rate)}
     ,{<<"Rate-Name">>, wh_json:get_binary_value(<<"rate_name">>, Rate)}
     ,{<<"Base-Cost">>, wh_util:to_binary(BaseCost)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
     ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].
