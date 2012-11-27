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

init() -> whapps_maintenance:refresh(?WH_RATES_DB).

-spec handle_req/2 :: (wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    true = wapi_rate:req_v(JObj),
    _ = wh_util:put_callid(JObj),
    lager:debug("valid rating request"),
    case get_rate_data(JObj) of
        {error, no_rate_found} -> ok;
        {ok, Resp} ->
            wapi_rate:publish_resp(wh_json:get_value(<<"Server-ID">>, JObj)
                                   ,props:filter_undefined(Resp)
                                  )
    end.

-spec get_rate_data/1 :: (wh_json:object()) ->
                                 {'ok', wh_proplist()} |
                                 {'error', 'no_rate_found'}.
get_rate_data(JObj) ->
    ToDID = wh_json:get_value(<<"To-DID">>, JObj),
    FromDID = wh_json:get_value(<<"From-DID">>, JObj),
    case hon_util:candidate_rates(ToDID, FromDID) of
        {ok, []} -> 
            wh_notify:system_alert("no rate found for ~s to ~s", [FromDID, ToDID]),
            lager:debug("rate lookup had no results"),
            {error, no_rate_found};
        {error, _E} -> 
            wh_notify:system_alert("no rate found for ~s to ~s", [FromDID, ToDID]),
            lager:debug("rate lookup error: ~p", [_E]),
            {error, no_rate_found};
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

-spec maybe_get_rate_discount/1 :: (wh_json:object()) -> api_binary().
maybe_get_rate_discount(JObj) ->
    AccountId = wh_json:get_value(<<"Account-ID">>, JObj),
    AccountDb = wh_util:format_account_id(AccountId, encoded),
    case couch_mgr:open_cache_doc(AccountDb, <<"limits">>) of
        {error, _R} ->
            lager:debug("unable to open account ~s definition: ~p", [AccountId, _R]),
            undefined;
        {ok, Def} ->
            Number = wh_json:get_value(<<"To-DID">>, JObj),
            Classification = wnm_util:classify_number(Number),
            lager:debug("~s number discount percentage: ~p", [Classification, Def]),
            wh_json:get_value([<<"pvt_discounts">>, Classification, <<"percentage">>], Def)
    end.

-spec rate_resp/2 :: (wh_json:object(), wh_json:object()) -> wh_proplist().
rate_resp(Rate, JObj) ->
    lager:debug("using rate definition ~s", [wh_json:get_value(<<"rate_name">>, Rate)]),
    BaseCost = wapi_money:base_call_cost(wh_json:get_float_value(<<"rate_cost">>, Rate, 0.01)
                                         ,wh_json:get_integer_value(<<"rate_minimum">>, Rate, 60)
                                         ,wh_json:get_float_value(<<"rate_surcharge">>, Rate, 0.0)),

    lager:debug("base cost for a minute call: ~p", [BaseCost]),
    [{<<"Rate">>, wh_json:get_binary_value(<<"rate_cost">>, Rate)}
     ,{<<"Rate-Increment">>, wh_json:get_binary_value(<<"rate_increment">>, Rate)}
     ,{<<"Rate-Minimum">>, wh_json:get_binary_value(<<"rate_minimum">>, Rate)}
     ,{<<"Discount-Percentage">>, maybe_get_rate_discount(JObj)}
     ,{<<"Surcharge">>, wh_json:get_binary_value(<<"rate_surcharge">>, Rate)}
     ,{<<"Rate-Name">>, wh_json:get_binary_value(<<"rate_name">>, Rate)}
     ,{<<"Rate-ID">>, wh_json:get_value(<<"_id">>, Rate)}
     ,{<<"Base-Cost">>, wh_util:to_binary(BaseCost)}
     ,{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
     ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
     | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
    ].
