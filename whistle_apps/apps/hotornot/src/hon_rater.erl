%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP, INC
%%% @doc
%%% Given a rate_req, find appropriate rate for the call
%%% @end
%%% Created : 16 Aug 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(hon_rater).

-export([init/0, handle_req/2]).

-include("hotornot.hrl").

init() ->
    couch_mgr:db_create(?WH_RATES_DB),
    couch_mgr:load_doc_from_file(?WH_RATES_DB, hotornot, <<"fixtures/us-1.json">>),
    couch_mgr:revise_doc_from_file(?WH_RATES_DB, crossbar, <<"views/rates.json">>). %% only load it (will fail if exists)

-spec handle_req/2 :: (wh_json:json_object(), proplist()) -> 'ok'.
handle_req(JObj, Props) ->
    true = wapi_call:rate_req_v(JObj),
    ?LOG("Valid rating request"),

    {ok, RatesData} = get_rate_data(JObj),
    ?LOG("Rate data retrieved"),

    RespProp = [{<<"Rates">>, RatesData}
                | wh_api:default_headers(props:get_value(queue, Props, <<>>), ?APP_NAME, ?APP_VERSION)
               ],

    spawn( fun() -> set_rate_ccvs(RespProp, wh_json:get_value(<<"Control-Queue">>, JObj), JObj) end),

    wapi_call:publish_rate_resp(wh_json:get_value(<<"Server-ID">>, JObj), RespProp).

-spec get_rate_data/1 :: (wh_json:json_object()) -> {'ok', wh_json:json_objects()} | {'error', 'no_rate_found'}.
get_rate_data(JObj) ->
    ToDID = wh_json:get_value(<<"To-DID">>, JObj),
    FromDID = wh_json:get_value(<<"From-DID">>, JObj),

    case hon_util:candidate_rates(ToDID, FromDID) of
        {ok, []} -> ?LOG("rate lookup had no results"), {error, no_rate_found};
        {error, _E} -> ?LOG("rate lookup error: ~p", [_E]), {error, no_rate_found};
        {ok, Rates} ->
            RouteOptions = wh_json:get_value(<<"Options">>, JObj, []),
            Direction = wh_json:get_value(<<"Direction">>, JObj),

            Matching = hon_util:matching_rates(Rates, ToDID, Direction, RouteOptions),
            case hon_util:sort_rates(Matching) of
                [] -> ?LOG("no rates left after filter"), {error, no_rate_found};
                [_|_]=SortedRates ->
                    {ok, [rate_to_json(Rate) || Rate <- SortedRates]}
            end
    end.

-spec rate_to_json/1 :: (wh_json:json_object()) -> wh_json:json_object().
rate_to_json(Rate) ->
    ?LOG("using rate definition ~s", [wh_json:get_value(<<"rate_name">>, Rate)]),

    BaseCost = wapi_money:base_call_cost(wh_json:get_float_value(<<"rate_cost">>, Rate, 0.01)
                                         ,wh_json:get_integer_value(<<"rate_minimum">>, Rate, 60)
                                         ,wh_json:get_float_value(<<"rate_surcharge">>, Rate, 0.0)),

    ?LOG("base cost for a minute call: ~p", [BaseCost]),

    wh_json:from_list([{<<"Rate">>, wh_json:get_binary_value(<<"rate_cost">>, Rate)}
                       ,{<<"Rate-Increment">>, wh_json:get_binary_value(<<"rate_increment">>, Rate)}
                       ,{<<"Rate-Minimum">>, wh_json:get_binary_value(<<"rate_minimum">>, Rate)}
                       ,{<<"Surcharge">>, wh_json:get_binary_value(<<"rate_surcharge">>, Rate)}
                       ,{<<"Rate-Name">>, wh_json:get_binary_value(<<"rate_name">>, Rate)}
                       ,{<<"Base-Cost">>, wh_util:to_binary(BaseCost)}
                      ]).

-spec set_rate_ccvs/3 :: (proplist(), 'undefined' | ne_binary(), wh_json:json_object()) -> ok.
set_rate_ccvs(_, undefined, _) -> ok;
set_rate_ccvs(Response, CtrlQ, JObj) ->    
    case props:get_value(<<"Rates">>, Response) of
        [] ->
            ToDID = wnm_util:to_e164(wh_json:get_value(<<"To-DID">>, JObj)),
            ?LOG(notice, "rate information unavailable for ~s", [ToDID]);
        [RateInfoJObj | _] ->                    
            Command = [{<<"Application-Name">>, <<"set">>}
                       ,{<<"Call-ID">>, wh_json:get_value(<<"Call-ID">>, JObj)}
                       ,{<<"Custom-Call-Vars">>, wh_json:new()}
                       ,{<<"Custom-Channel-Vars">>, RateInfoJObj}
                       ,{<<"Insert-At">>, <<"now">>}
                       | wh_api:default_headers(<<>>, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
                      ],
            wapi_dialplan:publish_command(CtrlQ, Command),
            ?LOG("set rate information on call channel")
    end.
