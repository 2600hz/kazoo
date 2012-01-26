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

-spec get_rate_data/1 :: (wh_json:json_object()) -> {'ok', proplist()} | {'error', 'no_rate_found'}.
get_rate_data(JObj) ->
    ToDID = wnm_util:to_e164(wh_json:get_value(<<"To-DID">>, JObj)),
    _FromDID = wnm_util:to_e164(wh_json:get_value(<<"From-DID">>, JObj)),
    RouteOptions = wh_json:get_value(<<"Options">>, JObj, []),
    Direction = wh_json:get_value(<<"Direction">>, JObj),
    <<"+", Start:1/binary, Rest/binary>> = ToDID,
    End = <<Start/binary, Rest/binary>>,

    ?LOG("searching for rates in the range ~s to ~s", [Start, End]),
    case couch_mgr:get_results(?WH_RATES_DB, <<"rates/lookup">>, [{<<"startkey">>, wh_util:to_integer(Start)}
                                                                  ,{<<"endkey">>, wh_util:to_integer(End)}
                                                                 ]) of
        {ok, []} -> ?LOG("rate lookup had no results"), {error, no_rate_found};
        {error, _E} -> ?LOG("rate lookup error: ~p", [_E]), {error, no_rate_found};
        {ok, Rates} ->
            Matching = filter_rates(ToDID, Direction, RouteOptions, Rates),
            case lists:usort(fun sort_rates/2, Matching) of
                [] -> ?LOG("no rates left after filter"), {error, no_rate_found};
                [_|_]=SortedRates ->
                    %% wh_timer:tick("post usort data found"),

                    {ok, [rate_to_json(Rate) || Rate <- SortedRates]}
            end
    end.

-spec rate_to_json/1 :: (wh_json:json_object()) -> wh_json:json_object().
rate_to_json(Rate) ->
    ?LOG("using rate definition ~s", [wh_json:get_value(<<"rate_name">>, Rate)]),

    BaseCost = wh_json:get_float_value(<<"rate_cost">>, Rate, 0.01) * ( wh_json:get_integer_value(<<"rate_minimum">>, Rate, 60) div 60 )
        + wh_json:get_float_value(<<"rate_surcharge">>, Rate, 0.0),

    ?LOG("base cost for a minute call: ~p", [BaseCost]),

    wh_json:from_list([{<<"Rate">>, wh_json:get_binary_value(<<"rate_cost">>, Rate)}
                       ,{<<"Rate-Increment">>, wh_json:get_binary_value(<<"rate_increment">>, Rate)}
                       ,{<<"Rate-Minimum">>, wh_json:get_binary_value(<<"rate_minimum">>, Rate)}
                       ,{<<"Surcharge">>, wh_json:get_binary_value(<<"rate_surcharge">>, Rate)}
                       ,{<<"Rate-Name">>, wh_json:get_binary_value(<<"rate_name">>, Rate)}
                       ,{<<"Base-Cost">>, wh_util:to_binary(BaseCost)}
                      ]).


filter_rates(To, Direction, RouteOptions, Rates) ->
    [ begin
          Rate = wh_json:get_value(<<"value">>, R),
          wh_json:set_value(<<"rate_name">>, wh_json:get_value(<<"id">>, R), Rate)
      end || R <- Rates, matching_rate(To, Direction, RouteOptions, R)].

matching_rate(To, Direction, RouteOptions, Rate) ->
    %% need to match direction and options at some point too
    Routes = wh_json:get_value([<<"value">>, <<"routes">>], Rate),

    lists:member(Direction, wh_json:get_value([<<"value">>, <<"direction">>], Rate, [])) andalso
        options_match(RouteOptions, wh_json:get_value([<<"value">>, <<"options">>], Rate, [])) andalso
        lists:any(fun(Regex) -> re:run(To, Regex) =/= nomatch end, Routes).

%% Return true of RateA has higher weight than RateB
sort_rates(RateA, RateB) ->
    ts_util:constrain_weight(wh_json:get_value(<<"weight">>, RateA, 1)) >= ts_util:constrain_weight(wh_json:get_value(<<"weight">>, RateB, 1)).

%% Route options come from the client device
%% Rate options come from the carrier providing the trunk
%% All Route options must exist in a carrier's options to keep the carrier
%% in the list of carriers capable of handling the call
-type trunking_options() :: [ne_binary(),...] | [].
-spec options_match/2 :: (trunking_options(), trunking_options()) -> boolean().
options_match([], []) -> true;
options_match([], _) -> true;
options_match(RouteOptions, RateOptions) ->
    lists:all(fun(Opt) -> props:get_value(Opt, RateOptions, false) =/= false end, RouteOptions).

-spec set_rate_ccvs/3 :: (proplist(), 'undefined' | ne_binary(), wh_json:json_object()) -> ok.
set_rate_ccvs(_, undefined, _) -> ok;
set_rate_ccvs(Response, CtrlQ, JObj) ->    
    case props:get_value(<<"Rates">>, Response) of
        [] ->
            ToDID = wnm_util:to_e164(wh_json:get_value(<<"To-DID">>, JObj)),
            whapps_util:alert(<<"error">>, ["Source: ~s(~b)~n"
                                            ,"Alert: rate information unavailable for ~s~n"
                                            ,"Call-ID: ~s~n"]
                              ,[?MODULE, ?LINE, ToDID, wh_json:get_value(<<"Call-ID">>, JObj)]),
            ?LOG("no rates found for ~s", [ToDID]);
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
