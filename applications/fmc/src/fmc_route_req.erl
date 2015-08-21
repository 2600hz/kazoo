%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(fmc_route_req).

-export([handle_req/2]).

-include("fmc.hrl").

handle_req(JObj, Props) ->
    lager:info("FMC tried to handle route_req"),
    lager:debug("FMC JObj is ~p", [JObj]),
    lager:debug("FMC Props is ~p", [Props]),
    'true' = wapi_route:req_v(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put('callid', CallId),
    Call = whapps_call:from_route_req(JObj),
    
    ANumber = wh_json:get_value(<<"Caller-ID-Number">>, JObj),
    FMCConfig = fmc_db:get_fmc_config(),
    lager:debug("FMC Config is ~p", [FMCConfig]),
    FMCHeaderValue = wh_json:get_value([<<"Custom-SIP-Headers">>, wh_json:get_value(<<"x_fmc_header">>, FMCConfig)], JObj),
    FMCHeaderValueRegexp = wh_json:get_value(<<"x_fmc_regexp">>, FMCConfig),
    FMCHeaderValue1 = binary_to_list(FMCHeaderValue),
    FMCHeaderValueRegexp1 = binary_to_list(FMCHeaderValueRegexp),
    ExtractedFMCValue = case re:run(FMCHeaderValue1, FMCHeaderValueRegexp1) of
                            'nomatch' -> 'undefined';
                            {'match', Groups} ->
                                {Pos, Len} = lists:nth(2, Groups),
                                ExtractedValue = lists:sublist(FMCHeaderValue1, Pos + 1, Len),
                                list_to_binary(ExtractedValue)
                        end,
    Result = fmc_db:get_fmc_item(ANumber, ExtractedFMCValue),
    
    %% do magic to determine if we should respond...
    %% update the call kvs with which module to use (tone or echo)
    case Result of
        {'ok', Action} ->
            lager:info("FMC knows how to route the call"),
            lager:debug("FMC Record found: ~p", [Action]),
            ControllerQ = props:get_value('queue', Props),
            send_route_response(ControllerQ, JObj),
            UpdatedCall = whapps_call:kvs_store(<<"fmc_action">>, {Action, JObj}, Call),
            whapps_call:cache(UpdatedCall, ?APP_NAME);
        {'error', Error} ->
            lager:info("FMC does not know what to do with this! Error is ~p", [Error])
    end.

-spec send_route_response(ne_binary(), wh_json:json()) -> 'ok'.
send_route_response(ControllerQ, JObj) ->
    lager:info("FMC knows how to route the call! sent park response"),
    Resp = props:filter_undefined([{<<"Msg-ID">>, wh_json:get_value(<<"Msg-ID">>, JObj)}
                                   ,{<<"Routes">>, []}
                                   ,{<<"Method">>, <<"park">>}
                                   | wh_api:default_headers(ControllerQ, ?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    lager:debug("Park response is ~p", [Resp]),
    whapps_util:amqp_pool_send(Resp, Publisher).
