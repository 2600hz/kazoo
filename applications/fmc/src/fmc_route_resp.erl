%%%-------------------------------------------------------------------
%%% @copyright (C) 2013-2015, 2600Hz, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   SIPLABS, LLC (Vladimir Potapev)
%%%-------------------------------------------------------------------
-module(fmc_route_resp).

-export([handle_req/2]).

-include("fmc.hrl").

handle_req(JObj, Props) ->
    lager:info("FMC trying to handle route_resp ..."),
    lager:debug("FMC JObj is ~p", [JObj]),
    lager:debug("FMC Props is ~p", [Props]),
    'true' = wapi_route:resp_v(JObj),
    MsgId = wh_json:get_value(<<"Msg-ID">>, JObj),
    case wh_json:get_value(<<"App-Name">>, JObj) of
        <<"callflow">> ->
            lager:debug("It's a route_resp from callflow"),
            [{MsgId, CallId, _Call}] = fmc_ets:get(MsgId),
            put('callid', CallId),
            lager:debug("FMC CallID from dict is ~p", [CallId]),
            case whapps_call:retrieve(CallId, ?APP_NAME) of
                {'ok', C} ->
                    lager:info("Starting to prepare route_win for callflow"),
                    lager:debug("Retrieved call is ~p", [C]),
                    WinJObj = whapps_call:kvs_fetch(<<"fmc_action_win">>, C),
                    RouteWinCall = whapps_call:from_route_win(WinJObj, C),
                    lager:debug("RouteWinCall is ~p", [RouteWinCall]),
                    Q = wh_json:get_value(<<"Control-Queue">>, WinJObj),
                    CCVs = wh_json:get_value(<<"Custom-Channel-Vars">>, WinJObj),
                    % create req_win header to send it to callflow
                    ServerId = wh_json:get_value(<<"Server-ID">>, JObj),
                    RouteWin = [{<<"Msg-ID">>, CallId}
                                ,{<<"Call-ID">>, CallId}
                                ,{<<"Control-Queue">>, Q}
                                ,{<<"Custom-Channel-Vars">>, CCVs}
                                | wh_api:default_headers(ServerId, <<"dialplan">>, <<"route_win">>, ?APP_NAME, ?APP_VERSION)
                               ],
                    lager:debug("The route_win for callflow is ~p", [RouteWin]),
                    lager:debug("Sending route_win to ~s", [ServerId]),
                    wapi_route:publish_win(ServerId, RouteWin);
                {'error', _R} ->
                    lager:error("Something went wrong: ~p", [_R])
            end,
            fmc_ets:delete(MsgId);
       _AppName ->
            lager:debug("It's a route_resp from ~p, so bypass it", [_AppName]),
            'ok'
    end.
