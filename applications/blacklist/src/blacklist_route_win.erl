%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, 2600Hz
%%% @doc
%%% Handlers for various AMQP payloads
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(blacklist_route_win).

-export([handle_req/2]).

-include("blacklist.hrl").

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, _Props) ->
    'true' = wapi_route:win_v(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    put('callid', CallId),
    case whapps_call:retrieve(CallId, ?APP_NAME) of
        {'ok', C} ->
            lager:info("blacklist wins the routing", []),
            Call = whapps_call:from_route_win(JObj, C),
            Action = whapps_call:kvs_fetch(<<"blacklist_action">>, Call),
            lager:debug("blacklist will ~p that call", [Action]),
            case Action of
                _ ->
                    whapps_call_command:hangup(Call)
            end;
         {'error', _R} ->
            lager:error("something went wrong: ~p", [_R])
    end.
