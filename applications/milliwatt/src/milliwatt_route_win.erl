%%%-------------------------------------------------------------------
%%% @copyright (C) 2013, VoIP, INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%% Peter Defebvre
%%%-------------------------------------------------------------------
-module(milliwatt_route_win).

-export([handle_req/2]).

-include("milliwatt.hrl").

handle_req(JObj, _Props) ->
    'true' = wapi_route:win_v(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    wh_util:put_callid(CallId),
    case whapps_call:retrieve(CallId, ?APP_NAME) of
        {'ok', C} ->
            Call = whapps_call:from_route_win(JObj, C),
            Action = whapps_call:kvs_fetch(<<"milliwatt_action">>, Call),
            lager:info("milliwatt wins the routing", []),
            case Action of
                'tone' ->
                    lager:info("milliwatt will execute action tone", []),
                    milliwatt_tone:exec(Call);
                'echo' ->
                    lager:info("milliwatt will execute action echo", []),
                    milliwatt_echo:exec(Call);
                _ ->
                    lager:warning("milliwatt doesnt know action: ~p", [Action]),
                    whapps_call_command:hangup(Call)
            end;
         {'error', _R} ->
            lager:error("something went wrong: ~p", [_R])
    end.
