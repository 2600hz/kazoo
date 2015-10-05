%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(conf_route_win).

-include("conference.hrl").

-export([handle_req/2]).

-spec handle_req(wh_json:object(), wh_proplist()) -> _.
handle_req(JObj, _Options) ->
    'true' = wapi_route:win_v(JObj),
    wh_util:put_callid(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case conf_util:retrieve(CallId) of
        {'error', 'not_found'} -> 'ok';
        {'ok', {Call, Conference}} ->
            start_participant(whapps_call:from_route_win(JObj, Call), Conference)
    end.

-spec start_participant(whapps_call:call(), whapps_conference:conference()) -> 'ok'.
start_participant(Call, Conference) ->
    case conf_participant_sup:start_participant(Call) of
        {'ok', Participant} ->
            join_local(Call, Conference, Participant);
        _Else -> whapps_call_command:hangup(Call)
    end.

-spec join_local(whapps_call:call(), whapps_conference:conference(), server_ref()) -> 'ok'.
join_local(Call, Conference, Participant) ->
    Routines = [{fun whapps_conference:set_moderator/2, 'false'}
                ,{fun whapps_conference:set_application_version/2, ?APP_VERSION}
                ,{fun whapps_conference:set_application_name/2, ?APP_NAME}
               ],
    C = whapps_conference:update(Routines, Conference),
    conf_participant:set_conference(C, Participant),
    whapps_call_command:answer(Call),
    conf_participant:join_local(Participant).
