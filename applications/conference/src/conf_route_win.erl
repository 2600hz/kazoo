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

-spec handle_req(wh_json:object(), wh_proplist()) -> any().
handle_req(JObj, _Options) ->
    'true' = wapi_route:win_v(JObj),
    wh_util:put_callid(JObj),
    CallId = wh_json:get_value(<<"Call-ID">>, JObj),
    case wh_cache:peek_local(?CONFERENCE_CACHE, CallId) of
        {'error', 'not_found'} -> 'ok';
        {'ok', {Conference, Call}} ->
            start_participant(Conference, whapps_call:from_route_win(JObj, Call))
    end.
            
start_participant(Conference, Call) ->
    case conf_participant_sup:start_participant(Call) of
        {'ok', Participant} ->
            join_local(Participant, Conference, Call);
        _Else -> whapps_call_command:hangup(Call)
    end.

join_local(Participant, Conference, Call) ->
    Routines = [fun(C) -> whapps_conference:set_moderator('false', C) end
                ,fun(C) -> whapps_conference:set_application_version(<<"2.0.0">>, C) end
                ,fun(C) -> whapps_conference:set_application_name(<<"conferences">>, C) end
               ],
    C = whapps_conference:update(Routines, Conference),
    conf_participant:set_conference(C, Participant),
    whapps_call_command:answer(Call),
    conf_participant:join_local(Participant).
