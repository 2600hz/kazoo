%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(conf_route_req).

-include("conference.hrl").

-export([handle_req/2]).

-define(DEFAULT_ROUTE_WIN_TIMEOUT, 3000).
-define(ROUTE_WIN_TIMEOUT_KEY, <<"route_win_timeout">>).
-define(ROUTE_WIN_TIMEOUT, whapps_config:get_integer(?CONFIG_CAT, ?ROUTE_WIN_TIMEOUT_KEY, ?DEFAULT_ROUTE_WIN_TIMEOUT)).

-spec handle_req(wh_json:object(), wh_proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = wapi_route:req_v(JObj),
    wh_util:put_callid(JObj),
    Call = whapps_call:from_route_req(JObj),

    case whapps_call:request_user(Call) of
        <<"conference">> ->
            maybe_send_route_response(JObj, Call);
        _Else -> 'ok'
    end.

-spec maybe_send_route_response(wh_json:object(), whapps_call:call()) -> 'ok'.
maybe_send_route_response(JObj, Call) ->
    case find_conference(Call) of
        {'ok', Conference} ->
            send_route_response(JObj, Call, bridged_conference(Conference));
        {'error', _} -> 'ok'
    end.

-spec send_route_response(wh_json:object(), whapps_call:call(), whapps_conference:conference()) ->
                                 'ok'.
send_route_response(JObj, Call, Conference) ->
    lager:info("conference knows how to route the call! sending park response"),
    Resp = props:filter_undefined([{?KEY_MSG_ID, wh_api:msg_id(JObj)}
                                   ,{?KEY_MSG_REPLY_ID, whapps_call:call_id_direct(Call)}
                                   ,{<<"Routes">>, []}
                                   ,{<<"Method">>, <<"park">>}
                                   | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = wh_api:server_id(JObj),
    Publisher = fun(P) -> wapi_route:publish_resp(ServerId, P) end,
    case wh_amqp_worker:call(Resp
                             ,Publisher
                             ,fun wapi_route:win_v/1
                             ,?ROUTE_WIN_TIMEOUT
                            )
    of
        {'ok', RouteWin} ->
            lager:info("conference has received a route win"),
            start_participant(whapps_call:from_route_win(RouteWin, Call), Conference);
        {'error', _E} ->
            lager:info("conference didn't received a route win, exiting : ~p", [_E])
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

-spec find_conference(whapps_call:call()) -> {'error', any()} |
                                             {'ok', whapps_conference:conference()}.
find_conference(Call) ->
    find_conference(Call, find_account_db(Call)).

find_conference(_Call, 'undefined') -> {'error', 'realm_unknown'};
find_conference(Call, AccountDb) ->
    ConferenceId = whapps_call:to_user(Call),
    case couch_mgr:open_cache_doc(AccountDb, ConferenceId) of
        {'ok', JObj} ->
            <<"conference">> = wh_doc:type(JObj),
            {'ok', whapps_conference:from_conference_doc(JObj)};
        {'error', _R}=Error ->
            lager:info("unable to find conference ~s in account db ~s: ~p"
                       ,[ConferenceId, AccountDb, _R]
                      ),
            Error
    end.

-spec find_account_db(whapps_call:call()) -> api_binary().
find_account_db(Call) ->
    Realm = whapps_call:to_realm(Call),
    case whapps_util:get_account_by_realm(Realm) of
        {'ok', AccountDb} -> AccountDb;
        {'multiples', [AccountDb|_]} -> AccountDb;
        {'error', _R} ->
            lager:debug("unable to find account for realm ~s: ~p"
                        ,[Realm, _R]
                       ),
            'undefined'
    end.

-spec bridged_conference(whapps_conference:conference()) -> whapps_conference:conference().
bridged_conference(Conference) ->
    %% We are relying on the original channel to play media
    %% so that name announcements always work
    case ?SUPPORT_NAME_ANNOUNCEMENT of
        'true' ->
            Updaters = [fun(Conf) -> whapps_conference:set_play_entry_tone('false', Conf) end
                        ,fun(Conf) -> whapps_conference:set_play_exit_tone('false', Conf) end
                       ],
            whapps_conference:update(Updaters, Conference);
        'false' -> Conference
    end.
