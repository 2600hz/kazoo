%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(conf_route_req).

-include("conference.hrl").

-export([handle_req/2]).

-define(DEFAULT_ROUTE_WIN_TIMEOUT, 3000).
-define(ROUTE_WIN_TIMEOUT_KEY, <<"route_win_timeout">>).
-define(ROUTE_WIN_TIMEOUT, kapps_config:get_integer(?CONFIG_CAT, ?ROUTE_WIN_TIMEOUT_KEY, ?DEFAULT_ROUTE_WIN_TIMEOUT)).

-spec handle_req(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_req(JObj, _Props) ->
    'true' = kapi_route:req_v(JObj),
    kz_log:put_callid(JObj),
    Call = kapps_call:from_route_req(JObj),

    case kapps_call:request_user(Call) of
        <<"conference">> ->
            maybe_send_route_response(JObj, Call);
        _Else -> 'ok'
    end.

-spec maybe_send_route_response(kz_json:object(), kapps_call:call()) -> 'ok'.
maybe_send_route_response(JObj, Call) ->
    case find_conference(Call) of
        {'ok', Conference} ->
            send_route_response(JObj, Call, Conference);
        {'error', _} -> 'ok'
    end.

-spec send_route_response(kz_json:object(), kapps_call:call(), kapps_conference:conference()) ->
          'ok'.
send_route_response(JObj, Call, Conference) ->
    lager:info("conference knows how to route the call! sending park response"),
    CCVs = kz_json:set_values([{<<"Account-ID">>, kapps_conference:account_id(Conference)}]
                             ,kapps_call:custom_channel_vars(Call)
                             ),
    Resp = props:filter_undefined([{?KEY_MSG_ID, kz_api:msg_id(JObj)}
                                  ,{?KEY_MSG_REPLY_ID, kapps_call:call_id_direct(Call)}
                                  ,{<<"Routes">>, []}
                                  ,{<<"Method">>, <<"park">>}
                                  ,{<<"Custom-Channel-Vars">>, CCVs}
                                  ,{<<"Custom-Application-Vars">>, kapps_call:custom_application_vars(Call)}
                                   | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
                                  ]),
    ServerId = kz_api:server_id(JObj),
    Publisher = fun(P) -> kapi_route:publish_resp(ServerId, P) end,
    case kz_amqp_worker:call(Resp
                            ,Publisher
                            ,fun kapi_route:win_v/1
                            ,?ROUTE_WIN_TIMEOUT
                            )
    of
        {'ok', RouteWin} ->
            lager:info("conference has received a route win"),
            start_participant(kapps_call:from_route_win(RouteWin, Call), Conference);
        {'error', _E} ->
            lager:info("conference didn't received a route win, exiting : ~p", [_E])
    end.

-spec start_participant(kapps_call:call(), kapps_conference:conference()) -> 'ok'.
start_participant(Call, Conference) ->
    case conf_participant_sup:start_participant(Call) of
        {'ok', Participant} ->
            join_local(Call, Conference, Participant);
        _Else -> kapps_call_command:hangup(Call)
    end.

-spec join_local(kapps_call:call(), kapps_conference:conference(), kz_types:server_ref()) -> 'ok'.
join_local(Call, Conference, Participant) ->
    IsModerator = kz_term:is_true(kapps_call:custom_sip_header(<<"X-Conf-Flags-Moderator">>, Call)),
    Routines = [{fun kapps_conference:set_moderator/2, IsModerator}
               ,{fun kapps_conference:set_application_version/2, ?APP_VERSION}
               ,{fun kapps_conference:set_application_name/2, ?APP_NAME}
               ],
    C = kapps_conference:update(Routines, Conference),
    conf_participant:set_conference(C, Participant),
    maybe_set_name_pronounced(Call, Participant),
    kapps_call_command:answer(Call),
    conf_participant:join_local(Participant).

-spec maybe_set_name_pronounced(kapps_call:call(), kz_types:server_ref()) -> 'ok'.
maybe_set_name_pronounced(Call, Participant) ->
    AccountId = kapps_call:custom_sip_header(<<"X-Conf-Values-Pronounced-Name-Account-ID">>, Call),
    MediaId = kapps_call:custom_sip_header(<<"X-Conf-Values-Pronounced-Name-Media-ID">>, Call),
    maybe_set_name_pronounced(AccountId, MediaId, Participant).

-spec maybe_set_name_pronounced(kz_json:api_json_term(), kz_json:api_json_term(), kz_types:server_ref()) -> 'ok'.
maybe_set_name_pronounced('undefined', _, _) -> 'ok';
maybe_set_name_pronounced(_, 'undefined', _) -> 'ok';
maybe_set_name_pronounced(AccountId, MediaId, Participant) ->
    conf_participant:set_name_pronounced({'undefined', AccountId, MediaId}, Participant).

-spec find_conference(kapps_call:call()) ->
          {'error', 'realm_unknown'} |
          kz_datamgr:data_error() |
          {'ok', kapps_conference:conference()}.
find_conference(Call) ->
    find_conference(Call, find_account_db(Call)).

-spec find_conference(kapps_call:call(), kz_term:api_ne_binary()) ->
          {'error', 'realm_unknown'} |
          {'ok', kapps_conference:conference()}.
find_conference(_Call, 'undefined') ->
    {'error', 'realm_unknown'};
find_conference(Call, AccountDb) ->
    ConferenceId = kapps_call:to_user(Call),
    case kz_datamgr:open_cache_doc(AccountDb, ConferenceId) of
        {'ok', JObj} ->
            <<"conference">> = kz_doc:type(JObj),
            {'ok', kapps_conference:from_conference_doc(JObj)};
        {'error', _R}=Error ->
            lager:info("unable to find conference ~s in account db ~s: ~p"
                      ,[ConferenceId, AccountDb, _R]
                      ),
            Error
    end.

-spec find_account_db(kapps_call:call()) -> kz_term:api_binary().
find_account_db(Call) ->
    Realm = kapps_call:to_realm(Call),
    case kapps_util:get_account_by_realm(Realm) of
        {'ok', AccountDb} ->
            lager:debug("found account db by realm ~s: ~s", [Realm, AccountDb]),
            AccountDb;
        {'multiples', [AccountDb|_]} ->
            lager:debug("found account db by realm ~s: ~s", [Realm, AccountDb]),
            AccountDb;
        {'error', _R} ->
            lager:debug("unable to find account for realm ~s: ~p"
                       ,[Realm, _R]
                       ),
            'undefined'
    end.
