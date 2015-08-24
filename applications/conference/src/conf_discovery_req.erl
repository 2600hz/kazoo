
%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2014, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%-------------------------------------------------------------------
-module(conf_discovery_req).

-include("conference.hrl").

-export([handle_req/2]).

-spec handle_req(wh_json:object(), wh_proplist()) -> _.
handle_req(JObj, _Options) ->
    'true' = wapi_conference:discovery_req_v(JObj),
    Call = whapps_call:from_json(wh_json:get_value(<<"Call">>, JObj)),
    _ = whapps_call_command:set(wh_json:from_list([{<<"Is-Conference">>, <<"true">>}]), 'undefined', Call),
    whapps_call:put_callid(Call),
    case conf_participant_sup:start_participant(Call) of
        {'ok', Srv} ->
            conf_participant:set_discovery_event(JObj, Srv),
            conf_participant:consume_call_events(Srv),
            whapps_call_command:answer(Call),
            maybe_welcome_to_conference(Call, Srv, JObj);
        _Else -> discovery_failed(Call, 'undefined')
    end.

-spec maybe_welcome_to_conference(whapps_call:call(), pid(), wh_json:object()) -> 'ok'.
maybe_welcome_to_conference(Call, Srv, DiscoveryJObj) ->
    case wh_json:is_true(<<"Play-Welcome">>, DiscoveryJObj, 'true') of
        'false' -> maybe_collect_conference_id(Call, Srv, DiscoveryJObj);
        'true' -> welcome_to_conference(Call, Srv, DiscoveryJObj)
    end.

-spec welcome_to_conference(whapps_call:call(), pid(), wh_json:object()) -> 'ok'.
welcome_to_conference(Call, Srv, DiscoveryJObj) ->
    case wh_json:get_binary_value(<<"Play-Welcome-Media">>, DiscoveryJObj) of
        'undefined' -> whapps_call_command:prompt(<<"conf-welcome">>, Call);
        Media -> whapps_call_command:play(
                   wh_media_util:media_path(Media, whapps_call:account_id(Call))
                   ,Call
                  )
    end,
    maybe_collect_conference_id(Call, Srv, DiscoveryJObj).

-spec maybe_collect_conference_id(whapps_call:call(), pid(), wh_json:object()) -> 'ok'.
maybe_collect_conference_id(Call, Srv, DiscoveryJObj) ->
    case wh_json:get_value(<<"Conference-Doc">>, DiscoveryJObj) of
        'undefined' -> collect_conference_id(Call, Srv, DiscoveryJObj);
        Doc ->
            N = wh_json:get_value(<<"name">>, Doc, wh_util:rand_hex_binary(8)),
            lager:debug("conf doc (~s) set instead of conf id", [N]),
            Conference0 = whapps_conference:set_id(N, create_conference(Doc, <<"none">>, Call)),
            Conference = maybe_set_conference_tones(Conference0, DiscoveryJObj),  %% MAY remove this line
            maybe_collect_conference_pin(Conference, Call, Srv)
    end.

-spec collect_conference_id(whapps_call:call(), pid(), wh_json:object()) -> 'ok'.
collect_conference_id(Call, Srv, DiscoveryJObj) ->
    {'ok', JObj} = conf_participant:discovery_event(Srv),
    ConferenceId = wh_json:get_value(<<"Conference-ID">>, JObj),
    case validate_conference_id(ConferenceId, Call) of
        {'ok', Conference0} ->
            Conference = maybe_set_conference_tones(Conference0, DiscoveryJObj),
            maybe_collect_conference_pin(Conference, Call, Srv);
        {'error', _} ->
            discovery_failed(Call, Srv)
    end.

-spec maybe_set_conference_tones(whapps_conference:conference(), wh_json:object()) ->
                                  whapps_conference:conference().
maybe_set_conference_tones(Conference, JObj) ->
    ShouldPlayOnEntry = wh_json:get_ne_value(<<"Play-Entry-Tone">>, JObj, whapps_conference:play_entry_tone(Conference)),
    ShouldPlayOnExit = wh_json:get_ne_value(<<"Play-Exit-Tone">>, JObj, whapps_conference:play_exit_tone(Conference)),
    whapps_conference:set_play_entry_tone(ShouldPlayOnEntry
                                          ,whapps_conference:set_play_exit_tone(ShouldPlayOnExit, Conference)
                                         ).

-spec maybe_collect_conference_pin(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
maybe_collect_conference_pin(Conference, Call, Srv) ->
    case whapps_conference:moderator(Conference) of
        'true' ->
            maybe_collect_moderator_pin(Conference, Call, Srv);
        'false' ->
            maybe_collect_member_pin(Conference, Call, Srv);
        _Else ->
            maybe_collect_pin(Conference, Call, Srv)
    end.

-spec maybe_collect_pin(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
maybe_collect_pin(Conference, Call, Srv) ->
    case wh_util:is_empty(whapps_conference:moderator_pins(Conference))
        andalso wh_util:is_empty(whapps_conference:member_pins(Conference))
    of
        'false' ->
            collect_conference_pin('undefined', Conference, Call, Srv);
        'true' ->
            C = whapps_conference:set_moderator('false', Conference),
            prepare_whapps_conference(C, Call, Srv)
    end.

-spec maybe_collect_moderator_pin(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
maybe_collect_moderator_pin(Conference, Call, Srv) ->
    case whapps_conference:moderator_pins(Conference) of
        [] ->
            lager:debug("moderator entry requires no pin"),
            prepare_whapps_conference(Conference, Call, Srv);
        _Else ->
            collect_conference_pin('true', Conference, Call, Srv)
    end.

-spec maybe_collect_member_pin(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
maybe_collect_member_pin(Conference, Call, Srv) ->
    case whapps_conference:member_pins(Conference) of
        [] ->
            lager:debug("member entry requires no pin"),
            prepare_whapps_conference(Conference, Call, Srv);
        _Else ->
            collect_conference_pin('false', Conference, Call, Srv)
    end.

-spec collect_conference_pin(api_boolean(), whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
collect_conference_pin(Type, Conference, Call, Srv) ->
    case validate_conference_pin(Type, Conference, Call, 1) of
        {'ok', C} ->
            prepare_whapps_conference(C, Call, Srv);
        {'error', _} ->
            discovery_failed(Call, Srv)
    end.

-spec prepare_whapps_conference(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
prepare_whapps_conference(Conference, Call, Srv) ->
    Routines = [{fun whapps_conference:set_application_version/2, ?APP_VERSION}
                ,{fun whapps_conference:set_application_name/2, ?APP_NAME}
                ,{fun maybe_set_as_moderator/2, Srv}
               ],
    C = whapps_conference:update(Routines, Conference),
    search_for_conference(C, Call, Srv).

-spec maybe_set_as_moderator(pid(), whapps_conference:conference()) ->
                                    whapps_conference:conference().
maybe_set_as_moderator(Srv, Conference) ->
    {'ok', JObj} = conf_participant:discovery_event(Srv),
    case wh_json:is_true(<<"Moderator">>, JObj, 'false')
        orelse wh_json:is_true([<<"Conference-Doc">>, <<"moderator">>], JObj, 'undefined')
    of
        'undefined' -> Conference;
        'true' ->
            lager:debug("discovery request defines participant as moderator, overriding previous value"),
            whapps_conference:set_moderator('true', Conference);
        'false' ->
            lager:debug("discovery request defines participant as member, overriding previous value"),
            whapps_conference:set_moderator('false', Conference)
    end.

-spec search_for_conference(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
search_for_conference(Conference, Call, Srv) ->
    case whapps_conference_command:search(Conference) of
        {'error', _} -> handle_search_error(Conference, Call, Srv);
        {'ok', JObj} -> handle_search_resp(JObj, Conference, Call, Srv)
    end.

-spec handle_search_error(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
handle_search_error(Conference, Call, Srv) ->
    Arbitrator = wh_amqp_connections:arbitrator_broker(),
    Queue = whapps_conference:id(Conference),
    wh_amqp_channel:remove_consumer_pid(),
    wh_amqp_channel:consumer_broker(Arbitrator),
    _ = amqp_util:new_queue(Queue),
    try amqp_util:basic_consume(Queue, [{'exclusive', 'true'}]) of
        'ok' ->
            lager:debug("initial participant creating conference on switch nodename '~p'", [whapps_call:switch_hostname(Call)]),
            conf_participant:set_conference(Conference, Srv),
            conf_participant:join_local(Srv),
            wait_for_creation(Conference)
    catch
        'exit':{{'shutdown', {'server_initiated_close', 403, <<"ACCESS_REFUSED", _/binary>>}}, _} ->
            lager:debug("conference queue ~s is exclusive, waiting for conference creation by initial participant", [Queue]),
            handle_resource_locked(Conference, Call, Srv)
    end.

-spec handle_resource_locked(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
handle_resource_locked(Conference, Call, Srv) ->
    case wait_for_creation(Conference) of
        {'ok', JObj} -> handle_search_resp(JObj, Conference, Call, Srv);
        {'error', 'timeout'} -> handle_search_error(Conference, Call, Srv)
    end.

-spec wait_for_creation(whapps_conference:conference()) ->
                               {'ok', wh_json:object()} |
                               {'error', 'timeout'}.
wait_for_creation(Conference) ->
    lager:debug("checking if conference ~s has been created", [whapps_conference:id(Conference)]),
    wait_for_creation(Conference, 8 * ?MILLISECONDS_IN_SECOND).

-spec wait_for_creation(whapps_conference:conference(), non_neg_integer()) ->
                               {'ok', wh_json:object()} |
                               {'error', 'timeout'}.
wait_for_creation(_, After) when After =< 0 ->
    {'error', 'timeout'};
wait_for_creation(Conference, After) ->
    Start = os:timestamp(),
    case whapps_conference_command:search(Conference) of
        {'ok', _}=Ok -> Ok;
        {'error', _} ->
            timer:sleep(?MILLISECONDS_IN_SECOND),
            wait_for_creation(Conference, wh_util:decr_timeout(After, Start))
    end.

-spec handle_search_resp(wh_json:object(), whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
handle_search_resp(JObj, Conference, Call, Srv) ->
    MaxParticipants =  whapps_conference:max_participants(Conference),
    Participants = length(wh_json:get_value(<<"Participants">>, JObj, [])),
    case MaxParticipants =/= 0 andalso Participants >= MaxParticipants of
        'false' -> add_participant_to_conference(JObj, Conference, Call, Srv);
        'true' ->
            _ = whapps_call_command:b_prompt(?DEFAULT_MAX_MEMBERS_MEDIA, Call),
            whapps_call_command:hangup(Call)
    end.

-spec maybe_play_name(whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
maybe_play_name(Conference, Call, Srv) ->
    case whapps_conference:play_name_on_join(Conference) of
        'true' ->
            PronouncedName = case conf_pronounced_name:lookup_name(Call) of
                                 'undefined' ->
                                     lager:debug("Recording pronunciation of the name"),
                                     conf_pronounced_name:record(Call);
                                 Value ->
                                     lager:debug("has pronounced name: ~p", [Value]),
                                     Value
                             end,
            conf_participant:set_name_pronounced(PronouncedName, Srv);
        'false' -> 'ok'
    end.

-spec add_participant_to_conference(wh_json:object(), whapps_conference:conference(), whapps_call:call(), pid()) -> 'ok'.
add_participant_to_conference(JObj, Conference, Call, Srv) ->
    _ = maybe_play_name(Conference, Call, Srv),

    _ = case whapps_conference:play_entry_prompt(Conference) of
            'false' -> 'ok';
            'true' -> whapps_call_command:prompt(<<"conf-joining_conference">>, Call)
        end,

    SwitchHostname = whapps_call:switch_hostname(Call),
    lager:debug("participant switch nodename ~p", [SwitchHostname]),

    case wh_json:get_value(<<"Switch-Hostname">>, JObj) of
        SwitchHostname ->
            lager:debug("running conference is on the same switch, joining on ~s", [SwitchHostname]),
            conf_participant:set_conference(Conference, Srv),
            conf_participant:join_local(Srv);
        _Else ->
            lager:debug("running conference is on a different switch, bridging to ~s: ~p", [_Else, JObj]),
            conf_participant:set_conference(Conference, Srv),
            conf_participant:join_remote(Srv, JObj)
    end.

-spec discovery_failed(whapps_call:call(), pid() | 'undefined') -> 'ok'.
discovery_failed(Call, _) -> whapps_call_command:hangup(Call).

-spec validate_conference_id(api_binary(), whapps_call:call()) ->
                                    {'ok', whapps_conference:conference()} |
                                    {'error', _}.
validate_conference_id(ConferenceId, Call) ->
    validate_conference_id(ConferenceId, Call, 1).

-spec validate_conference_id(api_binary(), whapps_call:call(), pos_integer()) ->
                                    {'ok', whapps_conference:conference()} |
                                    {'error', _}.
validate_conference_id('undefined', Call, Loop) when Loop > 3 ->
    lager:debug("caller has failed to provide a valid conference number to many times"),
    _ = whapps_call_command:b_prompt(<<"conf-too_many_attempts">>, Call),
    {'error', 'too_many_attempts'};
validate_conference_id('undefined', Call, Loop) ->
    lager:debug("requesting conference id from caller"),
    case whapps_call_command:b_prompt_and_collect_digits(1, 16, <<"conf-enter_conf_number">>, 1, Call) of
        {'error', _}=E -> E;
        {'ok', Digits} ->
            validate_collected_conference_id(Call, Loop, Digits)
    end;
validate_conference_id(ConferenceId, Call, Loop) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:open_doc(AccountDb, ConferenceId) of
        {'ok', JObj} ->
            lager:debug("discovery request contained a valid conference id, building object"),
            {'ok', create_conference(JObj, <<"none">>, Call)};
        _Else ->
            lager:debug("could not find conference ~s: ~p", [ConferenceId, _Else]),
            validate_conference_id('undefined', Call, Loop)
    end.

-spec validate_collected_conference_id(whapps_call:call(), non_neg_integer(), binary()) ->
                                              {'ok', whapps_conference:conference()} |
                                              {'error', _}.
validate_collected_conference_id(Call, Loop, Digits) ->
    AccountDb = whapps_call:account_db(Call),
    ViewOptions = [{'key', Digits}
                   ,'include_docs'
                  ],
    case couch_mgr:get_results(AccountDb, <<"conference/listing_by_number">>, ViewOptions) of
        {'ok', [JObj]} ->
            lager:debug("caller has entered a valid conference id, building object"),
            {'ok', create_conference(wh_json:get_value(<<"doc">>, JObj), Digits, Call)};
        _Else ->
            lager:debug("could not find conference number ~s: ~p", [Digits, _Else]),
            _ = whapps_call_command:prompt(<<"conf-bad_conf">>, Call),
            validate_conference_id('undefined', Call, Loop + 1)
    end.

-spec validate_conference_pin(api_boolean(), whapps_conference:conference(), whapps_call:call(), pos_integer()) ->
                                     {'ok', whapps_conference:conference()} |
                                     {'error', term()}.
validate_conference_pin(_, _, Call, Loop) when Loop > 3->
    lager:debug("caller has failed to provide a valid conference pin to many times"),
    _ = whapps_call_command:b_prompt(<<"conf-too_many_attempts">>, Call),
    {'error', 'too_many_attempts'};
validate_conference_pin('true', Conference, Call, Loop) ->
    lager:debug("requesting moderator pin from caller"),
    case whapps_call_command:b_prompt_and_collect_digits(1, 16, <<"conf-enter_conf_pin">>, 1, Call) of
        {'error', _}=E -> E;
        {'ok', Digits} ->
            validate_collected_conference_pin(Conference, Call, Loop, Digits)
    end;
validate_conference_pin('false', Conference, Call, Loop) ->
    lager:debug("requesting member pin from caller"),
    case whapps_call_command:b_prompt_and_collect_digits(1, 16, <<"conf-enter_conf_pin">>, 1, Call) of
        {'error', _}=E -> E;
        {'ok', Digits} ->
            validate_collected_member_pins(Conference, Call, Loop, Digits)
    end;
validate_conference_pin(_, Conference, Call, Loop) ->
    lager:debug("requesting conference pin from caller, which will be used to disambiguate member/moderator"),
    case whapps_call_command:b_prompt_and_collect_digits(1, 16, <<"conf-enter_conf_pin">>, 1, Call) of
        {'error', _}=E -> E;
        {'ok', Digits} ->
            validate_if_pin_is_for_moderator(Conference, Call, Loop, Digits)
    end.

-spec validate_if_pin_is_for_moderator(whapps_conference:conference(), whapps_call:call(), non_neg_integer(), binary()) ->
                                              {'ok', whapps_conference:conference()} |
                                              {'error', _}.
validate_if_pin_is_for_moderator(Conference, Call, Loop, Digits) ->
    MemberPins = whapps_conference:member_pins(Conference),
    ModeratorPins = whapps_conference:moderator_pins(Conference),
    case {(lists:member(Digits, MemberPins)
           orelse (MemberPins =:= [] andalso Digits =:= <<>>)
          )
          ,(lists:member(Digits, ModeratorPins)
            orelse (MemberPins =:= [] andalso Digits =:= <<>>)
           )
         }
    of
        {'true', _} ->
            lager:debug("caller entered a pin belonging to a member"),
            {'ok', whapps_conference:set_moderator('false', Conference)};
        {'false', 'true'} ->
            lager:debug("caller entered a pin belonging to a moderator"),
            {'ok', whapps_conference:set_moderator('true', Conference)};
        _Else ->
            lager:debug("caller entered an invalid pin"),
            _ = whapps_call_command:prompt(<<"conf-bad_pin">>, Call),
            validate_conference_pin('undefined', Conference, Call, Loop + 1)
    end.

-spec validate_collected_member_pins(whapps_conference:conference(), whapps_call:call(), non_neg_integer(), binary()) ->
                                            {'ok', whapps_conference:conference()} |
                                            {'error', _}.
validate_collected_member_pins(Conference, Call, Loop, Digits) ->
    Pins = whapps_conference:member_pins(Conference),
    case lists:member(Digits, Pins)
        orelse (Pins =:= [] andalso Digits =:= <<>>)
    of
        'true' ->
            lager:debug("caller entered a valid member pin"),
            {'ok', Conference};
        'false' ->
            lager:debug("caller entered an invalid pin"),
            _ = whapps_call_command:prompt(<<"conf-bad_pin">>, Call),
            validate_conference_pin('false', Conference, Call, Loop + 1)
    end.

-spec validate_collected_conference_pin(whapps_conference:conference(), whapps_call:call(), pos_integer(), binary()) ->
                                               {'ok', whapps_conference:conference()} |
                                               {'error', _}.
validate_collected_conference_pin(Conference, Call, Loop, Digits) ->
    Pins = whapps_conference:moderator_pins(Conference),
    case lists:member(Digits, Pins)
        orelse (Pins =:= [] andalso Digits =:= <<>>)
    of
        'true' ->
            lager:debug("caller entered a valid moderator pin"),
            {'ok', Conference};
        'false' ->
            lager:debug("caller entered an invalid pin"),
            _ = whapps_call_command:prompt(<<"conf-bad_pin">>, Call),
            validate_conference_pin('true', Conference, Call, Loop + 1)
    end.

-spec create_conference(wh_json:object(), binary(), whapps_call:call()) ->
                               whapps_conference:conference().
create_conference(JObj, Digits, Call) ->
    Doc = whapps_conference:from_conference_doc(JObj),
    Conference = whapps_conference:set_call(Call, Doc),
    ModeratorNumbers = wh_json:get_value([<<"moderator">>, <<"numbers">>], JObj, []),
    MemberNumbers = wh_json:get_value([<<"member">>, <<"numbers">>], JObj, []),
    case {lists:member(Digits, MemberNumbers)
          ,lists:member(Digits, ModeratorNumbers)
         }
    of
        {'true', 'false'} ->
            lager:debug("the digits used to find the conference were unambiguously a member"),
            whapps_conference:set_moderator('false', Conference);
        {'false', 'true'} ->
            lager:debug("the digits used to find the conference were unambiguously a moderator"),
            whapps_conference:set_moderator('true', Conference);
        %% the conference number is ambiguous regarding member: either both have the same number
        %%   or they joined by the discovery event having the conference id
        _Else -> Conference
    end.
