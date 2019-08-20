%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2019, 2600Hz
%%% @doc
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(conf_discovery_req).

-export([handle_req/2]).

-include("conference.hrl").

-spec handle_req(kapi_conference:discovery_req(), kz_term:proplist()) -> any().
handle_req(DiscoveryReq, _Options) ->
    'true' = kapi_conference:discovery_req_v(DiscoveryReq),
    Call = kapps_call:from_json(kz_json:get_value(<<"Call">>, DiscoveryReq)),
    _ = kapps_call_command:set(kz_json:from_list([{<<"Is-Conference">>, <<"true">>}]), 'undefined', Call),
    kapps_call:put_callid(Call),
    case conf_participant_sup:start_participant(Call) of
        {'ok', Srv} ->
            conf_participant:set_discovery_event(DiscoveryReq, Srv),
            conf_participant:consume_call_events(Srv),
            kapps_call_command:answer(Call),
            Conference = create_conference(DiscoveryReq, Call),
            conf_config_req:cache_profile(Conference),
            maybe_welcome_to_conference(Srv, Conference);
        _Else -> discovery_failed(Call, 'undefined')
    end.

-spec create_conference(kapi_conference:discover_req(), kapps_call:call()) -> kapps_conference:conference().
create_conference(DiscoveryReq, Call) ->
    create_conference(DiscoveryReq, Call, kz_json:get_ne_binary_value(<<"Conference-ID">>, DiscoveryReq)).

-spec create_conference(kapi_conference:discovery_req(), kapps_call:call(), kz_term:api_ne_binary()) -> kapps_conference:conference().
create_conference(DiscoveryReq, Call, 'undefined') ->
    lager:debug("using discovery to build conference"),
    Conference = kapps_conference:set_call(Call, kapps_conference:new()),
    Conference1 = kapps_conference:set_discovery_request(DiscoveryReq, Conference),
    kapps_conference:from_json(DiscoveryReq, Conference1);
create_conference(DiscoveryReq, Call, ConferenceId) ->
    Conference = kapps_conference:set_call(Call, kapps_conference:new()),
    case kz_datamgr:open_cache_doc(kapps_call:account_db(Call), ConferenceId) of
        {'ok', Doc} ->
            lager:debug("discovery request contained a valid conference id, building object"),
            WithConferenceDoc = kz_json:set_value(<<"Conference-Doc">>, Doc, DiscoveryReq),
            kapps_conference:from_json(WithConferenceDoc, Conference);
        _Else ->
            lager:debug("could not find specified conference id ~s: ~p", [ConferenceId, _Else]),
            kapps_conference:from_json(cleanup_conference_doc(DiscoveryReq), Conference)
    end.
-spec create_collected_conference(kz_json:object(), kapps_conference:conference()) -> kapps_conference:conference().
create_collected_conference(Doc, Conference) ->
    Id = kz_doc:id(Doc),
    lager:debug("creating discovered conf ~s", [Id]),
    ReqData = kapps_conference:discovery_request(Conference),
    NewReqData = kz_json:set_values([{<<"Conference-ID">>, Id}
                                    ,{<<"Conference-Doc">>, Doc}
                                    ,{<<"Conference-Name">>, kzd_conferences:name(Doc)}
                                    ]
                                   ,ReqData
                                   ),
    Conference1 = kapps_conference:from_json(NewReqData, Conference),
    conf_config_req:cache_profile(Conference1),
    Conference1.

-spec cleanup_conference_doc(kapi_conference:discovery_req()) -> kapi_conference:discovery_req().
cleanup_conference_doc(DiscoveryReq) ->
    cleanup_conference_doc(DiscoveryReq, kz_json:get_json_value(<<"Conference-Doc">>, DiscoveryReq)).

-spec cleanup_conference_doc(kapi_conference:discovery_req(), kzd_conferences:doc() | 'undefined') -> kapi_conference:discovery_req().
cleanup_conference_doc(DiscoveryReq, 'undefined') -> DiscoveryReq;
cleanup_conference_doc(DiscoveryReq, ConferenceDoc) ->
    kz_json:set_value(<<"Conference-Doc">>, kz_doc:public_fields(ConferenceDoc), DiscoveryReq).

-spec maybe_welcome_to_conference(pid(), kapps_conference:conference()) -> 'ok'.
maybe_welcome_to_conference(Srv, Conference) ->
    lager:debug("starting discovery process for conference ~s(id:~s)"
               ,[kapps_conference:name(Conference)
                ,kapps_conference:id(Conference)
                ]
               ),

    case kapps_conference:play_welcome(Conference) of
        'false' ->
            lager:debug("not playing welcome prompt"),
            maybe_collect_conference_id(Srv, Conference);
        'true' -> welcome_to_conference(Srv, Conference)
    end.

-spec welcome_to_conference(pid(), kapps_conference:conference()) -> 'ok'.
welcome_to_conference(Srv, Conference) ->
    Call = kapps_conference:call(Conference),

    DiscoveryJObj = kapps_conference:discovery_request(Conference),

    _ = case kz_json:get_ne_binary_value(<<"Play-Welcome-Media">>, DiscoveryJObj) of
            'undefined' ->
                lager:debug("no welcome media on discovery, using prompt"),
                kapps_call_command:prompt(<<"conf-welcome">>, Call);
            Media ->
                lager:debug("playing welcome media from discovery: ~s", [Media]),
                kapps_call_command:play(kz_media_util:media_path(Media, kapps_call:account_id(Call)), Call)
        end,
    maybe_collect_conference_id(Srv, Conference).

-spec maybe_collect_conference_id(pid(), kapps_conference:conference()) -> 'ok'.
maybe_collect_conference_id(Srv, Conference) ->
    case kapps_conference:id(Conference) of
        'undefined' -> collect_conference_id(Srv, Conference);
        _Else -> valid_conference_id(Srv, Conference, <<"none">>)
    end.

-spec collect_conference_id(pid(), kapps_conference:conference()) ->
                                   'ok'.
collect_conference_id(Srv, Conference) ->
    collect_conference_id(Srv, Conference, 1).

-spec collect_conference_id(pid(), kapps_conference:conference(), pos_integer()) ->
                                   'ok'.
collect_conference_id(Srv, Conference, Loop) when Loop > 3 ->
    lager:debug("caller has failed to provide a valid conference number to many times"),
    Call = kapps_conference:call(Conference),
    _ = kapps_call_command:b_prompt(<<"conf-too_many_attempts">>, Call),
    discovery_failed(Call, Srv);
collect_conference_id(Srv, Conference, Loop) ->
    lager:debug("requesting conference id from caller"),
    Call = kapps_conference:call(Conference),
    Timeout = get_number_timeout(Call),
    case kapps_call_command:b_prompt_and_collect_digits(1, 16, <<"conf-enter_conf_number">>, 1, Timeout, Call) of
        {'error', _} -> discovery_failed(Call, Srv);
        {'ok', Digits} ->
            validate_collected_conference_id(Srv, Conference, Loop, Digits)
    end.

-spec validate_collected_conference_id(pid(), kapps_conference:conference(), pos_integer(), binary()) ->
                                              'ok'.
validate_collected_conference_id(Srv, Conference, Loop, <<>>) ->
    Call = kapps_conference:call(Conference),
    _ = kapps_call_command:prompt(<<"conf-bad_conf">>, Call),
    collect_conference_id(Srv, Conference, Loop + 1);
validate_collected_conference_id(Srv, Conference, Loop, Digits) ->
    Call = kapps_conference:call(Conference),
    AccountDb = kapps_call:account_db(Call),
    ViewOptions = [{'key', Digits}
                  ,'include_docs'
                  ],
    case kz_datamgr:get_results(AccountDb, <<"conference/listing_by_number">>, ViewOptions) of
        {'ok', [JObj]} ->
            lager:debug("caller has entered a valid conference id, building object"),
            Doc = kz_json:get_json_value(<<"doc">>, JObj),
            Conference2 = create_collected_conference(Doc, Conference),
            valid_conference_id(Srv, Conference2, Digits);
        _Else ->
            lager:debug("could not find conference number ~s: ~p", [Digits, _Else]),
            _ = kapps_call_command:prompt(<<"conf-bad_conf">>, Call),
            collect_conference_id(Srv, Conference, Loop + 1)
    end.

-spec valid_conference_id(pid(), kapps_conference:conference(), binary()) -> 'ok'.
valid_conference_id(Srv, Conference, Digits) ->
    JObj = kapps_conference:discovery_request(Conference),
    ModeratorNumbers = kz_json:get_value([<<"moderator">>, <<"numbers">>], JObj, []),
    MemberNumbers = kz_json:get_value([<<"member">>, <<"numbers">>], JObj, []),
    case {lists:member(Digits, MemberNumbers)
         ,lists:member(Digits, ModeratorNumbers)
         }
    of
        {'true', 'false'} ->
            lager:debug("the digits used to find the conference were unambiguously a member"),
            Conference1 = kapps_conference:set_moderator('false', Conference),
            Conference2 = maybe_set_conference_tones(Conference1, JObj),
            Call = kapps_conference:call(Conference2),
            maybe_collect_conference_pin(Conference2, Call, Srv);
        {'false', 'true'} ->
            lager:debug("the digits used to find the conference were unambiguously a moderator"),
            Conference1 = kapps_conference:set_moderator('true', Conference),
            Conference2 = maybe_set_conference_tones(Conference1, JObj),
            Call = kapps_conference:call(Conference2),
            maybe_collect_conference_pin(Conference2, Call, Srv);
        %% the conference number is ambiguous regarding member: either both have the same number
        %%   or they joined by the discovery event having the conference id
        _Else ->
            lager:debug("the digits used were ambiguous whether the caller is member or moderator"),
            Conference1 = kapps_conference:set_moderator('undefined', Conference),
            Conference2 = maybe_set_conference_tones(Conference1, JObj),
            Call = kapps_conference:call(Conference2),
            maybe_collect_conference_pin(Conference2, Call, Srv)
    end.

-spec maybe_set_conference_tones(kapps_conference:conference(), kz_json:object()) ->
                                        kapps_conference:conference().
maybe_set_conference_tones(Conference, JObj) ->
    ShouldPlayOnEntry = kz_json:get_ne_value(<<"Play-Entry-Tone">>, JObj, kapps_conference:play_entry_tone(Conference)),
    ShouldPlayOnExit = kz_json:get_ne_value(<<"Play-Exit-Tone">>, JObj, kapps_conference:play_exit_tone(Conference)),
    kapps_conference:set_play_entry_tone(ShouldPlayOnEntry
                                        ,kapps_conference:set_play_exit_tone(ShouldPlayOnExit, Conference)
                                        ).

-spec maybe_collect_conference_pin(kapps_conference:conference(), kapps_call:call(), pid()) -> 'ok'.
maybe_collect_conference_pin(Conference, Call, Srv) ->
    case kapps_conference:moderator(Conference) of
        'true' ->
            lager:debug("caller is expected to enter moderator pin"),
            maybe_collect_moderator_pin(Conference, Call, Srv);
        'false' ->
            lager:debug("caller is expected to enter member pin"),
            maybe_collect_member_pin(Conference, Call, Srv);
        _Else ->
            lager:debug("not sure if caller is moderator/member, asking for a pin"),
            maybe_collect_pin(Conference, Call, Srv)
    end.

-spec maybe_collect_pin(kapps_conference:conference(), kapps_call:call(), pid()) -> 'ok'.
maybe_collect_pin(Conference, Call, Srv) ->
    case kz_term:is_empty(kapps_conference:moderator_pins(Conference))
        andalso kz_term:is_empty(kapps_conference:member_pins(Conference))
    of
        'false' ->
            collect_conference_pin('undefined', Conference, Call, Srv);
        'true' ->
            lager:debug("no member or moderator pins set, assuming caller is a member"),
            C = kapps_conference:set_moderator('false', Conference),
            prepare_kapps_conference(C, Call, Srv)
    end.

-spec maybe_collect_moderator_pin(kapps_conference:conference(), kapps_call:call(), pid()) -> 'ok'.
maybe_collect_moderator_pin(Conference, Call, Srv) ->
    case kapps_conference:moderator_pins(Conference) of
        [] ->
            lager:debug("moderator entry requires no pin"),
            prepare_kapps_conference(Conference, Call, Srv);
        _Else ->
            collect_conference_pin('true', Conference, Call, Srv)
    end.

-spec maybe_collect_member_pin(kapps_conference:conference(), kapps_call:call(), pid()) -> 'ok'.
maybe_collect_member_pin(Conference, Call, Srv) ->
    case kapps_conference:member_pins(Conference) of
        [] ->
            lager:debug("member entry requires no pin"),
            prepare_kapps_conference(Conference, Call, Srv);
        _Else ->
            collect_conference_pin('false', Conference, Call, Srv)
    end.

-spec collect_conference_pin(kz_term:api_boolean(), kapps_conference:conference(), kapps_call:call(), pid()) -> 'ok'.
collect_conference_pin(Type, Conference, Call, Srv) ->
    case validate_conference_pin(Type, Conference, Call, 1) of
        {'ok', C} ->
            prepare_kapps_conference(C, Call, Srv);
        {'error', _} ->
            discovery_failed(Call, Srv)
    end.

-spec prepare_kapps_conference(kapps_conference:conference(), kapps_call:call(), pid()) -> 'ok'.
prepare_kapps_conference(Conference, Call, Srv) ->
    Routines = [{fun kapps_conference:set_application_version/2, ?APP_VERSION}
               ,{fun kapps_conference:set_application_name/2, ?APP_NAME}
               ,{fun maybe_set_as_moderator/2, Srv}
               ],
    C = kapps_conference:update(Routines, Conference),
    search_for_conference(C, Call, Srv).

-spec maybe_set_as_moderator(pid(), kapps_conference:conference()) ->
                                    kapps_conference:conference().
maybe_set_as_moderator(Srv, Conference) ->
    {'ok', JObj} = conf_participant:discovery_event(Srv),
    case kz_json:is_true(<<"Moderator">>, JObj, 'false')
        orelse kz_json:is_true([<<"Conference-Doc">>, <<"moderator">>], JObj, 'undefined')
    of
        'undefined' -> Conference;
        'true' ->
            lager:debug("discovery request defines participant as moderator, overriding previous value"),
            kapps_conference:set_moderator('true', Conference);
        'false' ->
            lager:debug("discovery request defines participant as member, overriding previous value"),
            kapps_conference:set_moderator('false', Conference)
    end.

-spec search_for_conference(kapps_conference:conference(), kapps_call:call(), pid()) -> 'ok'.
search_for_conference(Conference, Call, Srv) ->
    case kapps_conference_command:search(Conference) of
        {'error', _} -> handle_search_error(Conference, Call, Srv);
        {'ok', JObj} -> handle_search_resp(JObj, Conference, Call, Srv)
    end.

-spec handle_search_error(kapps_conference:conference(), kapps_call:call(), pid()) -> 'ok'.
handle_search_error(Conference, Call, Srv) ->
    Arbitrator = kz_amqp_connections:arbitrator_broker(),
    Queue = kapps_conference:id(Conference),
    kz_amqp_channel:remove_consumer_pid(),
    kz_amqp_channel:remove_consumer_channel(),
    _ = kz_amqp_channel:consumer_broker(Arbitrator),
    _ = kz_amqp_util:new_queue(Queue),
    try kz_amqp_util:basic_consume(Queue, [{'exclusive', 'true'}]) of
        'ok' ->
            lager:debug("initial participant creating conference on switch nodename '~s'", [kapps_call:switch_hostname(Call)]),
            conf_participant:set_conference(Conference, Srv),
            maybe_play_name(Conference, Call, Srv),
            conf_participant:join_local(Srv),
            wait_for_creation(Conference)
    catch
        'exit':{{'shutdown', {'server_initiated_close', 403, <<"ACCESS_REFUSED", _/binary>>}}, _} ->
            lager:debug("conference queue ~s is exclusive, waiting for conference creation by initial participant", [Queue]),
            handle_resource_locked(Conference, Call, Srv)
    end.

-spec handle_resource_locked(kapps_conference:conference(), kapps_call:call(), pid()) -> 'ok'.
handle_resource_locked(Conference, Call, Srv) ->
    case wait_for_creation(Conference) of
        {'ok', JObj} -> handle_search_resp(JObj, Conference, Call, Srv);
        {'error', 'timeout'} -> handle_search_error(Conference, Call, Srv)
    end.

-spec wait_for_creation(kapps_conference:conference()) ->
                               {'ok', kz_json:object()} |
                               {'error', 'timeout'}.
wait_for_creation(Conference) ->
    lager:debug("checking if conference ~s has been created", [kapps_conference:id(Conference)]),
    wait_for_creation(Conference, 8 * ?MILLISECONDS_IN_SECOND).

-spec wait_for_creation(kapps_conference:conference(), non_neg_integer()) ->
                               {'ok', kz_json:object()} |
                               {'error', 'timeout'}.
wait_for_creation(_, After) when After =< 0 ->
    kz_amqp_channel:release(),
    {'error', 'timeout'};
wait_for_creation(Conference, After) ->
    Start = kz_time:start_time(),
    case kapps_conference_command:search(Conference) of
        {'ok', _}=Ok ->
            kz_amqp_channel:release(),
            Ok;
        {'error', _} ->
            timer:sleep(?MILLISECONDS_IN_SECOND),
            wait_for_creation(Conference, kz_time:decr_timeout(After, Start))
    end.

-spec handle_search_resp(kz_json:object(), kapps_conference:conference(), kapps_call:call(), pid()) -> 'ok'.
handle_search_resp(JObj, Conference, Call, Srv) ->
    MaxParticipants =  kapps_conference:max_participants(Conference),
    Participants = length(kz_json:get_value(<<"Participants">>, JObj, [])),
    case MaxParticipants =/= 0
        andalso Participants >= MaxParticipants of
        'false' -> add_participant_to_conference(JObj, Conference, Call, Srv);
        'true' ->
            _ = kapps_call_command:b_prompt(?DEFAULT_MAX_MEMBERS_MEDIA, Call),
            kapps_call_command:hangup(Call)
    end.

-spec maybe_play_name(kapps_conference:conference(), kapps_call:call(), pid()) -> 'ok'.
maybe_play_name(Conference, Call, Srv) ->
    case kapps_conference:play_name_on_join(Conference)
        andalso ?SUPPORT_NAME_ANNOUNCEMENT(kapps_call:account_id(Call))
    of
        'true' ->
            PronouncedName = case conf_pronounced_name:lookup_name(Call) of
                                 'undefined' ->
                                     lager:debug("recording pronunciation of the name"),
                                     conf_pronounced_name:record(Call);
                                 Value ->
                                     lager:debug("has pronounced name: ~p", [Value]),
                                     Value
                             end,
            conf_participant:set_name_pronounced(PronouncedName, Srv);
        'false' -> 'ok'
    end.

-spec add_participant_to_conference(kz_json:object(), kapps_conference:conference(), kapps_call:call(), pid()) -> 'ok'.
add_participant_to_conference(JObj, Conference, Call, Srv) ->
    conf_participant:set_conference(Conference, Srv),
    _ = maybe_play_name(Conference, Call, Srv),

    SwitchHostname = kapps_call:switch_hostname(Call),
    lager:debug("participant switch nodename ~s", [SwitchHostname]),

    case kz_json:get_first_defined([<<"Switch-Hostname">>, <<"Media-Server">>], JObj) of
        SwitchHostname ->
            lager:debug("running conference is on the same switch, joining on ~s", [SwitchHostname]),
            conf_participant:join_local(Srv);
        _Else ->
            lager:debug("running conference is on a different switch, bridging to ~s: ~p", [_Else, JObj]),
            conf_participant:join_remote(Srv, JObj)
    end.

-spec discovery_failed(kapps_call:call(), pid() | 'undefined') -> 'ok'.
discovery_failed(Call, _) -> kapps_call_command:hangup(Call).

-spec validate_conference_pin(kz_term:api_boolean(), kapps_conference:conference(), kapps_call:call(), pos_integer()) ->
                                     {'ok', kapps_conference:conference()} |
                                     {'error', any()}.
validate_conference_pin(_, _, Call, Loop) when Loop > 3->
    lager:debug("caller has failed to provide a valid conference pin to many times"),
    _ = kapps_call_command:b_prompt(<<"conf-too_many_attempts">>, Call),
    {'error', 'too_many_attempts'};
validate_conference_pin('true', Conference, Call, Loop) ->
    lager:debug("requesting moderator pin from caller"),
    case collect_pin(Conference, Call) of
        {'error', _}=E -> E;
        {'ok', Digits} ->
            validate_collected_conference_pin(Conference, Call, Loop, Digits)
    end;
validate_conference_pin('false', Conference, Call, Loop) ->
    lager:debug("requesting member pin from caller"),
    case collect_pin(Conference, Call) of
        {'error', _}=E -> E;
        {'ok', Digits} ->
            validate_collected_member_pins(Conference, Call, Loop, Digits)
    end;
validate_conference_pin(_, Conference, Call, Loop) ->
    lager:debug("requesting conference pin from caller, which will be used to disambiguate member/moderator"),
    case collect_pin(Conference, Call) of
        {'error', _}=E -> E;
        {'ok', Digits} ->
            validate_if_pin_is_for_moderator(Conference, Call, Loop, Digits)
    end.

collect_pin(Conference, Call) ->
    Timeout = get_pin_timeout(Conference),
    kapps_call_command:b_prompt_and_collect_digits(1, 16, <<"conf-enter_conf_pin">>, 1, Timeout, Call).

-spec validate_if_pin_is_for_moderator(kapps_conference:conference(), kapps_call:call(), non_neg_integer(), binary()) ->
                                              {'ok', kapps_conference:conference()} |
                                              {'error', any()}.
validate_if_pin_is_for_moderator(Conference, Call, Loop, Digits) ->
    MemberPins = kapps_conference:member_pins(Conference),
    ModeratorPins = kapps_conference:moderator_pins(Conference),
    case {(lists:member(Digits, MemberPins)
           orelse (MemberPins =:= []
                   andalso Digits =:= <<>>)
          )
         ,(lists:member(Digits, ModeratorPins)
           orelse (MemberPins =:= []
                   andalso Digits =:= <<>>)
          )
         }
    of
        {'true', _} ->
            lager:debug("caller entered a pin belonging to a member"),
            {'ok', kapps_conference:set_moderator('false', Conference)};
        {'false', 'true'} ->
            lager:debug("caller entered a pin belonging to a moderator"),
            {'ok', kapps_conference:set_moderator('true', Conference)};
        _Else ->
            lager:debug("caller entered an invalid pin"),
            _ = kapps_call_command:prompt(<<"conf-bad_pin">>, Call),
            validate_conference_pin('undefined', Conference, Call, Loop + 1)
    end.

-spec validate_collected_member_pins(kapps_conference:conference(), kapps_call:call(), non_neg_integer(), binary()) ->
                                            {'ok', kapps_conference:conference()} |
                                            {'error', any()}.
validate_collected_member_pins(Conference, Call, Loop, Digits) ->
    validate_collected_member_pins(Conference, Call, Loop, Digits, kapps_conference:member_pins(Conference)).

-spec validate_collected_member_pins(kapps_conference:conference(), kapps_call:call(), non_neg_integer(), binary(), kz_term:ne_binaries()) ->
                                            {'ok', kapps_conference:conference()} |
                                            {'error', any()}.
validate_collected_member_pins(Conference, _Call, _Loop, <<>>, []) ->
    lager:info("no member pins configured or necessary and empty collection"),
    {'ok', Conference};
validate_collected_member_pins(Conference, Call, Loop, Digits, Pins) ->
    lager:debug("checking pin ~s against configured pins ~p", [Digits, Pins]),
    case lists:member(Digits, Pins) of
        'true' ->
            lager:debug("caller entered a valid member pin"),
            {'ok', Conference};
        'false' ->
            lager:debug("caller entered an invalid pin"),
            _ = kapps_call_command:prompt(<<"conf-bad_pin">>, Call),
            validate_conference_pin('false', Conference, Call, Loop + 1)
    end.

-spec validate_collected_conference_pin(kapps_conference:conference(), kapps_call:call(), pos_integer(), binary()) ->
                                               {'ok', kapps_conference:conference()} |
                                               {'error', any()}.
validate_collected_conference_pin(Conference, Call, Loop, Digits) ->
    validate_collected_conference_pin(Conference, Call, Loop, Digits, kapps_conference:moderator_pins(Conference)).

-spec validate_collected_conference_pin(kapps_conference:conference(), kapps_call:call(), pos_integer(), binary(), kz_term:ne_binaries()) ->
                                               {'ok', kapps_conference:conference()} |
                                               {'error', any()}.
validate_collected_conference_pin(Conference, _Call, _Loop, <<>>, []) ->
    lager:info("no moderator pins configured or necessary and empty collection"),
    {'ok', Conference};
validate_collected_conference_pin(Conference, Call, Loop, Digits, Pins) ->
    lager:debug("checking pin ~s against configured pins ~p", [Digits, Pins]),
    case lists:member(Digits, Pins) of
        'true' ->
            lager:debug("caller entered a valid moderator pin"),
            {'ok', Conference};
        'false' ->
            lager:debug("caller entered an invalid pin"),
            _ = kapps_call_command:prompt(<<"conf-bad_pin">>, Call),
            validate_conference_pin('true', Conference, Call, Loop + 1)
    end.

-spec get_pin_timeout(kapps_conference:conference()) -> pos_integer().
get_pin_timeout(Conference) ->
    AccountId = kapps_conference:account_id(Conference),
    lager:debug("finding pin timeout for account '~s' conference '~s'"
               ,[AccountId, kapps_conference:id(Conference)]
               ),
    JObj = kapps_conference:conference_doc(Conference),
    kz_json:get_integer_value(<<"pin_timeout">>, JObj, get_account_pin_timeout(AccountId)).

-spec get_account_pin_timeout(kz_term:ne_binary()) -> pos_integer().
get_account_pin_timeout(AccountId) ->
    kapps_account_config:get_global(AccountId, ?CONFIG_CAT, <<"pin_timeout">>, ?COLLECT_PIN_DEFAULT_TIMEOUT).

-spec get_number_timeout(kapps_call:call()) -> pos_integer().
get_number_timeout(Call) ->
    AccountId = kapps_call:account_id(Call),
    kapps_account_config:get_global(AccountId, ?CONFIG_CAT, <<"number_timeout">>, ?COLLECT_NUMBER_DEFAULT_TIMEOUT).
