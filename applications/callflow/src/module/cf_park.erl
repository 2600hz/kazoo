%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc
%%% @author Karl Anderson
%%% @author James Aimonetti
%%%
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(cf_park).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).
-export([update_presence/3]).
-export([maybe_cleanup_slot/3]).

-define(PARKED_CALL_DOC_TYPE, <<"parked_call">>).
-define(PARKED_CALLS_VIEW, <<"parking/parked_calls">>).
-define(PARKED_CALL_VIEW, <<"parking/parked_call">>).

-define(SLOT_DOC_ID(A), <<"parking-slot-", A/binary>>).

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".park">>).

-define(DB_DOC_NAME, kapps_config:get_binary(?MOD_CONFIG_CAT, <<"db_doc_name">>, <<"parked_calls">>)).
-define(DEFAULT_RINGBACK_TM, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"default_ringback_timeout">>, 120000)).
-define(DEFAULT_CALLBACK_TM, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"default_callback_timeout">>, 30000)).
-define(PARKED_CALLS_KEY(Db), {?MODULE, 'parked_calls', Db}).
-define(DEFAULT_PARKED_TYPE, <<"early">>).
-define(SYSTEM_PARKED_TYPE, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"parked_presence_type">>, ?DEFAULT_PARKED_TYPE)).
-define(ACCOUNT_PARKED_TYPE(A), kapps_account_config:get(A, ?MOD_CONFIG_CAT, <<"parked_presence_type">>, ?SYSTEM_PARKED_TYPE)).
-define(PRESENCE_TYPE_KEY, <<"Presence-Type">>).
-define(PARK_DELAY_CHECK_TIME_KEY, <<"valet_reservation_cleanup_time_ms">>).
-define(PARK_DELAY_CHECK_TIME, kapps_config:get_integer(?MOD_CONFIG_CAT, ?PARK_DELAY_CHECK_TIME_KEY, ?MILLISECONDS_IN_SECOND * 3)).
-define(PARKING_APP_NAME, <<"park">>).
-define(MAX_SLOT_NUMBER_KEY, <<"max_slot_number">>).
-define(MAX_SLOT_EXCEEDED, 'max_slot_exceeded').

%%------------------------------------------------------------------------------
%% @doc Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%------------------------------------------------------------------------------
-spec update_presence(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
update_presence(SlotNumber, _PresenceId, AccountDb) ->
    case get_slot(SlotNumber, AccountDb) of
        {'ok', Slot} -> update_presence(Slot);
        _ -> 'ok'
    end.

%%------------------------------------------------------------------------------
%% @doc Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%------------------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> any().
handle(Data, Call) ->
    ParkedCalls = get_parked_calls(Call),
    AutoSlotNumber = get_slot_number(ParkedCalls, kapps_call:kvs_fetch('cf_capture_group', Call)),
    SlotNumber = kz_json:get_ne_binary_value(<<"slot">>, Data, AutoSlotNumber),
    ReferredTo = kapps_call:custom_channel_var(<<"Referred-To">>, <<>>, Call),
    PresenceType = presence_type(SlotNumber, Data, Call),

    case re:run(ReferredTo, "Replaces=([^;]*)", [{'capture', [1], 'binary'}]) of
        'nomatch' when ReferredTo =:= <<>> ->
            handle_nomatch_with_empty_referred_to(Data, Call, PresenceType, ParkedCalls, SlotNumber);
        'nomatch' ->
            handle_nomatch(Data, Call, PresenceType, ParkedCalls, SlotNumber, ReferredTo);
        {'match', [Replaces]} ->
            handle_replaces(Data, Call, Replaces)
    end.

-spec handle_replaces(kz_json:object(), kapps_call:call(), kz_term:ne_binary()) ->
          'ok' |
          {'error', 'timeout' | 'failed'}.
handle_replaces(Data, Call, Replaces) ->
    lager:info("call was the result of an attended-transfer completion, updating call id"),
    {'ok', FoundInSlotNumber, Slot} = update_call_id(Replaces, Call),
    wait_for_pickup(FoundInSlotNumber, Slot, Data, Call).

-spec handle_nomatch(kz_json:object(), kapps_call:call(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
handle_nomatch(Data, Call, PresenceType, ParkedCalls, SlotNumber, ReferredTo) ->
    lager:info("call was the result of a blind transfer, assuming intention was to park"),
    Slot = create_slot('undefined', PresenceType, SlotNumber, Data, 'false', Call),
    park_call(SlotNumber, Slot, ParkedCalls, ReferredTo, Data, Call).

-spec handle_nomatch_with_empty_referred_to(kz_json:object(), kapps_call:call(), kz_term:ne_binary(), kz_json:object(), kz_term:ne_binary()) -> 'ok'.
handle_nomatch_with_empty_referred_to(Data, Call, PresenceType, ParkedCalls, SlotNumber) ->
    lager:info("call was the result of a direct dial"),
    case kz_json:get_ne_binary_value(<<"action">>, Data, <<"park">>) of
        <<"direct_park">> ->
            lager:info("action is to directly park the call"),
            Slot = create_slot(cf_exe:callid(Call), PresenceType, SlotNumber, Data, 'false', Call),
            direct_park(SlotNumber, Slot, ParkedCalls, Data, Call);
        <<"park">> ->
            lager:info("action is to park the call"),
            Slot = create_slot('undefined', PresenceType, SlotNumber, Data, 'true', Call),
            park_call(SlotNumber, Slot, ParkedCalls, 'undefined', Data, Call);
        <<"retrieve">> ->
            lager:info("action is to retrieve a parked call"),
            case retrieve(SlotNumber, Call) of
                {'ok', _} ->
                    cf_exe:transfer(Call);
                _Else ->
                    _ = kapps_call_command:b_answer(Call),
                    _ = kapps_call_command:b_prompt(<<"park-no_caller">>, Call),
                    cf_exe:stop(Call)
            end;
        <<"auto">> ->
            lager:info("action is to automatically determine if we should retrieve or park"),
            case retrieve(SlotNumber, Call) of
                {'error', _} ->
                    Slot = create_slot(cf_exe:callid(Call), PresenceType, SlotNumber, Data, 'true', Call),
                    park_call(SlotNumber, Slot, ParkedCalls, 'undefined', Data, Call);
                {'ok', _} ->
                    cf_exe:transfer(Call)
            end
    end.

-spec direct_park(kz_term:ne_binary(), kz_json:object(), kz_json:object(), kz_json:object(), kapps_call:call()) -> 'ok'.
direct_park(SlotNumber, Slot, ParkedCalls, Data, Call) ->
    MaxSlotNumber = kz_json:get_integer_value(?MAX_SLOT_NUMBER_KEY, Data),
    case save_slot(SlotNumber, MaxSlotNumber, Slot, ParkedCalls, Call) of
        {'ok', _} -> parked_call(SlotNumber, Slot, Data, Call);
        {'error', ?MAX_SLOT_EXCEEDED} ->
            cf_exe:continue(kz_term:to_upper_binary(?MAX_SLOT_EXCEEDED), Call);
        {'error', _Reason} ->
            lager:info("unable to save direct park slot: ~p", [_Reason]),
            cf_exe:stop(Call)
    end.

%%------------------------------------------------------------------------------
%% @doc Determine the appropriate action to retrieve a parked call
%% @end
%%------------------------------------------------------------------------------
-spec retrieve(kz_term:ne_binary(), kapps_call:call()) ->
          {'ok', 'retrieved'} |
          {'error', 'slot_empty' | 'timeout' | 'failed'}.
retrieve(SlotNumber, Call) ->
    SlotReturn = get_slot(SlotNumber, kapps_call:account_db(Call)),
    retrieve(SlotNumber, SlotReturn, 1, Call).

-spec retrieve(kz_term:ne_binary(), {'ok', kz_json:object()} | {'error', kz_datamgr:data_error()}, 1..3, kapps_call:call()) ->
          {'ok', 'retrieved'} |
          {'error', 'slot_empty' | 'timeout' | 'failed'}.
retrieve(SlotNumber, {'error', 'not_found'}, _Try, _Call) ->
    lager:info("the parking slot ~s is empty, unable to retrieve caller", [SlotNumber]),
    {'error', 'slot_empty'};
retrieve(SlotNumber, {'error', _E}, _Try, _Call) ->
    lager:info("getting the parking slot ~s errored, unable to retrieve caller: ~p", [SlotNumber, _E]),
    {'error', 'slot_empty'};
retrieve(SlotNumber, {'ok', Slot}, 3, Call) ->
    retrieve_after_retries(SlotNumber, Slot, Call);
retrieve(SlotNumber, {'ok', Slot}, Try, Call) ->
    maybe_attempt_retrieve(SlotNumber, Slot, Try, Call).

maybe_attempt_retrieve(SlotNumber, Slot, Try, Call) ->
    maybe_attempt_retrieve(SlotNumber, Slot, Try, Call, maybe_retrieve_slot(Slot)).

maybe_attempt_retrieve(_SlotNumber, _Slot, _Try, _Call, {'error', _}=E) -> E;
maybe_attempt_retrieve(SlotNumber, _Slot, Try, Call, {'retry', ParkedCallId}) ->
    lager:info("the parking slot ~s currently has a pending attended parked call ~s, retrying in 1 sec"
              ,[SlotNumber, ParkedCallId]
              ),
    timer:sleep(?MILLISECONDS_IN_SECOND),
    retrieve(SlotNumber, get_slot(SlotNumber, kapps_call:account_db(Call)), Try + 1, Call);
maybe_attempt_retrieve(SlotNumber, _Slot, _Try, Call, {'ok', ParkedCallId}) ->
    lager:info("the parking slot ~s currently has a parked call ~s, attempting to retrieve caller"
              ,[SlotNumber, ParkedCallId]
              ),
    case retrieve_slot(ParkedCallId, Call) of
        'ok' ->
            _ = publish_retrieved(Call, SlotNumber),
            _ = cleanup_slot(SlotNumber, ParkedCallId, kapps_call:account_db(Call)),
            {'ok', 'retrieved'};
        {'error', _E}=E ->
            lager:debug("failed to retrieve slot: ~p", [_E]),
            _ = cleanup_slot(SlotNumber, ParkedCallId, kapps_call:account_db(Call)),
            E
    end.

retrieve_after_retries(SlotNumber, Slot, Call) ->
    ParkedCallId = kz_json:get_ne_binary_value(<<"Call-ID">>, Slot),
    lager:info("the parking slot ~s currently has a parked call ~s after 3 tries, attempting to retrieve caller"
              ,[SlotNumber, ParkedCallId]
              ),
    case retrieve_slot(ParkedCallId, Call) of
        'ok' -> {'ok', 'retrieved'};
        Error -> Error
    end.

-spec maybe_retrieve_slot(kz_json:object()) -> {'retry' | 'ok', kz_term:ne_binary()} | {'error', 'slot_empty'}.
maybe_retrieve_slot(Slot) ->
    ParkedCallId = kz_json:get_ne_binary_value(<<"Call-ID">>, Slot),
    case kz_json:is_true(<<"Attended">>, Slot) of
        'true' -> {'retry', ParkedCallId};
        'false' when ParkedCallId =/= 'undefined' -> {'ok', ParkedCallId};
        'false' -> {'error', 'slot_empty'}
    end.

-spec retrieve_slot(kz_term:ne_binary(), kapps_call:call()) ->
          'ok' |
          {'error', 'timeout' | 'failed'}.
retrieve_slot(ParkedCallId, Call) ->
    lager:info("retrieved parked call from slot, maybe bridging to caller ~s", [ParkedCallId]),
    _ = send_pickup(ParkedCallId, Call),
    wait_for_pickup(Call).

-spec send_pickup(kz_term:ne_binary(), kapps_call:call()) -> 'ok'.
send_pickup(ParkedCallId, Call) ->
    Req = [{<<"Unbridged-Only">>, 'true'}
          ,{<<"Application-Name">>, <<"call_pickup">>}
          ,{<<"Target-Call-ID">>, ParkedCallId}
          ,{<<"Continue-On-Fail">>, 'false'}
          ,{<<"Continue-On-Cancel">>, 'true'}
          ,{<<"Park-After-Pickup">>, 'false'}
          ],
    kapps_call_command:send_command(Req, Call).

-spec wait_for_pickup(kapps_call:call()) ->
          'ok' |
          {'error', 'timeout' | 'failed'}.
wait_for_pickup(Call) ->
    case kapps_call_command:receive_event(10 * ?MILLISECONDS_IN_SECOND) of
        {'ok', Evt} ->
            pickup_event(Call, kz_util:get_event_type(Evt), Evt);
        {'error', 'timeout'}=E ->
            lager:debug("timed out"),
            E
    end.

-spec pickup_event(kapps_call:call(), {kz_term:ne_binary(), kz_term:ne_binary()}, kz_call_event:doc()) ->
          'ok' |
          {'error', 'failed'}.
pickup_event(_Call, {<<"error">>, <<"dialplan">>}, _Evt) ->
    lager:debug("error in dialplan: ~s", [kz_call_event:error_message(_Evt)]),
    {'error', 'failed'};
pickup_event(_Call, {<<"call_event">>,<<"CHANNEL_BRIDGE">>}, _Evt) ->
    'ok' = lager:debug("channel bridged to ~s", [kz_call_event:other_leg_call_id(_Evt)]);
pickup_event(Call, _Type, _Evt) ->
    wait_for_pickup(Call).

%%------------------------------------------------------------------------------
%% @doc Determine the appropriate action to park the current call scenario
%% @end
%%------------------------------------------------------------------------------
-spec park_call(kz_term:ne_binary(), kz_json:object(), kz_json:object(), kz_term:api_binary(), kz_json:object(), kapps_call:call()) -> 'ok'.
park_call(SlotNumber, Slot, ParkedCalls, ReferredTo, Data, Call) ->
    lager:info("attempting to park call in slot ~s", [SlotNumber]),
    MaxSlotNumber = kz_json:get_integer_value(?MAX_SLOT_NUMBER_KEY, Data),
    case {ReferredTo, save_slot(SlotNumber, MaxSlotNumber, Slot, ParkedCalls, Call)} of
        %% attended transfer but the provided slot number is occupied, we are still connected to the 'parker'
        %% not the 'parkee'
        {'undefined', {'error', 'occupied'}} ->
            error_occupied_slot(Call);
        %% attended transfer and allowed to update the provided slot number, we are still connected to the 'parker'
        %% not the 'parkee'
        {'undefined', _} ->
            lager:info("playback slot number ~s to caller", [SlotNumber]),
            %% Update screen with new slot number
            _ = kapps_call_command:b_answer(Call),
            %% Caller parked in slot number...
            _ = kapps_call_command:b_prompt(<<"park-call_placed_in_spot">>, Call),
            _ = kapps_call_command:b_say(kz_term:to_binary(SlotNumber), Call),
            _ = wait_for_hangup(Call),
            _ = timer:apply_after(?PARK_DELAY_CHECK_TIME, ?MODULE, 'maybe_cleanup_slot', [SlotNumber, Call, cf_exe:callid(Call)]),
            cf_exe:transfer(Call);
        %% blind transfer and but the provided slot number is occupied
        {_, {'error', 'occupied'}} ->
            lager:info("blind transfer to a occupied slot, call the parker back.."),
            case ringback_parker(kz_json:get_ne_binary_value(<<"Ringback-ID">>, Slot), SlotNumber, Slot, Data, Call) of
                'answered' -> cf_exe:transfer(Call);
                'intercepted' -> cf_exe:transfer(Call);
                'channel_hungup' -> cf_exe:stop(Call);
                'failed' ->
                    kapps_call_command:hangup(Call),
                    cf_exe:stop(Call)
            end,
            'ok';
        %% blind transfer and allowed to update the provided slot number
        {_, {'ok', _}} -> parked_call(SlotNumber, Slot, Data, Call)
    end.

-spec parked_call(kz_term:ne_binary(), kz_json:object(), kz_json:object(), kapps_call:call()) -> 'ok'.
parked_call(SlotNumber, Slot, Data, Call) ->
    ParkedCallId = kz_json:get_ne_binary_value(<<"Call-ID">>, Slot),
    lager:info("call ~s parked in slot ~s", [ParkedCallId, SlotNumber]),
    _ = publish_parked(Call, SlotNumber),
    update_presence(Slot),
    wait_for_pickup(SlotNumber, Slot, Data, Call).

-spec wait_for_hangup(kapps_call:call()) -> {'ok', 'channel_hungup'} | {'error', 'timeout'}.
wait_for_hangup(Call) ->
    case cf_exe:is_channel_destroyed(Call) of
        'false' -> kapps_call_command:wait_for_hangup(?MILLISECONDS_IN_SECOND * 30);
        'true' -> {'ok', 'channel_hungup'}
    end.

%%------------------------------------------------------------------------------
%% @doc Builds the json object representing the call in the parking slot
%% @end
%%------------------------------------------------------------------------------
-spec create_slot(kz_term:api_ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_json:object(), boolean(), kapps_call:call()) -> kz_json:object().
create_slot(ParkerCallId, PresenceType, SlotNumber, Data, Attended, Call) ->
    CallId = cf_exe:callid(Call),
    RingbackId = maybe_get_ringback_id(Call),
    SlotCallId = kz_binary:rand_hex(16),
    User = slot_presence_id(SlotNumber, Data, Call),
    Realm = kapps_call:account_realm(Call),
    kz_json:from_list([{<<"Call-ID">>, CallId}
                      ,{<<"Attended">>, Attended}
                      ,{<<"Slot-Call-ID">>, SlotCallId}
                      ,{<<"Switch-URI">>, kapps_call:switch_uri(Call)}
                      ,{<<"From-Tag">>, kapps_call:from_tag(Call)}
                      ,{<<"To-Tag">>, kapps_call:to_tag(Call)}
                      ,{<<"Parker-Call-ID">>, ParkerCallId}
                      ,{<<"Ringback-ID">>, RingbackId}
                      ,{<<"Presence-User">>, User}
                      ,{<<"Presence-Realm">>, Realm}
                      ,{<<"Presence-ID">>, <<User/binary, "@", Realm/binary>>}
                      ,{<<"Node">>, kapps_call:switch_nodename(Call)}
                      ,{<<"CID-Number">>, kapps_call:caller_id_number(Call)}
                      ,{<<"CID-Name">>, kapps_call:caller_id_name(Call)}
                      ,{<<"CID-URI">>, kapps_call:from(Call)}
                      ,{<<"Hold-Media">>, kz_attributes:moh_attributes(RingbackId, <<"media_id">>, Call)}
                      ,{?PRESENCE_TYPE_KEY, PresenceType}
                      ]
                     ).

-spec slot_presence_id(kz_term:ne_binary(), kz_json:object(), kapps_call:call()) -> kz_term:ne_binary().
slot_presence_id(SlotNumber, Data, Call) ->
    case kz_json:is_true(<<"custom_presence_id">>, Data, 'false') of
        'true' -> maybe_custom_slot_presence_id(SlotNumber, Data, Call);
        'false' -> slot_presence_id(SlotNumber, Call)
    end.

-spec slot_presence_id(kz_term:ne_binary(), kapps_call:call()) -> kz_term:ne_binary().
slot_presence_id(SlotNumber, Call) ->
    User = kapps_call:request_user(Call),
    case kapps_call:kvs_fetch('cf_capture_group', <<>>, Call) of
        _ = ?NE_BINARY -> User;
        _Other -> <<User/binary, SlotNumber/binary>>
    end.

-spec maybe_custom_slot_presence_id(kz_term:ne_binary(), kz_json:object(), kapps_call:call()) -> kz_term:ne_binary().
maybe_custom_slot_presence_id(SlotNumber, Data, Call) ->
    case kz_json:get_ne_binary_value(<<"presence_id">>, slot_configuration(Data, SlotNumber)) of
        'undefined' -> maybe_custom_presence_id(Data, Call);
        PresenceId -> PresenceId
    end.

-spec maybe_custom_presence_id(kz_json:object(), kapps_call:call()) -> kz_term:ne_binary().
maybe_custom_presence_id(Data, Call) ->
    case kz_json:get_ne_binary_value(<<"presence_id">>, Data) of
        'undefined' -> kapps_call:request_user(Call);
        PresenceId -> PresenceId
    end.

%%------------------------------------------------------------------------------
%% @doc Returns the provided slot number or the next available if none
%% was provided
%% @end
%%------------------------------------------------------------------------------
-spec get_slot_number(kz_json:object(), kz_term:api_binary()) -> kz_term:ne_binary().
get_slot_number(_, ?NE_BINARY=CaptureGroup) ->
    CaptureGroup;
get_slot_number(ParkedCalls, _) ->
    Slots = [kz_term:to_integer(Slot)
             || Slot <- kz_json:get_keys(<<"slots">>, ParkedCalls)
            ],
    Sorted = ordsets:to_list(ordsets:from_list([100|Slots])),
    kz_term:to_binary(find_slot_number(Sorted)).

-spec find_slot_number([integer(),...]) -> integer().
find_slot_number([A]) -> A + 1;
find_slot_number([A|[B|_]=Slots]) ->
    case B =:= A + 1 of
        'false' -> A + 1;
        'true' -> find_slot_number(Slots)
    end.

%%------------------------------------------------------------------------------
%% @doc Save the slot data in the parked calls object at the slot number.
%% If, on save, it conflicts then it gets the new instance
%% and tries again, determining the new slot.
%% @end
%%------------------------------------------------------------------------------
-spec save_slot(kz_term:ne_binary(), kz_term:api_integer(), kz_json:object(), kz_json:object(), kapps_call:call()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
save_slot(SlotNumber, MaxSlotNumber, Slot, ParkedCalls, Call) ->
    try kz_term:to_integer(SlotNumber) of
        SlotNumberInt ->
            MaxExceeded = MaxSlotNumber =/= 'undefined'
                andalso SlotNumberInt > MaxSlotNumber,
            MaxExceeded
                andalso lager:info("no more slots available - max_slot_number (~b) has been exceeded", [MaxSlotNumber]),
            save_slot_check_max_slot_exceeded(SlotNumber, MaxExceeded, Slot, ParkedCalls, Call)
    catch
        'error':'badarg' ->
            save_slot(SlotNumber, Slot, ParkedCalls, Call)
    end.

-spec save_slot_check_max_slot_exceeded(kz_term:ne_binary(), boolean(), kz_json:object(), kz_json:object(), kapps_call:call()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
save_slot_check_max_slot_exceeded(_, 'true', _, _, _) ->
    {'error', ?MAX_SLOT_EXCEEDED};
save_slot_check_max_slot_exceeded(SlotNumber, 'false', Slot, ParkedCalls, Call) ->
    save_slot(SlotNumber, Slot, ParkedCalls, Call).

-spec save_slot(kz_term:ne_binary(), kz_json:object(), kz_json:object(), kapps_call:call()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
save_slot(SlotNumber, Slot, ParkedCalls, Call) ->
    ParkedCallId = kz_json:get_ne_binary_value([<<"slots">>, SlotNumber, <<"Call-ID">>], ParkedCalls),
    ParkerCallId = kz_json:get_ne_binary_value([<<"slots">>, SlotNumber, <<"Parker-Call-ID">>], ParkedCalls),
    case kz_term:is_empty(ParkedCallId)
        orelse ParkedCallId =:= ParkerCallId
    of
        'true' ->
            lager:info("slot has parked call '~s' by parker '~s', it is available", [ParkedCallId, ParkerCallId]),
            do_save_slot(SlotNumber, Slot, Call);
        'false' ->
            case kapps_call_command:b_channel_status(ParkedCallId) of
                {'ok', _} ->
                    lager:info("slot has active call '~s' in it, denying use of slot", [ParkedCallId]),
                    {'error', 'occupied'};
                _Else ->
                    lager:info("slot is availabled because parked call '~s' no longer exists: ~p", [ParkedCallId, _Else]),
                    do_save_slot(SlotNumber, Slot, Call)
            end
    end.

-spec do_save_slot(kz_term:ne_binary(), kz_json:object(), kapps_call:call()) ->
          {'ok', kz_json:object()} |
          {'error', atom()}.
do_save_slot(SlotNumber, Slot, Call) ->
    Doc = slot_doc(SlotNumber, Slot, Call),
    AccountDb = kapps_call:account_db(Call),
    CallId = kz_json:get_ne_binary_value(<<"Call-ID">>, Slot),
    lager:debug("attempting to update parked call document for slot ~s with call ~s", [SlotNumber, CallId]),
    case kz_datamgr:save_doc(AccountDb, Doc) of
        {'ok', _}=Ok ->
            lager:info("saved call parking data for slot ~s", [SlotNumber]),
            Ok;
        {'error', _Error} ->
            lager:info("error when attempting to store call parking data for slot ~s : ~p", [SlotNumber, _Error]),
            {'error', 'occupied'}
    end.

-spec slot_doc(kz_term:ne_binary(), kz_json:object(), kapps_call:call()) -> kz_json:object().
slot_doc(SlotNumber, Slot, Call) ->
    AccountDb = kapps_call:account_db(Call),
    Doc = case kz_json:get_json_value(<<"pvt_fields">>, Slot) of
              'undefined' -> kz_json:set_value(<<"slot">>, Slot, kz_json:new());
              Pvt -> kz_json:set_value(<<"slot">>, kz_doc:public_fields(Slot), Pvt)
          end,
    Options = [{'type', ?PARKED_CALL_DOC_TYPE}
              ,{'account_id', kapps_call:account_id(Call)}
              ,{'id', ?SLOT_DOC_ID(SlotNumber)}
              ],
    maybe_add_slot_doc_rev(kz_doc:update_pvt_parameters(Doc, AccountDb, Options), AccountDb).

-spec maybe_add_slot_doc_rev(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
maybe_add_slot_doc_rev(JObj, AccountDb) ->
    case kz_datamgr:lookup_doc_rev(AccountDb, kz_doc:id(JObj)) of
        {'ok', Rev} -> kz_json:set_value(<<"_rev">>, Rev, JObj);
        {'error', _} -> JObj
    end.

%%------------------------------------------------------------------------------
%% @doc After an attended transfer we need to find the callid that we stored
%% because it was the "C-Leg" of a transfer and now we have the
%% actual "A-Leg".  Find the old callid and update it with the new one.
%% @end
%%------------------------------------------------------------------------------
-spec update_call_id(kz_term:ne_binary(), kapps_call:call()) ->
          {'ok', kz_term:ne_binary(), kz_json:object()}.
update_call_id(Replaces, Call) ->
    CallId = cf_exe:callid(Call),
    lager:info("update parked call id ~s with new call id ~s", [Replaces, CallId]),
    AccountDb = kapps_call:account_db(Call),
    case kz_datamgr:get_result_doc(AccountDb, ?PARKED_CALL_VIEW, Replaces) of
        {'ok', Doc} ->
            ?SLOT_DOC_ID(SlotNumber) = kz_doc:id(Doc),
            lager:info("found parked call id ~s in slot ~s", [Replaces, SlotNumber]),
            _ = publish_parked(Call, SlotNumber),
            CallerNode = kapps_call:switch_nodename(Call),
            Updaters = [fun(J) -> kz_json:set_value(<<"Call-ID">>, CallId, J) end
                       ,fun(J) -> kz_json:set_value(<<"Node">>, CallerNode, J) end
                       ,fun(J) -> kz_json:set_value(<<"CID-Number">>, kapps_call:caller_id_number(Call), J) end
                       ,fun(J) -> kz_json:set_value(<<"CID-Name">>, kapps_call:caller_id_name(Call), J) end
                       ,fun(J) -> kz_json:set_value(<<"CID-URI">>, kapps_call:from(Call), J) end
                       ,fun(J) -> kz_json:set_value(<<"Attended">>, 'false', J) end
                       ,fun(J) -> maybe_set_hold_media(J, Call) end
                       ,fun(J) -> maybe_set_ringback_id(J, Call) end
                       ],
            Slot = kz_json:get_json_value(<<"slot">>, Doc),
            UpdatedSlot = lists:foldr(fun(F, J) -> F(J) end, Slot, Updaters),
            JObj = kz_json:set_value(<<"slot">>, UpdatedSlot, Doc),
            case kz_datamgr:save_doc(AccountDb, JObj) of
                {'ok', _} ->
                    update_presence(UpdatedSlot),
                    {'ok', SlotNumber, UpdatedSlot};
                {'error', _R} = E -> E
            end;
        {'error', _R} = E ->
            lager:info("failed to find parking slot with call id ~s: ~p", [Replaces, _R]),
            E
    end.

-spec maybe_set_ringback_id(kz_json:object(), kapps_call:call()) -> kz_json:object().
maybe_set_ringback_id(JObj, Call) ->
    case kz_json:get_ne_binary_value(<<"Ringback-ID">>, JObj) =:= 'undefined'
        andalso maybe_get_ringback_id(Call)
    of
        'undefined' -> JObj; %% no found ringback id
        'false' -> JObj; %% ringback on JObj
        RingbackId ->
            kz_json:set_value(<<"Ringback-ID">>, RingbackId, JObj)
    end.

-spec maybe_set_hold_media(kz_json:object(), kapps_call:call()) -> kz_json:object().
maybe_set_hold_media(JObj, Call) ->
    RingbackId = kz_json:get_ne_binary_value(<<"Ringback-ID">>, JObj),
    HoldMedia = kz_json:get_ne_binary_value(<<"Hold-Media">>, JObj),
    case RingbackId =/= 'undefined'
        andalso HoldMedia =:= 'undefined'
    of
        'false' -> JObj;
        'true' ->
            maybe_set_hold_media_from_ringback(JObj, Call, RingbackId)
    end.

-spec maybe_set_hold_media_from_ringback(kz_json:object(), kapps_call:call(), kz_term:ne_binary()) -> kz_json:object().
maybe_set_hold_media_from_ringback(JObj, Call, RingbackId) ->
    case kz_attributes:moh_attributes(RingbackId, <<"media_id">>, Call) of
        'undefined' -> JObj;
        RingbackHoldMedia ->
            kz_json:set_value(<<"Hold-Media">>, RingbackHoldMedia, JObj)
    end.

-spec maybe_get_ringback_id(kapps_call:call()) -> kz_term:api_binary().
maybe_get_ringback_id(Call) ->
    Referred = kapps_call:custom_channel_var(<<"Referred-By">>, Call),
    ReOptions = [{'capture', [1], 'binary'}],
    case catch(re:run(Referred, <<".*sip:(.*)@.*">>, ReOptions)) of
        {'match', [Match]} -> get_endpoint_id(Match, Call);
        _ -> 'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc Attempts to retrieve the parked calls list from the datastore, if
%% the list does not exist then it returns an new empty instance
%% @end
%%------------------------------------------------------------------------------
-spec get_parked_calls(kapps_call:call() | kz_term:ne_binary()) -> kz_json:object().
get_parked_calls(?NE_BINARY = AccountDb) ->
    Options = ['include_docs'
              ,{'doc_type', ?PARKED_CALL_DOC_TYPE}
              ],
    case kz_datamgr:get_results(AccountDb, ?PARKED_CALLS_VIEW, Options) of
        {'error', _} -> load_parked_calls([]);
        {'ok', JObjs} -> load_parked_calls(JObjs)
    end;
get_parked_calls(Call) ->
    get_parked_calls(kapps_call:account_db(Call)).

-spec load_parked_calls(kz_json:objects()) -> kz_json:object().
load_parked_calls(JObjs) ->
    Slots = [load_parked_call(JObj) || JObj <- JObjs],
    kz_json:from_list([{<<"slots">>, kz_json:from_list(Slots)}]).

-spec load_parked_call(kz_json:object()) -> {kz_term:ne_binary(), kz_json:object()}.
load_parked_call(JObj) ->
    Doc = kz_json:get_json_value(<<"doc">>, JObj),
    <<"parking-slot-", SlotNumber/binary>> = kz_doc:id(Doc),
    case kz_json:get_json_value(<<"slot">>, Doc) of
        'undefined' -> 'undefined';
        Slot -> {SlotNumber, kz_json:set_value(<<"pvt_fields">>, kz_doc:private_fields(Doc), Slot)}
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec maybe_cleanup_slot(kz_term:ne_binary(), kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
maybe_cleanup_slot(SlotNumber, Call, OldCallId) ->
    _ = kz_log:put_callid(OldCallId),
    ParkedCalls = get_parked_calls(Call),
    AccountDb   = kapps_call:account_db(Call),

    lager:info("maybe cleaning up parking slot ~p with old call-id ~p", [SlotNumber, OldCallId]),
    case kz_json:get_json_value([<<"slots">>, SlotNumber], ParkedCalls) of
        'undefined' ->
            lager:info("slot not found, not doing anything");
        Slot ->
            ParkedCallId = kz_json:get_ne_binary_value(<<"Call-ID">>, Slot),
            maybe_cleanup_slot(SlotNumber, OldCallId, ParkedCallId, AccountDb)
    end.

-spec maybe_cleanup_slot(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
maybe_cleanup_slot(SlotNumber, CallId, CallId, AccountDb) ->
    lager:info("callid (~p) in parking slot ~p has not changed, cleaning up...", [CallId, SlotNumber]),
    cleanup_slot(SlotNumber, CallId, AccountDb);

maybe_cleanup_slot(_SlotNumber, _OldCallId, _NewCallId, _AccountDb) ->
    lager:info("parking slot ~p call-id changed from ~p to ~p, not cleaning.", [_SlotNumber, _OldCallId, _NewCallId]).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec cleanup_slot(kz_term:ne_binary(), kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
cleanup_slot(SlotNumber, ParkedCallId, AccountDb) ->
    case kz_datamgr:open_doc(AccountDb, ?SLOT_DOC_ID(SlotNumber)) of
        {'ok', JObj} ->
            case kz_json:get_ne_binary_value([<<"slot">>, <<"Call-ID">>], JObj) of
                ParkedCallId ->
                    lager:info("delete parked call ~s in slot ~s", [ParkedCallId, SlotNumber]),
                    delete_slot(AccountDb, JObj);
                _Else ->
                    lager:info("call ~s is parked in slot ~s and we expected ~s", [_Else, SlotNumber, ParkedCallId]),
                    {'error', 'unexpected_callid'}
            end;
        {'error', _R}=E ->
            lager:info("failed to open the parked call doc ~s : ~p", [SlotNumber, _R]),
            E
    end.

-spec delete_slot(kz_term:ne_binary(), kz_json:object()) ->
          {'ok', kz_json:object()} |
          {'error', any()}.
delete_slot(AccountDb, JObj) ->
    case kz_datamgr:save_doc(AccountDb, kz_json:delete_key(<<"slot">>, JObj)) of
        {'ok', _}=Ok ->
            Slot = kz_json:get_json_value(<<"slot">>, JObj),
            update_presence(<<"terminated">>, Slot),
            Ok;
        {'error', _R}=E ->
            lager:info("failed to delete slot ~s : ~p", [kz_doc:id(JObj), _R]),
            E
    end.

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec wait_for_pickup(kz_term:ne_binary(), kz_json:object(), kz_json:object(), kapps_call:call()) -> 'ok'.
wait_for_pickup(SlotNumber, Slot, Data, Call) ->
    RingbackId = kz_json:get_ne_binary_value(<<"Ringback-ID">>, Slot),
    HoldMedia = kz_json:get_ne_binary_value(<<"Hold-Media">>, Slot),
    Timeout = case kz_term:is_empty(RingbackId) of
                  'true' -> 'infinity';
                  'false' -> ringback_timeout(Data, SlotNumber)
              end,
    lager:info("waiting '~p' for parked caller to be picked up or hangup", [Timeout]),
    kapps_call_command:hold(HoldMedia, Call),
    case kapps_call_command:wait_for_unparked_call(Call, Timeout) of
        {'error', 'timeout'} ->
            ChannelUp = case kapps_call_command:b_channel_status(Call) of
                            {'ok', _} -> 'true';
                            {'error', _} -> 'false'
                        end,
            case ChannelUp
                andalso ringback_parker(RingbackId, SlotNumber, Slot, Data, Call)
            of
                'false' ->
                    lager:info("parked call does not exist anymore, hangup"),
                    _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), kapps_call:account_db(Call)),
                    cf_exe:stop(Call);
                'intercepted' ->
                    lager:info("parked caller ringback was intercepted"),
                    _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), kapps_call:account_db(Call)),
                    _ = publish_retrieved(Call, SlotNumber),
                    cf_exe:transfer(Call);
                'answered' ->
                    lager:info("parked caller ringback was answered"),
                    _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), kapps_call:account_db(Call)),
                    _ = publish_retrieved(Call, SlotNumber),
                    wait_for_bridge(Call);
                'failed' ->
                    unanswered_action(SlotNumber, Slot, Data, Call);
                'channel_hungup' ->
                    lager:info("parked call does not exist anymore, hangup"),
                    _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), kapps_call:account_db(Call)),
                    cf_exe:stop(Call)
            end;
        {'error', 'channel_disconnected'} ->
            lager:info("parked caller has disconnected, checking status"),
            case kapps_call_command:b_channel_status(cf_exe:callid(Call)) of
                {'ok', _} ->
                    lager:info("call '~s' is still active", [cf_exe:callid(Call)]),
                    wait_for_pickup(SlotNumber, Slot, Data, Call);
                _Else ->
                    lager:info("call '~s' is no longer active, ", [cf_exe:callid(Call)]),
                    _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), kapps_call:account_db(Call)),
                    cf_exe:stop(Call)
            end;
        {'error', 'channel_hungup'} ->
            lager:info("parked caller hangup"),
            _ = publish_abandoned(Call, SlotNumber),
            _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), kapps_call:account_db(Call)),
            cf_exe:stop(Call);
        {'ok', _JObj} ->
            lager:info("parked caller has been picked up"),
            _ = publish_retrieved(Call, SlotNumber),
            _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), kapps_call:account_db(Call)),
            cf_exe:transfer(Call);
        _Else ->
            lager:info("unhandled case waiting for call pickup"),
            _ = publish_abandoned(Call, SlotNumber),
            _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), kapps_call:account_db(Call)),
            cf_exe:stop(Call)
    end.

-spec ringback_timeout(kz_json:object(), kz_term:ne_binary()) -> integer().
ringback_timeout(Data, SlotNumber) ->
    JObj = slot_configuration(Data, SlotNumber),
    DefaultRingbackTime = kz_json:get_integer_value(<<"default_ringback_timeout">>, Data, ?DEFAULT_RINGBACK_TM),
    kz_json:get_integer_value(<<"ringback_timeout">>, JObj, DefaultRingbackTime).

-spec callback_timeout(kz_json:object(), kz_term:ne_binary()) -> integer().
callback_timeout(Data, SlotNumber) ->
    JObj = slot_configuration(Data, SlotNumber),
    DefaultCallbackTime = kz_json:get_integer_value(<<"default_callback_timeout">>, Data, ?DEFAULT_CALLBACK_TM),
    kz_json:get_integer_value(<<"callback_timeout">>, JObj, DefaultCallbackTime).

-spec unanswered_action(kz_term:ne_binary(), kz_json:object(), kz_json:object(), kapps_call:call()) -> 'ok'.
unanswered_action(SlotNumber, Slot, Data, Call) ->
    case cf_exe:next(SlotNumber, Call) of
        'undefined' -> wait_for_pickup(SlotNumber, Slot, Data, Call);
        _ ->
            _ = publish_abandoned(Call, SlotNumber),
            _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), kapps_call:account_db(Call)),
            cf_exe:continue(SlotNumber, Call)
    end.

-spec presence_type(kz_term:ne_binary(), kz_json:object(), kapps_call:call()) -> kz_term:ne_binary().
presence_type(SlotNumber, Data, Call) ->
    JObj = slot_configuration(Data, SlotNumber),
    DefaultPresenceType =
        kz_json:get_ne_binary_value(<<"default_presence_type">>
                                   ,Data
                                   ,?ACCOUNT_PARKED_TYPE(kapps_call:account_id(Call))
                                   ),
    kz_json:get_ne_binary_value(<<"presence_type">>, JObj, DefaultPresenceType).

-spec slots_configuration(kz_json:object()) -> kz_json:object().
slots_configuration(Data) ->
    kz_json:get_json_value(<<"slots">>, Data, kz_json:new()).

-spec slot_configuration(kz_json:object(), kz_term:ne_binary()) -> kz_json:object().
slot_configuration(Data, SlotNumber) ->
    kz_json:get_json_value(SlotNumber, slots_configuration(Data), kz_json:new()).

%%------------------------------------------------------------------------------
%% @doc Ringback the device that parked the call
%% @end
%%------------------------------------------------------------------------------
-spec get_endpoint_id(kz_term:api_binary(), kapps_call:call()) -> kz_term:api_binary().
get_endpoint_id('undefined', _) -> 'undefined';
get_endpoint_id(Username, Call) ->
    AccountDb = kapps_call:account_db(Call),
    case cf_util:endpoint_id_by_sip_username(AccountDb, Username) of
        {'ok', EndpointId} -> EndpointId;
        {'error', _} -> 'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc Ringback the device that parked the call
%% @end
%%------------------------------------------------------------------------------
-type ringback_parker_result() :: 'answered' | 'intercepted' | 'failed' | 'channel_hungup'.

-spec ringback_parker(kz_term:api_binary(), kz_term:ne_binary(), kz_json:object(), kz_json:object(), kapps_call:call()) -> ringback_parker_result().
ringback_parker('undefined', _, _, _, _) -> 'failed';
ringback_parker(EndpointId, SlotNumber, Slot, Data, Call0) ->
    CalleeNumber = kz_json:get_value(<<"CID-Number">>, Slot),
    CalleeName = kz_json:get_value(<<"CID-Name">>, Slot),
    TmpCID = <<"Parking slot ", SlotNumber/binary, " - ", CalleeName/binary>>,

    Routines = [{fun kapps_call:kvs_store/3, 'dynamic_cid', {'undefined', TmpCID}}
               ,{fun kapps_call:kvs_store/3, 'force_dynamic_cid', 'true'}
               ,{fun kapps_call:set_callee_id_number/2, CalleeNumber}
               ,{fun kapps_call:set_callee_id_name/2, CalleeName}
               ],
    Call = kapps_call:exec(Routines, Call0),
    Timeout = callback_timeout(Data, SlotNumber),
    CVars = kz_json:from_list([{<<"Caller-ID-Name">>, CalleeName}]),
    case kz_endpoint:build(EndpointId, kz_json:from_list([{<<"can_call_self">>, 'true'}]), Call) of
        {'ok', [Endpoint]} ->
            lager:info("attempting to ringback endpoint ~s", [EndpointId]),
            EP = kz_json:set_value([<<"Endpoint-Actions">>
                                   ,<<"Execute-On-Answer">>
                                   ,<<"Set-Caller-ID">>
                                   ]
                                  ,set_command(CVars)
                                  ,Endpoint
                                  ),
            kapps_call_command:bridge([EP], Call),
            wait_for_ringback(Timeout, Call);
        _ -> 'failed'
    end.

-spec set_command(kz_json:object()) -> kz_json:object().
set_command(ChannelVars) ->
    Command = [{<<"Application-Name">>, <<"set">>}
              ,{<<"Custom-Channel-Vars">>, ChannelVars}
              ,{<<"Custom-Call-Vars">>, kz_json:new()}
              ,{<<"Call-ID">>, kz_binary:rand_hex(16)}
              ,{<<"Msg-ID">>, kz_binary:rand_hex(16)}
               | kz_api:default_headers(<<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    kz_json:from_list(Command).

-spec wait_for_ringback(timeout(), kapps_call:call()) -> ringback_parker_result().
wait_for_ringback(Timeout, Call) ->
    case wait_for_parker(Timeout, Call) of
        {'ok', JObj} ->
            case kz_api:event_name(JObj) of
                <<"CHANNEL_INTERCEPTED">> ->
                    lager:info("channel intercepted during ringback"),
                    'intercepted';
                _Else ->
                    lager:info("completed successful bridge to the ringback device"),
                    'answered'
            end;
        {'fail', JObj} ->
            case kz_api:event_name(JObj) of
                <<"CHANNEL_DESTROY">> ->
                    lager:info("channel hungup during ringback"),
                    'channel_hungup';
                _Else ->
                    lager:info("ringback failed, returning caller to parking slot: ~p", [_Else]),
                    'failed'
            end;
        _Else ->
            lager:info("ringback failed, returning caller to parking slot: ~p" , [_Else]),
            'failed'
    end.

-type wait_for_parker_result() :: {'ok' | 'fail' | 'error', kz_json:object()}.
-type receive_event_result() :: {'ok', kz_json:object()} | {'error', 'timeout'}.

-spec wait_for_parker(timeout(), kapps_call:call()) -> wait_for_parker_result().
wait_for_parker(Timeout, Call) ->
    Start = kz_time:start_time(),
    lager:debug("waiting for parker for ~p ms", [Timeout]),
    wait_for_parker(Timeout, Call, Start, kapps_call_command:receive_event(Timeout)).

-spec wait_for_parker(timeout(), kapps_call:call(), kz_time:start_time(), receive_event_result()) -> wait_for_parker_result().
wait_for_parker(_Timeout, _Call, _Start, {'error', 'timeout'}=E) -> E;
wait_for_parker(Timeout, Call, Start, {'ok', JObj}) ->
    Disposition = kz_json:get_value(<<"Disposition">>, JObj),
    Cause = kz_json:get_first_defined([<<"Application-Response">>
                                      ,<<"Hangup-Cause">>
                                      ], JObj, <<"UNSPECIFIED">>),
    Result = case Disposition =:= <<"SUCCESS">>
                 orelse Cause =:= <<"SUCCESS">>
             of
                 'true' -> 'ok';
                 'false' -> 'fail'
             end,
    case kapps_call_command:get_event_type(JObj) of
        {<<"error">>, _, <<"bridge">>} ->
            lager:debug("channel execution error while waiting for bridge: ~s", [kz_json:encode(JObj)]),
            {'error', JObj};
        {<<"call_event">>, <<"CHANNEL_DESTROY">>, _} ->
            lager:info("bridge channel destroy completed with result ~s(~s)", [Disposition, Result]),
            {Result, JObj};
        {<<"call_event">>, <<"CHANNEL_INTERCEPTED">>, _} ->
            lager:debug("ringback channel intercepted"),
            {'ok', JObj};
        {<<"call_event">>, <<"CHANNEL_BRIDGE">>, _} ->
            lager:debug("ringback channel bridged"),
            {'ok', JObj};
        {<<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>} ->
            lager:info("bridge execute completed with result ~s(~s)", [Disposition, Result]),
            {Result, JObj};
        _E ->
            NewTimeout = kz_time:decr_timeout(Timeout, Start),
            NewStart = kz_time:start_time(),
            wait_for_parker(NewTimeout, Call, NewStart, kapps_call_command:receive_event(NewTimeout))
    end.

expires(<<"early">>) -> 3600;
expires(<<"confirmed">>) -> 3600;
expires(<<"terminated">>) -> 10.

-spec update_presence(kz_term:api_object()) -> 'ok'.
update_presence('undefined') -> 'ok';
update_presence(Slot) ->
    update_presence(kz_json:get_ne_binary_value(?PRESENCE_TYPE_KEY, Slot, <<"early">>), Slot).

-spec update_presence(kz_term:ne_binary(), kz_term:api_object()) -> 'ok'.
update_presence(_State, 'undefined') -> 'ok';
update_presence(State, Slot) ->
    PresenceUser = kz_json:get_ne_binary_value(<<"Presence-User">>, Slot),
    PresenceRealm = kz_json:get_ne_binary_value(<<"Presence-Realm">>, Slot),
    PresenceId = <<PresenceUser/binary, "@", PresenceRealm/binary>>,
    PresenceURI = <<"sip:", PresenceId/binary>>,

    SwitchURI = kz_json:get_ne_binary_value(<<"Switch-URI">>, Slot),
    CallId = kz_json:get_ne_binary_value(<<"Call-ID">>, Slot),
    _SlotCallId = kz_json:get_ne_binary_value(<<"Slot-Call-ID">>, Slot),
    ToUser = kz_json:get_ne_binary_value(<<"CID-Name">>, Slot),
    To = kz_json:get_ne_binary_value(<<"CID-URI">>, Slot),
    Expires = expires(State),

    Command = props:filter_undefined(
                [{<<"Presence-ID">>, PresenceId}
                ,{<<"From">>, PresenceURI}
                ,{<<"From-User">>, PresenceUser}
                ,{<<"From-Realm">>, PresenceRealm}
                ,{<<"From-Tag">>, <<"A">>}

                ,{<<"To">>, To}
                ,{<<"To-User">>, ToUser}
                ,{<<"To-Realm">>, PresenceRealm}
                ,{<<"To-Tag">>, <<"B">>}
                ,{<<"To-URI">>, PresenceURI}

                ,{<<"State">>, State}
                ,{<<"Call-ID">>, CallId}
                ,{<<"Switch-URI">>, SwitchURI}
                ,{<<"Direction">>, <<"recipient">>}

                ,{<<"Expires">>, Expires}
                ,{<<"Event-Package">>, <<"dialog">>}

                 | kz_api:default_headers(?PARKING_APP_NAME, ?APP_VERSION)
                ]),
    lager:info("update presence-id '~s' with state: ~s", [PresenceId, State]),
    kz_amqp_worker:cast(Command, fun kapi_presence:publish_dialog/1).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec publish_parked(kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
publish_parked(Call, SlotNumber) ->
    publish_event(Call, SlotNumber, <<"PARK_PARKED">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec publish_retrieved(kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
publish_retrieved(Call, SlotNumber) ->
    publish_event(Call, SlotNumber, <<"PARK_RETRIEVED">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec publish_abandoned(kapps_call:call(), kz_term:ne_binary()) -> 'ok'.
publish_abandoned(Call, Slot) ->
    publish_event(Call, Slot, <<"PARK_ABANDONED">>).

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec publish_event(kapps_call:call(), kz_term:ne_binary(), kz_term:ne_binary()) -> 'ok'.
publish_event(Call, SlotNumber, Event) ->
    Cmd = [{<<"Call-ID">>, kapps_call:call_id(Call)}
          ,{<<"Callee-ID-Name">>, kapps_call:callee_id_name(Call)}
          ,{<<"Callee-ID-Number">>, kapps_call:callee_id_number(Call)}
          ,{<<"Caller-ID-Name">>, kapps_call:caller_id_name(Call)}
          ,{<<"Caller-ID-Number">>, kapps_call:caller_id_number(Call)}
          ,{<<"Custom-Channel-Vars">>, custom_channel_vars(Call)}
          ,{<<"Event-Name">>, Event}
          ,{<<"Parking-Slot">>, kz_term:to_binary(SlotNumber)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapi_call:publish_event(Cmd).

-spec custom_channel_vars(kapps_call:call()) -> kz_json:object().
custom_channel_vars(Call) ->
    JObj = kapps_call:custom_channel_vars(Call),
    Realm = kapps_call:account_realm(Call),
    kz_json:set_value(<<"Realm">>, Realm, JObj).

-spec get_slot(kz_term:ne_binary(), kz_term:ne_binary()) ->
          {'ok', kz_json:object()} |
          kz_datamgr:data_error() |
          {'error', 'not_occupied'}.
get_slot(SlotNumber, AccountDb) ->
    DocId = ?SLOT_DOC_ID(SlotNumber),
    case kz_datamgr:open_doc(AccountDb, {?PARKED_CALL_DOC_TYPE, DocId}) of
        {'ok', JObj} -> maybe_empty_slot(JObj);
        {'error', _} = E -> E
    end.

-spec maybe_empty_slot(kz_json:object()) -> {'ok', kz_json:object()} |
          {'error', 'not_occupied'}.
maybe_empty_slot(JObj) ->
    case kz_json:get_json_value(<<"slot">>, JObj) of
        'undefined' -> {'error', 'not_occupied'};
        Slot ->
            {'ok', kz_json:set_value(<<"pvt_fields">>
                                    ,kz_doc:private_fields(JObj)
                                    ,Slot
                                    )
            }
    end.

-spec error_occupied_slot(kapps_call:call()) -> 'ok'.
error_occupied_slot(Call) ->
    lager:info("selected slot is occupied"),
    %% Update screen with error that the slot is occupied
    _ = case kapps_call_command:b_answer(Call) of
            {'error', 'timeout'} ->
                lager:info("timed out waiting for the answer to complete");
            {'error', 'channel_hungup'} ->
                lager:info("channel hungup while answering");
            _ ->
                lager:debug("channel answered, prompting of the slot being in use"),
                %% playback message that caller will have to try a different slot
                kapps_call_command:b_prompt(<<"park-already_in_use">>, Call)
        end,
    cf_exe:stop(Call).

-spec wait_for_bridge(kapps_call:call()) -> 'ok'.
wait_for_bridge(Call) ->
    _ = kapps_call_command:wait_for_bridge('infinity', Call),
    cf_exe:continue(Call).
