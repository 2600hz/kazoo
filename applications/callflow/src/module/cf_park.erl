%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2016, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%   James Aimonetti
%%%-------------------------------------------------------------------
-module(cf_park).

-behaviour(gen_cf_action).

-include("callflow.hrl").

-export([handle/2]).
-export([update_presence/3]).

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".park">>).

-define(DB_DOC_NAME, kapps_config:get(?MOD_CONFIG_CAT, <<"db_doc_name">>, <<"parked_calls">>)).
-define(DEFAULT_RINGBACK_TM, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"default_ringback_timeout">>, 120000)).
-define(DEFAULT_CALLBACK_TM, kapps_config:get_integer(?MOD_CONFIG_CAT, <<"default_callback_timeout">>, 30000)).
-define(PARKED_CALLS_KEY(Db), {?MODULE, 'parked_calls', Db}).
-define(DEFAULT_PARKED_TYPE, <<"early">>).
-define(SYSTEM_PARKED_TYPE, kapps_config:get_ne_binary(?MOD_CONFIG_CAT, <<"parked_presence_type">>, ?DEFAULT_PARKED_TYPE)).
-define(ACCOUNT_PARKED_TYPE(A), kapps_account_config:get(A, ?MOD_CONFIG_CAT, <<"parked_presence_type">>, ?SYSTEM_PARKED_TYPE)).
-define(PRESENCE_TYPE_KEY, <<"Presence-Type">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%--------------------------------------------------------------------
-spec update_presence(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
update_presence(SlotNumber, _PresenceId, AccountDb) ->
    AccountId = kz_util:format_account_id(AccountDb, 'raw'),
    ParkedCalls = get_parked_calls(AccountDb, AccountId),
    case get_slot_call_id(SlotNumber, ParkedCalls) of
        'undefined' -> 'ok';
        ParkedCallId ->
            update_parked_call_presence(SlotNumber, get_slot(SlotNumber, ParkedCalls), ParkedCallId, AccountId)
    end.

-spec update_parked_call_presence(ne_binary(), kz_json:object(), ne_binary(), ne_binary()) -> 'ok'.
update_parked_call_presence(SlotNumber, Slot, ParkedCallId, AccountId) ->
    case kapps_call_command:b_channel_status(ParkedCallId) of
        {'ok', _Status} -> update_presence(Slot);
        {'error', _} -> cleanup_slot(SlotNumber, ParkedCallId, kz_util:format_account_db(AccountId))
    end.

-spec get_slot_call_id(kz_json:path(), kz_json:object()) -> api_binary().
get_slot_call_id(SlotNumber, ParkedCalls) ->
    kz_json:get_value([<<"slots">>, SlotNumber, <<"Call-ID">>], ParkedCalls).

-spec get_slot(kz_json:path(), kz_json:object()) -> api_object().
get_slot(SlotNumber, ParkedCalls) ->
    kz_json:get_value([<<"slots">>, SlotNumber], ParkedCalls).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%--------------------------------------------------------------------
-spec handle(kz_json:object(), kapps_call:call()) -> any().
handle(Data, Call) ->
    ParkedCalls = get_parked_calls(Call),
    AutoSlotNumber = get_slot_number(ParkedCalls, kapps_call:kvs_fetch('cf_capture_group', Call)),
    SlotNumber = kz_json:get_value(<<"slot">>, Data, AutoSlotNumber),
    ReferredTo = kapps_call:custom_channel_var(<<"Referred-To">>, <<>>, Call),
    PresenceType = presence_type(SlotNumber, Data, Call),

    case re:run(ReferredTo, "Replaces=([^;]*)", [{'capture', [1], 'binary'}]) of
        'nomatch' when ReferredTo =:= <<>> ->
            handle_nomatch_with_empty_referred_to(Data, Call, PresenceType, ParkedCalls, SlotNumber);
        'nomatch' ->
            handle_nomatch(Data, Call, PresenceType, ParkedCalls, SlotNumber, ReferredTo);
        {'match', [Replaces]} ->
            handle_replaces(Data, Call, Replaces, ParkedCalls)
    end.

-spec handle_replaces(kz_json:object(), kapps_call:call(), ne_binary(), kz_json:object()) ->
                             'ok' |
                             {'error', 'timeout' | 'failed'}.
handle_replaces(Data, Call, Replaces, ParkedCalls) ->
    lager:info("call was the result of an attended-transfer completion, updating call id"),
    {'ok', FoundInSlotNumber, Slot} = update_call_id(Replaces, ParkedCalls, Call),
    wait_for_pickup(FoundInSlotNumber, Slot, Data, Call).

-spec handle_nomatch(kz_json:object(), kapps_call:call(), ne_binary(), kz_json:object(), ne_binary(), ne_binary()) -> 'ok'.
handle_nomatch(Data, Call, PresenceType, ParkedCalls, SlotNumber, ReferredTo) ->
    lager:info("call was the result of a blind transfer, assuming intention was to park"),
    Slot = create_slot('undefined', PresenceType, Call),
    park_call(SlotNumber, Slot, ParkedCalls, ReferredTo, Data, Call).

-spec handle_nomatch_with_empty_referred_to(kz_json:object(), kapps_call:call(), ne_binary(), kz_json:object(), ne_binary()) -> 'ok'.
handle_nomatch_with_empty_referred_to(Data, Call, PresenceType, ParkedCalls, SlotNumber) ->
    lager:info("call was the result of a direct dial"),
    case kz_json:get_value(<<"action">>, Data, <<"park">>) of
        <<"park">> ->
            lager:info("action is to park the call"),
            Slot = create_slot(<<>>, PresenceType, Call),
            park_call(SlotNumber, Slot, ParkedCalls, 'undefined', Data, Call);
        <<"retrieve">> ->
            lager:info("action is to retrieve a parked call"),
            case retrieve(SlotNumber, ParkedCalls, Call) of
                {'ok', _} -> 'ok';
                _Else ->
                    _ = kapps_call_command:b_answer(Call),
                    _ = kapps_call_command:b_prompt(<<"park-no_caller">>, Call),
                    cf_exe:stop(Call)
            end;
        <<"auto">> ->
            lager:info("action is to automatically determine if we should retrieve or park"),
            Slot = create_slot(cf_exe:callid(Call), PresenceType, Call),
            case retrieve(SlotNumber, ParkedCalls, Call) of
                {'error', _} -> park_call(SlotNumber, Slot, ParkedCalls, 'undefined', Data, Call);
                {'ok', _} -> cf_exe:transfer(Call)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the appropriate action to retrieve a parked call
%% @end
%%--------------------------------------------------------------------
-spec retrieve(ne_binary(), kz_json:object(), kapps_call:call()) ->
                      {'ok', 'channel_hungup'} |
                      {'error', 'slot_empty' | 'timeout' | 'failed'}.
retrieve(SlotNumber, ParkedCalls, Call) ->
    case kz_json:get_value([<<"slots">>, SlotNumber], ParkedCalls) of
        'undefined' ->
            lager:info("the parking slot ~s is empty, unable to retrieve caller", [SlotNumber]),
            {'error', 'slot_empty'};
        Slot ->
            ParkedCall = kz_json:get_ne_value(<<"Call-ID">>, Slot),
            lager:info("the parking slot ~s currently has a parked call ~s, attempting to retrieve caller", [SlotNumber, ParkedCall]),
            case maybe_retrieve_slot(SlotNumber, Slot, ParkedCall, Call) of
                'ok' ->
                    _ = publish_retrieved(Call, SlotNumber),
                    _ = cleanup_slot(SlotNumber, ParkedCall, kapps_call:account_db(Call)),
                    kapps_call_command:wait_for_hangup();
                {'error', _E}=E ->
                    update_presence(<<"terminated">>, Slot),
                    lager:debug("failed to retrieve slot: ~p", [_E]),
                    E
            end
    end.

-spec maybe_retrieve_slot(ne_binary(), kz_json:object(), ne_binary(), kapps_call:call()) ->
                                 'ok' |
                                 {'error', 'timeout' | 'failed'}.
maybe_retrieve_slot(SlotNumber, Slot, ParkedCall, Call) ->
    lager:info("retrieved parked call from slot, maybe bridging to caller ~s", [ParkedCall]),
    %% publish_usurp_control(ParkedCall, Call),
    Name = kz_json:get_value(<<"CID-Name">>, Slot, <<"Parking Slot ", SlotNumber/binary>>),
    Number = kz_json:get_value(<<"CID-Number">>, Slot, SlotNumber),
    Update = [{<<"Callee-ID-Name">>, Name}
             ,{<<"Callee-ID-Number">>, Number}
             ],
    _ = kapps_call_command:set(kz_json:from_list(Update), 'undefined', Call),
    _ = send_pickup(ParkedCall, Call),
    wait_for_pickup(Call).

-spec send_pickup(ne_binary(), kapps_call:call()) -> 'ok'.
send_pickup(ParkedCall, Call) ->
    Req = [{<<"Unbridged-Only">>, 'true'}
          ,{<<"Application-Name">>, <<"call_pickup">>}
          ,{<<"Target-Call-ID">>, ParkedCall}
          ,{<<"Continue-On-Fail">>, 'false'}
          ,{<<"Continue-On-Cancel">>, 'true'}
          ,{<<"Park-After-Pickup">>, 'false'}
          ],
    kapps_call_command:send_command(Req, Call).

-spec wait_for_pickup(kapps_call:call()) ->
                             'ok' |
                             {'error', 'timeout' | 'failed'}.
wait_for_pickup(Call) ->
    case kapps_call_command:receive_event(10000) of
        {'ok', Evt} ->
            pickup_event(Call, kz_util:get_event_type(Evt), Evt);
        {'error', 'timeout'}=E ->
            lager:debug("timed out"),
            E
    end.

-spec pickup_event(kapps_call:call(), {ne_binary(), ne_binary()}, kz_json:object()) ->
                          'ok' |
                          {'error', 'failed'}.
pickup_event(_Call, {<<"error">>, <<"dialplan">>}, Evt) ->
    lager:debug("error in dialplan: ~s", [kz_json:get_value(<<"Error-Message">>, Evt)]),
    {'error', 'failed'};
pickup_event(_Call, {<<"call_event">>,<<"CHANNEL_BRIDGE">>}, _Evt) ->
    lager:debug("channel bridged to ~s", [kz_json:get_value(<<"Other-Leg-Call-ID">>, _Evt)]);
pickup_event(Call, _Type, _Evt) ->
    wait_for_pickup(Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the appropriate action to park the current call scenario
%% @end
%%--------------------------------------------------------------------
-spec park_call(ne_binary(), kz_json:object(), kz_json:object(), api_binary(), kz_json:object(), kapps_call:call()) -> 'ok'.
park_call(SlotNumber, Slot, ParkedCalls, ReferredTo, Data, Call) ->
    lager:info("attempting to park call in slot ~s", [SlotNumber]),
    case {ReferredTo, save_slot(SlotNumber, Slot, ParkedCalls, Call)} of
        %% attended transfer but the provided slot number is occupied, we are still connected to the 'parker'
        %% not the 'parkee'
        {'undefined', {'error', 'occupied'}} ->
            lager:info("selected slot is occupied"),
            %% Update screen with error that the slot is occupied
            _ = kapps_call_command:b_answer(Call),
            %% playback message that caller will have to try a different slot
            _ = kapps_call_command:b_prompt(<<"park-already_in_use">>, Call),
            cf_exe:stop(Call),
            'ok';
        %% attended transfer and allowed to update the provided slot number, we are still connected to the 'parker'
        %% not the 'parkee'
        {'undefined', _} ->
            lager:info("playback slot number ~s to caller", [SlotNumber]),
            %% Update screen with new slot number
            _ = kapps_call_command:b_answer(Call),
            %% Caller parked in slot number...
            _ = kapps_call_command:b_prompt(<<"park-call_placed_in_spot">>, Call),
            _ = kapps_call_command:b_say(kz_util:to_binary(SlotNumber), Call),
            cf_exe:transfer(Call);
        %% blind transfer and but the provided slot number is occupied
        {_, {'error', 'occupied'}} ->
            lager:info("blind transfer to a occupied slot, call the parker back.."),
            TmpCID = <<"Parking slot ", SlotNumber/binary, " occupied">>,
            case ringback_parker(kz_json:get_value(<<"Ringback-ID">>, Slot), SlotNumber, TmpCID, Data, Call) of
                'answered' -> cf_exe:transfer(Call);
                'channel_hungup' -> cf_exe:stop(Call);
                'failed' ->
                    kapps_call_command:hangup(Call),
                    cf_exe:stop(Call)
            end,
            'ok';
        %% blind transfer and allowed to update the provided slot number
        {_, {'ok', _}} ->
            ParkedCallId = kz_json:get_value(<<"Call-ID">>, Slot),
            lager:info("call ~s parked in slot ~s", [ParkedCallId, SlotNumber]),
            _ = publish_parked(Call, SlotNumber),
            update_presence(Slot),
            wait_for_pickup(SlotNumber, Slot, Data, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the json object representing the call in the parking slot
%% @end
%%--------------------------------------------------------------------
-spec create_slot(api_binary(), ne_binary(), kapps_call:call()) -> kz_json:object().
create_slot(ParkerCallId, PresenceType, Call) ->
    CallId = cf_exe:callid(Call),
    AccountDb = kapps_call:account_db(Call),
    AccountId = kapps_call:account_id(Call),
    RingbackId = maybe_get_ringback_id(Call),
    SlotCallId = kz_util:rand_hex_binary(16),
    kz_json:from_list(
      props:filter_undefined(
        [{<<"Call-ID">>, CallId}
        ,{<<"Slot-Call-ID">>, SlotCallId}
        ,{<<"Switch-URI">>, kapps_call:switch_uri(Call)}
        ,{<<"From-Tag">>, kapps_call:from_tag(Call)}
        ,{<<"To-Tag">>, kapps_call:to_tag(Call)}
        ,{<<"Parker-Call-ID">>, ParkerCallId}
        ,{<<"Ringback-ID">>, RingbackId}
        ,{<<"Presence-ID">>, <<(kapps_call:request_user(Call))/binary
                               ,"@", (kz_util:get_account_realm(AccountDb, AccountId))/binary
                             >>
         }
        ,{<<"Node">>, kapps_call:switch_nodename(Call)}
        ,{<<"CID-Number">>, kapps_call:caller_id_number(Call)}
        ,{<<"CID-Name">>, kapps_call:caller_id_name(Call)}
        ,{<<"CID-URI">>, kapps_call:from(Call)}
        ,{<<"Hold-Media">>, kz_attributes:moh_attributes(RingbackId, <<"media_id">>, Call)}
        ,{?PRESENCE_TYPE_KEY, PresenceType}
        ])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the provided slot number or the next available if none
%% was provided
%% @end
%%--------------------------------------------------------------------
-spec get_slot_number(kz_json:object(), kapps_call:call()) -> ne_binary().
get_slot_number(_, CaptureGroup) when byte_size(CaptureGroup) > 0 ->
    CaptureGroup;
get_slot_number(ParkedCalls, _) ->
    Slots = [kz_util:to_integer(Slot)
             || Slot <- kz_json:get_keys(<<"slots">>, ParkedCalls)
            ],
    Sorted = ordsets:to_list(ordsets:from_list([100|Slots])),
    kz_util:to_binary(find_slot_number(Sorted)).

-spec find_slot_number([integer(),...]) -> integer().
find_slot_number([A]) -> A + 1;
find_slot_number([A|[B|_]=Slots]) ->
    case B =:= A + 1 of
        'false' -> A + 1;
        'true' -> find_slot_number(Slots)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Save the slot data in the parked calls object at the slot number.
%% If, on save, it conflicts then it gets the new instance
%% and tries again, determining the new slot.
%% @end
%%--------------------------------------------------------------------
-spec save_slot(ne_binary(), kz_json:object(), kz_json:object(), kapps_call:call()) ->
                       {'ok', kz_json:object()} |
                       {'error', atom()}.
save_slot(SlotNumber, Slot, ParkedCalls, Call) ->
    ParkedCallId = kz_json:get_ne_value([<<"slots">>, SlotNumber, <<"Call-ID">>], ParkedCalls),
    ParkerCallId = kz_json:get_ne_value([<<"slots">>, SlotNumber, <<"Parker-Call-ID">>], ParkedCalls),
    case kz_util:is_empty(ParkedCallId)
        orelse ParkedCallId =:= ParkerCallId
    of
        'true' ->
            lager:info("slot has parked call '~s' by parker '~s', it is available", [ParkedCallId, ParkerCallId]),
            do_save_slot(SlotNumber, Slot, ParkedCalls, Call);
        'false' ->
            case kapps_call_command:b_channel_status(ParkedCallId) of
                {'ok', _} ->
                    lager:info("slot has active call '~s' in it, denying use of slot", [ParkedCallId]),
                    {'error', 'occupied'};
                _Else ->
                    lager:info("slot is availabled because parked call '~s' no longer exists: ~p", [ParkedCallId, _Else]),
                    do_save_slot(SlotNumber, Slot, ParkedCalls, Call)
            end
    end.

-spec do_save_slot(ne_binary(), kz_json:object(), kz_json:object(), kapps_call:call()) ->
                          {'ok', kz_json:object()} |
                          {'error', atom()}.
do_save_slot(SlotNumber, Slot, ParkedCalls, Call) ->
    AccountDb = kapps_call:account_db(Call),
    CallId = kz_json:get_value(<<"Call-ID">>, Slot),
    case kapps_call_command:b_channel_status(CallId) of
        {'ok', _} ->
            lager:debug("attempting to update parked calls document for slot ~s with call ~s", [SlotNumber, CallId]),
            case kz_datamgr:save_doc(AccountDb, kz_json:set_value([<<"slots">>, SlotNumber], Slot, ParkedCalls)) of
                {'ok', JObj}=Ok ->
                    lager:info("successfully stored call parking data for slot ~s", [SlotNumber]),
                    CacheProps = [{'origin', {'db', AccountDb, ?DB_DOC_NAME}}],
                    kz_cache:store_local(?CACHE_NAME, ?PARKED_CALLS_KEY(AccountDb), JObj, CacheProps),
                    Ok;
                {'error', 'conflict'} ->
                    maybe_resolve_conflict(SlotNumber, Slot, ParkedCalls, Call)
            end;
        _Else ->
            lager:debug("ignoring attempt to park terminated call ~s", [CallId]),
            {'error', 'terminated'}
    end.

-spec maybe_resolve_conflict(ne_binary(), kz_json:object(), kz_json:object(), kapps_call:call()) ->
                                    {'ok', kz_json:object()} |
                                    {'error', atom()}.
maybe_resolve_conflict(SlotNumber, Slot, ParkedCalls, Call) ->
    AccountDb = kapps_call:account_db(Call),
    ExpectedParkedCall = kz_json:get_ne_value([<<"slots">>, SlotNumber, <<"Call-ID">>], ParkedCalls),
    {'ok', JObj1} = kz_datamgr:open_doc(AccountDb, ?DB_DOC_NAME),
    case kz_json:get_ne_value([<<"slots">>, SlotNumber, <<"Call-ID">>], JObj1) of
        ExpectedParkedCall ->
            UpdatedJObj = kz_json:set_value([<<"slots">>, SlotNumber], Slot, JObj1),
            {'ok', JObj2}=Ok = kz_datamgr:save_doc(AccountDb, UpdatedJObj),
            lager:info("conflict when attempting to store call parking data for slot ~s due to a different slot update", [SlotNumber]),
            CacheProps = [{'origin', {'db', AccountDb, ?DB_DOC_NAME}}],
            kz_cache:store_local(?CACHE_NAME, ?PARKED_CALLS_KEY(AccountDb), JObj2, CacheProps),
            Ok;
        CurrentParkedCall ->
            lager:debug("attempt to store parking data conflicted with a recent update to slot ~s", [SlotNumber]),
            CacheProps = [{'origin', {'db', AccountDb, ?DB_DOC_NAME}}],
            kz_cache:store_local(?CACHE_NAME, ?PARKED_CALLS_KEY(AccountDb), JObj1, CacheProps),
            case kapps_call_command:b_channel_status(CurrentParkedCall) of
                {'ok', _} ->
                    lager:debug("slot ~s is now occupied by ~s", [SlotNumber, CurrentParkedCall]),
                    {'error', 'occupied'};
                _Else ->
                    lager:info("slot ~s was updated to inactive call ~s", [SlotNumber, CurrentParkedCall]),
                    save_slot(SlotNumber, Slot, JObj1, Call)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% After an attended transfer we need to find the callid that we stored
%% because it was the "C-Leg" of a transfer and now we have the
%% actuall "A-Leg".  Find the old callid and update it with the new one.
%% @end
%%--------------------------------------------------------------------
-spec update_call_id(ne_binary(), kz_json:object(), kapps_call:call()) ->
                            {'ok', ne_binary(), kz_json:object()}.
update_call_id(Replaces, ParkedCalls, Call) ->
    update_call_id(Replaces, ParkedCalls, Call, 0).

update_call_id(_, _, _, Loops) when Loops > 5 ->
    lager:info("unable to update parked call id after ~p tries", [Loops]),
    {'error', 'update_failed'};
update_call_id(Replaces, ParkedCalls, Call, Loops) ->
    CallId = cf_exe:callid(Call),
    lager:info("update parked call id ~s with new call id ~s", [Replaces, CallId]),
    Slots = kz_json:get_value(<<"slots">>, ParkedCalls, kz_json:new()),
    case find_slot_by_callid(Slots, Replaces) of
        {'ok', SlotNumber, Slot} ->
            lager:info("found parked call id ~s in slot ~s", [Replaces, SlotNumber]),
            _ = publish_parked(Call, SlotNumber),
            CallerNode = kapps_call:switch_nodename(Call),
            Updaters = [fun(J) -> kz_json:set_value(<<"Call-ID">>, CallId, J) end
                       ,fun(J) -> kz_json:set_value(<<"Node">>, CallerNode, J) end
                       ,fun(J) -> kz_json:set_value(<<"CID-Number">>, kapps_call:caller_id_number(Call), J) end
                       ,fun(J) -> kz_json:set_value(<<"CID-Name">>, kapps_call:caller_id_name(Call), J) end
                       ,fun(J) -> kz_json:set_value(<<"CID-URI">>, kapps_call:from(Call), J) end
                       ,fun(J) -> maybe_set_hold_media(J, Call) end
                       ,fun(J) -> maybe_set_ringback_id(J, Call) end
                       ],
            UpdatedSlot = lists:foldr(fun(F, J) -> F(J) end, Slot, Updaters),
            JObj = kz_json:set_value([<<"slots">>, SlotNumber], UpdatedSlot, ParkedCalls),
            case kz_datamgr:save_doc(kapps_call:account_db(Call), JObj) of
                {'ok', _} ->
                    update_presence(UpdatedSlot),
                    {'ok', SlotNumber, UpdatedSlot};
                {'error', 'conflict'} ->
                    AccountDb = kapps_call:account_db(Call),
                    kz_cache:erase_local(?CACHE_NAME, ?PARKED_CALLS_KEY(AccountDb)),
                    update_call_id(Replaces, get_parked_calls(Call), Call);
                {'error', _R} ->
                    lager:info("failed to update parking slot with call id ~s: ~p", [Replaces, _R]),
                    timer:sleep(250),
                    update_call_id(Replaces, get_parked_calls(Call), Call, Loops + 1)
            end;
        {'error', _R} ->
            lager:info("failed to find parking slot with call id ~s: ~p", [Replaces, _R]),
            timer:sleep(250),
            update_call_id(Replaces, get_parked_calls(Call), Call, Loops + 1)
    end.

-spec maybe_set_ringback_id(kz_json:object(), kapps_call:call()) -> kz_json:object().
maybe_set_ringback_id(JObj, Call) ->
    case kz_json:get_value(<<"Ringback-ID">>, JObj) =:= 'undefined'
        andalso maybe_get_ringback_id(Call)
    of
        'undefined' -> JObj; %% no found ringback id
        'false' -> JObj; %% ringback on JObj
        RingbackId ->
            kz_json:set_value(<<"Ringback-ID">>, RingbackId, JObj)
    end.

-spec maybe_set_hold_media(kz_json:object(), kapps_call:call()) -> kz_json:object().
maybe_set_hold_media(JObj, Call) ->
    RingbackId = kz_json:get_value(<<"Ringback-ID">>, JObj),
    HoldMedia = kz_json:get_value(<<"Hold-Media">>, JObj),
    case RingbackId =/= 'undefined'
        andalso HoldMedia =:= 'undefined'
    of
        'false' -> JObj;
        'true' ->
            maybe_set_hold_media_from_ringback(JObj, Call, RingbackId)
    end.

-spec maybe_set_hold_media_from_ringback(kz_json:object(), kapps_call:call(), ne_binary()) -> kz_json:object().
maybe_set_hold_media_from_ringback(JObj, Call, RingbackId) ->
    case kz_attributes:moh_attributes(RingbackId, <<"media_id">>, Call) of
        'undefined' -> JObj;
        RingbackHoldMedia ->
            kz_json:set_value(<<"Hold-Media">>, RingbackHoldMedia, JObj)
    end.

-spec maybe_get_ringback_id(kapps_call:call()) -> api_binary().
maybe_get_ringback_id(Call) ->
    Referred = kapps_call:custom_channel_var(<<"Referred-By">>, Call),
    ReOptions = [{'capture', [1], 'binary'}],
    case catch(re:run(Referred, <<".*sip:(.*)@.*">>, ReOptions)) of
        {'match', [Match]} -> get_endpoint_id(Match, Call);
        _ -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given the parked calls and a list of parked keys find the slot with
%% the provided call id.
%% @end
%%--------------------------------------------------------------------
-spec find_slot_by_callid(kz_json:object(), ne_binary()) ->
                                 {'ok', ne_binary(), kz_json:object()} |
                                 {'error', 'not_found'}.
find_slot_by_callid(Slots, CallId) ->
    find_slot_by_callid(kz_json:get_keys(Slots), Slots, CallId).

-spec find_slot_by_callid(ne_binaries(), kz_json:object(), ne_binary()) ->
                                 {'ok', ne_binary(), kz_json:object()} |
                                 {'error', 'not_found'}.
find_slot_by_callid([], _, _) ->
    {'error', 'not_found'};
find_slot_by_callid([SlotNumber|SlotNumbers], Slots, CallId) ->
    Slot = kz_json:get_value(SlotNumber, Slots),
    case kz_json:get_value(<<"Call-ID">>, Slot) of
        CallId -> {'ok', SlotNumber, Slot};
        _ ->
            case kz_json:get_value(<<"Slot-Call-ID">>, Slot) of
                CallId -> {'ok', SlotNumber, Slot};
                _ -> find_slot_by_callid(SlotNumbers, Slots, CallId)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to retrieve the parked calls list from the datastore, if
%% the list does not exist then it returns an new empty instance
%% @end
%%--------------------------------------------------------------------
-spec get_parked_calls(kapps_call:call()) -> kz_json:object().
get_parked_calls(Call) ->
    get_parked_calls(kapps_call:account_db(Call), kapps_call:account_id(Call)).

-spec get_parked_calls(ne_binary(), ne_binary()) -> kz_json:object().
get_parked_calls(AccountDb, AccountId) ->
    case kz_cache:peek_local(?CACHE_NAME, ?PARKED_CALLS_KEY(AccountDb)) of
        {'ok', JObj} -> JObj;
        {'error', 'not_found'} ->
            fetch_parked_calls(AccountDb, AccountId)
    end.

-spec fetch_parked_calls(ne_binary(), ne_binary()) -> kz_json:object().
fetch_parked_calls(AccountDb, AccountId) ->
    case kz_datamgr:open_doc(AccountDb, ?DB_DOC_NAME) of
        {'error', 'not_found'} ->
            Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            Generators = [fun(J) -> kz_doc:set_id(J, <<"parked_calls">>) end
                         ,fun(J) -> kz_doc:set_type(J, <<"parked_calls">>) end
                         ,fun(J) -> kz_doc:set_account_id(J, AccountId) end
                         ,fun(J) -> kz_doc:set_account_db(J, AccountDb) end
                         ,fun(J) -> kz_doc:set_created(J, Timestamp) end
                         ,fun(J) -> kz_doc:set_modified(J, Timestamp) end
                         ,fun(J) -> kz_doc:set_vsn(J, <<"1">>) end
                         ,fun(J) -> kz_json:set_value(<<"slots">>, kz_json:new(), J) end],
            lists:foldr(fun(F, J) -> F(J) end, kz_json:new(), Generators);
        {'ok', JObj} ->
            JObj;
        {'error', _R}=E ->
            lager:info("unable to get parked calls: ~p", [_R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup_slot(ne_binary(), ne_binary(), ne_binary()) ->
                          {'ok', kz_json:object()} |
                          {'error', any()}.
cleanup_slot(SlotNumber, ParkedCallId, AccountDb) ->
    case kz_datamgr:open_doc(AccountDb, ?DB_DOC_NAME) of
        {'ok', JObj} ->
            case kz_json:get_value([<<"slots">>, SlotNumber, <<"Call-ID">>], JObj) of
                ParkedCallId ->
                    lager:info("delete parked call ~s in slot ~s", [ParkedCallId, SlotNumber]),
                    case kz_datamgr:save_doc(AccountDb, kz_json:delete_key([<<"slots">>, SlotNumber], JObj)) of
                        {'ok', _}=Ok ->
                            Slot = kz_json:get_value([<<"slots">>, SlotNumber], JObj),
                            update_presence(<<"terminated">>, Slot),
                            Ok;
                        {'error', 'conflict'} -> cleanup_slot(SlotNumber, ParkedCallId, AccountDb);
                        {'error', _R}=E ->
                            lager:info("failed to delete slot: ~p", [_R]),
                            E
                    end;
                _Else ->
                    lager:info("call ~s is parked in slot ~s and we expected ~s", [_Else, SlotNumber, ParkedCallId]),
                    {'error', 'unexpected_callid'}
            end;
        {'error', _R}=E ->
            lager:info("failed to open the parked calls doc: ~p", [_R]),
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec wait_for_pickup(ne_binary(), kz_json:object(), kz_json:object(), kapps_call:call()) -> 'ok'.
wait_for_pickup(SlotNumber, Slot, Data, Call) ->
    RingbackId = kz_json:get_value(<<"Ringback-ID">>, Slot),
    HoldMedia = kz_json:get_value(<<"Hold-Media">>, Slot),
    Timeout = case kz_util:is_empty(RingbackId) of
                  'true' -> 'infinity';
                  'false' -> ringback_timeout(Data, SlotNumber)
              end,
    lager:info("waiting '~p' for parked caller to be picked up or hangup", [Timeout]),
    kapps_call_command:hold(HoldMedia, Call),
    case kapps_call_command:wait_for_unparked_call(Call, Timeout) of
        {'error', 'timeout'} ->
            TmpCID = <<"Parking slot ", SlotNumber/binary>>,
            ChannelUp = case kapps_call_command:b_channel_status(Call) of
                            {'ok', _} -> 'true';
                            {'error', _} -> 'false'
                        end,
            case ChannelUp
                andalso ringback_parker(RingbackId, SlotNumber, TmpCID, Data, Call)
            of
                'answered' ->
                    lager:info("parked caller ringback was answered"),
                    _ = publish_retrieved(Call, SlotNumber),
                    cf_exe:transfer(Call);
                'failed' ->
                    unanswered_action(SlotNumber, Slot, Data, Call);
                _Else ->
                    lager:info("parked call doesnt exist anymore, hangup"),
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
                    cf_exe:transfer(Call)
            end;
        _Else ->
            lager:info("parked caller has been picked up"),
            _ = publish_abandoned(Call, SlotNumber),
            _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), kapps_call:account_db(Call)),
            cf_exe:transfer(Call)
    end.

-spec ringback_timeout(kz_json:object(), ne_binary()) -> integer().
ringback_timeout(Data, SlotNumber) ->
    JObj = slot_configuration(Data, SlotNumber),
    DefaultRingbackTime = kz_json:get_integer_value(<<"default_ringback_timeout">>, Data, ?DEFAULT_RINGBACK_TM),
    kz_json:get_integer_value(<<"ringback_timeout">>, JObj, DefaultRingbackTime).

-spec callback_timeout(kz_json:object(), ne_binary()) -> integer().
callback_timeout(Data, SlotNumber) ->
    JObj = slot_configuration(Data, SlotNumber),
    DefaultCallbackTime = kz_json:get_integer_value(<<"default_callback_timeout">>, Data, ?DEFAULT_CALLBACK_TM),
    kz_json:get_integer_value(<<"callback_timeout">>, JObj, DefaultCallbackTime).

-spec unanswered_action(ne_binary(), kz_json:object(), kz_json:object(), kapps_call:call()) -> 'ok'.
unanswered_action(SlotNumber, Slot, Data, Call) ->
    case cf_exe:next(SlotNumber, Call) of
        'undefined' -> wait_for_pickup(SlotNumber, Slot, Data, Call);
        _ ->
            _ = publish_abandoned(Call, SlotNumber),
            _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), kapps_call:account_db(Call)),
            cf_exe:continue(SlotNumber, Call)
    end.

-spec presence_type(ne_binary(), kz_json:object(), kapps_call:call()) -> ne_binary().
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
    kz_json:get_value(<<"slots">>, Data, kz_json:new()).

-spec slot_configuration(kz_json:object(), ne_binary()) -> kz_json:object().
slot_configuration(Data, SlotNumber) ->
    kz_json:get_value(SlotNumber, slots_configuration(Data), kz_json:new()).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ringback the device that parked the call
%% @end
%%--------------------------------------------------------------------
-spec get_endpoint_id(api_binary(), kapps_call:call()) -> api_binary().
get_endpoint_id('undefined', _) -> 'undefined';
get_endpoint_id(Username, Call) ->
    AccountDb = kapps_call:account_db(Call),
    case cf_util:endpoint_id_by_sip_username(AccountDb, Username) of
        {'ok', EndpointId} -> EndpointId;
        {'error', _} -> 'undefined'
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ringback the device that parked the call
%% @end
%%--------------------------------------------------------------------
-spec ringback_parker(api_binary(), ne_binary(), ne_binary(), kz_json:object(), kapps_call:call()) ->
                             'answered' | 'failed' | 'channel_hungup'.
ringback_parker('undefined', _, _, _, _) -> 'failed';
ringback_parker(EndpointId, SlotNumber, TmpCID, Data, Call) ->
    Timeout = callback_timeout(Data, SlotNumber),
    case kz_endpoint:build(EndpointId, kz_json:from_list([{<<"can_call_self">>, 'true'}]), Call) of
        {'ok', Endpoints} ->
            lager:info("attempting to ringback endpoint ~s", [EndpointId]),
            OriginalCID = kapps_call:caller_id_name(Call),
            CleanUpFun = fun(_) ->
                                 lager:info("parking ringback was answered", []),
                                 _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), kapps_call:account_db(Call)),
                                 kapps_call:set_caller_id_name(OriginalCID, Call)
                         end,
            Call1 = kapps_call:set_caller_id_name(TmpCID, Call),
            kapps_call_command:bridge(Endpoints, ?DEFAULT_TIMEOUT_S, Call1),
            wait_for_ringback(CleanUpFun, Timeout, Call1);
        _ -> 'failed'
    end.

-spec wait_for_ringback(function(), kz_timeout(), kapps_call:call()) ->
                               'answered' | 'failed' | 'channel_hungup'.
wait_for_ringback(Fun, Timeout, Call) ->
    case kapps_call_command:wait_for_bridge(Timeout, Fun, Call) of
        {'ok', _} ->
            lager:info("completed successful bridge to the ringback device"),
            'answered';
        {'fail', JObj} ->
            case kz_json:get_value(<<"Event-Name">>, JObj) of
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

-spec update_presence(api_object()) -> 'ok'.
update_presence('undefined') -> 'ok';
update_presence(Slot) ->
    update_presence(kz_json:get_value(?PRESENCE_TYPE_KEY, Slot, <<"early">>), Slot).

-spec update_presence(ne_binary(), api_object()) -> 'ok'.
update_presence(_State, 'undefined') -> 'ok';
update_presence(State, Slot) ->
    PresenceId = kz_json:get_value(<<"Presence-ID">>, Slot),
    TargetURI = kz_json:get_value(<<"CID-URI">>, Slot),
    ToTag = kz_json:get_value(<<"To-Tag">>, Slot),
    FromTag = kz_json:get_value(<<"From-Tag">>, Slot),
    SwitchURI = kz_json:get_value(<<"Switch-URI">>, Slot),
    CallId = kz_json:get_value(<<"Call-ID">>, Slot),
    SlotCallId = kz_json:get_value(<<"Slot-Call-ID">>, Slot),
    Command = props:filter_undefined(
                [{<<"Presence-ID">>, PresenceId}
                ,{<<"From">>, TargetURI}
                ,{<<"From-Tag">>, FromTag}
                ,{<<"To-Tag">>, ToTag}
                ,{<<"State">>, State}
                ,{<<"Call-ID">>, SlotCallId}
                ,{<<"Target-Call-ID">>, CallId}
                ,{<<"Switch-URI">>, SwitchURI}
                 | kz_api:default_headers(<<"park">>, ?APP_VERSION)
                ]),
    lager:info("update presence-id '~s' with state: ~s", [PresenceId, State]),
    kz_amqp_worker:cast(Command, fun kapi_presence:publish_update/1).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec publish_parked(kapps_call:call(), ne_binary()) -> 'ok'.
publish_parked(Call, SlotNumber) ->
    publish_event(Call, SlotNumber, <<"PARK_PARKED">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec publish_retrieved(kapps_call:call(), ne_binary()) -> 'ok'.
publish_retrieved(Call, SlotNumber) ->
    publish_event(Call, SlotNumber, <<"PARK_RETRIEVED">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec publish_abandoned(kapps_call:call(), ne_binary()) -> 'ok'.
publish_abandoned(Call, Slot) ->
    publish_event(Call, Slot, <<"PARK_ABANDONED">>).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% @end
%%--------------------------------------------------------------------
-spec publish_event(kapps_call:call(), ne_binary(), ne_binary()) -> 'ok'.
publish_event(Call, SlotNumber, Event) ->
    Cmd = [
           {<<"Event-Name">>, Event}
          ,{<<"Call-ID">>, kapps_call:call_id(Call)}
          ,{<<"Parking-Slot">>, kz_util:to_binary(SlotNumber)}
          ,{<<"Caller-ID-Number">>, kapps_call:caller_id_number(Call)}
          ,{<<"Caller-ID-Name">>, kapps_call:caller_id_name(Call)}
          ,{<<"Callee-ID-Number">>, kapps_call:callee_id_number(Call)}
          ,{<<"Callee-ID-Name">>, kapps_call:callee_id_name(Call)}
          ,{<<"Custom-Channel-Vars">>, kapps_call:custom_channel_vars(Call)}
           | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
          ],
    kapi_call:publish_event(Cmd).
