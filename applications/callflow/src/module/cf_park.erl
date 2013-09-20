%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, 2600Hz INC
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(cf_park).

-include("../callflow.hrl").

-export([handle/2]).
-export([update_presence/3]).

-define(MOD_CONFIG_CAT, <<(?CF_CONFIG_CAT)/binary, ".park">>).

-define(DB_DOC_NAME, whapps_config:get(?MOD_CONFIG_CAT, <<"db_doc_name">>, <<"parked_calls">>)).
-define(DEFAULT_RINGBACK_TM, whapps_config:get_integer(?MOD_CONFIG_CAT, <<"default_ringback_time">>, 120000)).
-define(PARKED_CALLS_KEY(Db), {?MODULE, 'parked_calls', Db}).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%--------------------------------------------------------------------
-spec update_presence(ne_binary(), ne_binary(), ne_binary()) -> 'ok'.
update_presence(SlotNumber, PresenceId, AccountDb) ->
    AccountId = wh_util:format_account_id(AccountDb, 'raw'),
    ParkedCalls = get_parked_calls(AccountDb, AccountId),
    State = case wh_json:get_value([<<"slots">>, SlotNumber, <<"Call-ID">>], ParkedCalls) of
                'undefined' -> <<"terminated">>;
                ParkedCallId ->
                    case whapps_call_command:b_channel_status(ParkedCallId) of
                        {'ok', _} -> <<"early">>;
                        {'error', _} -> <<"terminated">>
                    end
            end,
    ParkingId = wh_util:to_hex_binary(crypto:md5(PresenceId)),
    lager:debug("sending presence resp for parking slot ~s(~s): ~s", [SlotNumber, PresenceId, State]),
    whapps_call_command:presence(State, PresenceId, ParkingId).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%--------------------------------------------------------------------
-spec handle(wh_json:object(), whapps_call:call()) -> any().
handle(Data, Call) ->
    ParkedCalls = get_parked_calls(Call),
    SlotNumber = get_slot_number(ParkedCalls, whapps_call:kvs_fetch('cf_capture_group', Call)),
    ReferredTo = whapps_call:custom_channel_var(<<"Referred-To">>, <<>>, Call),
    case re:run(ReferredTo, "Replaces=([^;]*)", [{'capture', [1], 'binary'}]) of
        'nomatch' when ReferredTo =:= <<>> ->
            lager:info("call was the result of a direct dial"),
            case wh_json:get_value(<<"action">>, Data, <<"park">>) of
                <<"park">> ->
                    lager:info("action is to park the call"),
                    Slot = create_slot(ReferredTo, Call),
                    park_call(SlotNumber, Slot, ParkedCalls, 'undefined', Call);
                <<"retrieve">> ->
                    lager:info("action is to retrieve a parked call"),
                    case retrieve(SlotNumber, ParkedCalls, Call) of
                        {'ok', _} -> 'ok';
                        _Else ->
                            _ = whapps_call_command:b_answer(Call),
                            _ = whapps_call_command:b_prompt(<<"park-no_caller">>, Call),
                            cf_exe:continue(Call)
                    end;
                <<"auto">> ->
                    lager:info("action is to automatically determine if we should retrieve or park"),
                    Slot = create_slot(cf_exe:callid(Call), Call),
                    case retrieve(SlotNumber, ParkedCalls, Call) of
                        {'hungup', JObj} -> park_call(SlotNumber, Slot, JObj, 'undefined', Call);
                        {'error', _} -> park_call(SlotNumber, Slot, ParkedCalls, 'undefined', Call);
                        {'ok', _} -> 'ok'
                    end
            end;
        'nomatch' ->
            lager:info("call was the result of a blind transfer, assuming intention was to park"),
            Slot = create_slot('undefined', Call),
            park_call(SlotNumber, Slot, ParkedCalls, ReferredTo, Call);
        {'match', [Replaces]} ->
            lager:info("call was the result of an attended-transfer completion, updating call id"),
            {'ok', FoundInSlotNumber, Slot} = update_call_id(Replaces, ParkedCalls, Call),
            wait_for_pickup(FoundInSlotNumber, wh_json:get_value(<<"Ringback-ID">>, Slot), Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the hostname of the switch
%% @end
%%--------------------------------------------------------------------
-spec get_channel_status(api_binary()) -> wh_json:object().
get_channel_status(CallId) ->
    case whapps_call_command:b_channel_status(CallId) of
        {'error', _} -> wh_json:new();
        {'ok', JObj} -> JObj
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the appropriate action to retrieve a parked call
%% @end
%%--------------------------------------------------------------------
-spec retrieve(ne_binary(), wh_json:object(), whapps_call:call()) ->
                      {'ok', wh_json:object()} |
                      {'hungup', wh_json:object()} |
                      {'error', term()}.
retrieve(SlotNumber, ParkedCalls, Call) ->
    case wh_json:get_value([<<"slots">>, SlotNumber], ParkedCalls) of
        'undefined' ->
            lager:info("the parking slot ~s is empty, unable to retrieve caller", [SlotNumber]),
            {'error', 'slot_empty'};
        Slot ->
            CallerNode = whapps_call:switch_nodename(Call),
            ParkedCall = wh_json:get_ne_value(<<"Call-ID">>, Slot),
            lager:info("the parking slot ~s currently has a parked call ~s, attempting to retrieve caller", [SlotNumber, ParkedCall]),
            ChannelStatus = get_channel_status(ParkedCall),
            case wh_json:get_ne_value(<<"Switch-Nodename">>, ChannelStatus) of
                'undefined' ->
                    lager:info("the parked call has hungup, but is was still listed in the slot"),
                    case cleanup_slot(SlotNumber, ParkedCall, whapps_call:account_db(Call)) of
                        {'ok', JObj} -> {'hungup', JObj};
                        {'error', _} -> {'hungup', ParkedCalls}
                    end;
                CallerNode ->
                    ParkedCall = wh_json:get_ne_value(<<"Call-ID">>, Slot),
                    case cleanup_slot(SlotNumber, ParkedCall, whapps_call:account_db(Call)) of
                        {'ok', _}=Ok ->
                            lager:info("retrieved parked call from slot, bridging to caller"),
                            publish_usurp_control(ParkedCall, Call),
                            Name = wh_json:get_value(<<"CID-Name">>, Slot, <<"Parking Slot ", SlotNumber/binary>>),
                            Number = wh_json:get_value(<<"CID-Number">>, Slot, SlotNumber),
                            Update = [{<<"Callee-ID-Name">>, Name}
                                      ,{<<"Callee-ID-Number">>, Number}
                                     ],
                            _ = whapps_call_command:set(wh_json:from_list(Update), 'undefined', Call),
                            _ = whapps_call_command:answer(Call),
                            _ = whapps_call_command:b_pickup(ParkedCall, Call),
                            cf_exe:continue(Call),
                            Ok;
                        %% if we cant clean up the slot then someone beat us to it
                        {'error', _R}=E ->
                            lager:info("unable to remove parked call from slot: ~p", [_R]),
                            E
                    end;
                OtherNode ->
                    lager:info("the parked call is on node ~s but this call is on node ~s, redirecting", [OtherNode, CallerNode]),
                    URL = get_node_url(OtherNode, ChannelStatus),
                    send_redirect(URL, Call),
                    cf_exe:transfer(Call),
                    {'ok', ParkedCalls}
            end
    end.

-spec get_node_url(atom(), wh_json:object()) -> ne_binary().
get_node_url(Node, JObj) ->
    case wh_json:get_value(<<"Switch-URL">>, JObj) of
        'undefined' ->
            IP = get_node_ip(Node),
            Port = whapps_config:get_binary(?MOD_CONFIG_CAT, <<"parking_server_sip_port">>, 5060),
            <<"sip:mod_sofia@", IP/binary, ":", Port/binary>>;
        URL -> URL
    end.

-spec send_redirect(ne_binary(), whapps_call:call()) -> 'ok'.
send_redirect(URL, Call) ->
    case whapps_config:get_is_true(?MOD_CONFIG_CAT, <<"redirect_via_proxy">>, 'true') of
        'true' -> redirect_via_proxy(URL, Call);
        'false' -> redirect_via_endpoint(URL, Call)
    end.

-spec redirect_via_proxy(ne_binary(), whapps_call:call()) -> 'ok'.
redirect_via_proxy(URL, Call) ->
    Contact = <<"sip:", (whapps_call:to_user(Call))/binary
                ,"@", (whapps_call:to_realm(Call))/binary>>,
    CorrectedURL = binary:replace(URL, <<"mod_sofia@">>, <<>>),
    whapps_call_command:redirect(Contact, CorrectedURL, Call).

-spec redirect_via_endpoint(ne_binary(), whapps_call:call()) -> 'ok'.
redirect_via_endpoint(URL, Call) ->
    CorrectedURL = binary:replace(URL, <<"mod_sofia">>, whapps_call:to_user(Call)),
    whapps_call_command:redirect(CorrectedURL, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the appropriate action to park the current call scenario
%% @end
%%--------------------------------------------------------------------
-spec park_call(ne_binary(), wh_json:object(), wh_json:object(), api_binary(), whapps_call:call()) -> 'ok'.
park_call(SlotNumber, Slot, ParkedCalls, ReferredTo, Call) ->
    lager:info("attempting to park call in slot ~s", [SlotNumber]),
    case {ReferredTo, save_slot(SlotNumber, Slot, ParkedCalls, Call)} of
        %% attended transfer but the provided slot number is occupied, we are still connected to the 'parker'
        %% not the 'parkee'
        {'undefined', {'error', 'occupied'}} ->
            lager:info("selected slot is occupied"),
            %% Update screen with error that the slot is occupied
            _ = whapps_call_command:b_answer(Call),
            %% playback message that caller will have to try a different slot
            _ = whapps_call_command:b_prompt(<<"park-already_in_use">>, Call),
            cf_exe:continue(Call),
            'ok';
        %% attended transfer and allowed to update the provided slot number, we are still connected to the 'parker'
        %% not the 'parkee'
        {'undefined', _} ->
            lager:info("playback slot number ~s to caller", [SlotNumber]),
            %% Update screen with new slot number
            _ = whapps_call_command:b_answer(Call),
            %% Caller parked in slot number...
            _ = whapps_call_command:b_prompt(<<"park-call_placed_in_spot">>, Call),
            _ = whapps_call_command:b_say(wh_util:to_binary(SlotNumber), Call),
            cf_exe:transfer(Call),
            'ok';
        %% blind transfer and but the provided slot number is occupied
        {_, {'error', 'occupied'}} ->
            lager:info("blind transfer to a occupied slot, call the parker back.."),
            TmpCID = <<"Parking slot ", SlotNumber/binary, " occupied">>,
            case ringback_parker(wh_json:get_value(<<"Ringback-ID">>, Slot), SlotNumber, TmpCID, Call) of
                'answered' -> cf_exe:continue(Call);
                'failed' ->
                    whapps_call_command:hangup(Call),
                    cf_exe:stop(Call)
            end,
            'ok';
        %% blind transfer and allowed to update the provided slot number
        {_, {'ok', _}} ->
            ParkedCallId = wh_json:get_value(<<"Call-ID">>, Slot),
            PresenceId = wh_json:get_value(<<"Presence-ID">>, Slot),
            ParkingId = wh_util:to_hex_binary(crypto:md5(PresenceId)),
            lager:info("call ~s parked in slot ~s, update presence-id '~s' with state: early", [ParkedCallId, SlotNumber, PresenceId]),
            whapps_call_command:presence(<<"early">>, PresenceId, ParkingId),
            wait_for_pickup(SlotNumber, wh_json:get_value(<<"Ringback-ID">>, Slot), Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the json object representing the call in the parking slot
%% @end
%%--------------------------------------------------------------------
-spec create_slot('undefined' | binary(), whapps_call:call()) -> wh_json:object().
create_slot('undefined', Call) ->
    CallId = cf_exe:callid(Call),
    AccountDb = whapps_call:account_db(Call),
    AccountId = whapps_call:account_id(Call),
    wh_json:from_list([{<<"Call-ID">>, CallId}
                       ,{<<"Presence-ID">>, <<(whapps_call:request_user(Call))/binary
                                              ,"@", (wh_util:get_account_realm(AccountDb, AccountId))/binary>>}
                       ,{<<"Node">>, whapps_call:switch_nodename(Call)}
                       ,{<<"CID-Number">>, whapps_call:caller_id_number(Call)}
                       ,{<<"CID-Name">>, whapps_call:caller_id_name(Call)}
                      ]);
create_slot(ParkerCallId, Call) ->
    CallId = cf_exe:callid(Call),
    AccountDb = whapps_call:account_db(Call),
    AccountId = whapps_call:account_id(Call),
    Referred = whapps_call:custom_channel_var(<<"Referred-By">>, Call),
    ReOptions = [{'capture', [1], 'binary'}],
    RingbackId = case catch(re:run(Referred, <<".*sip:(.*)@.*">>, ReOptions)) of
                     {'match', [Match]} -> get_endpoint_id(Match, Call);
                     _ -> 'undefined'
                 end,
    wh_json:from_list(
      props:filter_undefined(
        [{<<"Call-ID">>, CallId}
         ,{<<"Parker-Call-ID">>, ParkerCallId}
         ,{<<"Presence-ID">>, <<(whapps_call:request_user(Call))/binary
                                ,"@", (wh_util:get_account_realm(AccountDb, AccountId))/binary>>}
         ,{<<"Ringback-ID">>, RingbackId}
         ,{<<"Node">>, whapps_call:switch_nodename(Call)}
         ,{<<"CID-Number">>, whapps_call:caller_id_number(Call)}
         ,{<<"CID-Name">>, whapps_call:caller_id_name(Call)}
        ])).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the provided slot number or the next available if none
%% was provided
%% @end
%%--------------------------------------------------------------------
-spec get_slot_number(wh_json:object(), whapps_call:call()) -> ne_binary().
get_slot_number(_, CaptureGroup) when is_binary(CaptureGroup)
                                      andalso size(CaptureGroup) > 0 ->
    CaptureGroup;
get_slot_number(ParkedCalls, _) ->
    Slots = [wh_util:to_integer(Slot)
             || Slot <- wh_json:get_keys(<<"slots">>, ParkedCalls)
            ],
    Sorted = ordsets:to_list(ordsets:from_list([100|Slots])),
    wh_util:to_binary(find_slot_number(Sorted)).

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
-spec save_slot(ne_binary(), wh_json:object(), wh_json:object(), whapps_call:call()) ->
                       {'ok', wh_json:object()} |
                       {'error', atom()}.
save_slot(SlotNumber, Slot, ParkedCalls, Call) ->
    ParkedCallId = wh_json:get_ne_value([<<"slots">>, SlotNumber, <<"Call-ID">>], ParkedCalls),
    ParkerCallId = wh_json:get_ne_value([<<"slots">>, SlotNumber, <<"Parker-Call-ID">>], ParkedCalls),
    case wh_util:is_empty(ParkedCallId) orelse ParkedCallId =:= ParkerCallId of
        'true' ->
            lager:info("slot has parked call '~s' by parker '~s', it is available", [ParkedCallId, ParkerCallId]),
            do_save_slot(SlotNumber, Slot, ParkedCalls, Call);
        'false' ->
            case whapps_call_command:b_channel_status(ParkedCallId) of
                {'ok', _} ->
                    lager:info("slot has active call '~s' in it, denying use of slot", [ParkedCallId]),
                    {'error', 'occupied'};
                _Else ->
                    lager:info("slot is availabled because parked call '~s' no longer exists: ~p", [ParkedCallId, _Else]),
                    do_save_slot(SlotNumber, Slot, ParkedCalls, Call)
            end
    end.

-spec do_save_slot(ne_binary(), wh_json:object(), wh_json:object(), whapps_call:call()) ->
                          {'ok', wh_json:object()} |
                          {'error', atom()}.
do_save_slot(SlotNumber, Slot, ParkedCalls, Call) ->
    AccountDb = whapps_call:account_db(Call),
    case couch_mgr:save_doc(AccountDb, wh_json:set_value([<<"slots">>, SlotNumber], Slot, ParkedCalls)) of
        {'ok', JObj}=Ok ->
            lager:info("successfully stored call parking data for slot ~s", [SlotNumber]),
            CacheProps = [{'origin', {'db', AccountDb, ?DB_DOC_NAME}}],
            wh_cache:store_local(?CALLFLOW_CACHE, ?PARKED_CALLS_KEY(AccountDb), JObj, CacheProps),
            Ok;
        {'error', 'conflict'} ->
            maybe_resolve_conflict(SlotNumber, Slot, Call)
    end.

-spec maybe_resolve_conflict(ne_binary(), wh_json:object(), whapps_call:call()) ->
                                    {'ok', wh_json:object()} |
                                    {'error', atom()}.
maybe_resolve_conflict(SlotNumber, Slot, Call) ->
    AccountDb = whapps_call:account_db(Call),
    {'ok', JObj} = couch_mgr:open_doc(AccountDb, ?DB_DOC_NAME),
    {'ok', JObj}=Ok = couch_mgr:save_doc(AccountDb, wh_json:set_value([<<"slots">>, SlotNumber], Slot, JObj)),
    lager:info("successfully stored call parking data for slot ~s", [SlotNumber]),
    CacheProps = [{'origin', {'db', AccountDb, ?DB_DOC_NAME}}],
    wh_cache:store_local(?CALLFLOW_CACHE, ?PARKED_CALLS_KEY(AccountDb), JObj, CacheProps),
    Ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% After an attended transfer we need to find the callid that we stored
%% because it was the "C-Leg" of a transfer and now we have the
%% actuall "A-Leg".  Find the old callid and update it with the new one.
%% @end
%%--------------------------------------------------------------------
-spec update_call_id(ne_binary(), wh_json:object(), whapps_call:call()) ->
                            {'ok', ne_binary(), wh_json:object()}.
update_call_id(Replaces, ParkedCalls, Call) ->
    update_call_id(Replaces, ParkedCalls, Call, 0).

update_call_id(_, _, _, Loops) when Loops > 5 ->
    lager:info("unable to update parked call id after ~p tries", [Loops]),
    {'error', 'update_failed'};
update_call_id(Replaces, ParkedCalls, Call, Loops) ->
    CallId = cf_exe:callid(Call),
    lager:info("update parked call id ~s with new call id ~s", [Replaces, CallId]),
    Slots = wh_json:get_value(<<"slots">>, ParkedCalls, wh_json:new()),
    case find_slot_by_callid(Slots, Replaces) of
        {'ok', SlotNumber, Slot} ->
            lager:info("found parked call id ~s in slot ~s", [Replaces, SlotNumber]),
            CallerNode = whapps_call:switch_nodename(Call),
            Updaters = [fun(J) -> wh_json:set_value(<<"Call-ID">>, CallId, J) end
                        ,fun(J) -> wh_json:set_value(<<"Node">>, CallerNode, J) end
                        ,fun(J) -> wh_json:set_value(<<"CID-Number">>, whapps_call:caller_id_number(Call), J) end
                        ,fun(J) -> wh_json:set_value(<<"CID-Name">>, whapps_call:caller_id_name(Call), J) end
                        ,fun(J) ->
                                 Referred = whapps_call:custom_channel_var(<<"Referred-By">>, Call),
                                 ReOptions = [{'capture', [1], 'binary'}],
                                 case catch(re:run(Referred, <<".*sip:(.*)@.*">>, ReOptions)) of
                                     {'match', [Match]} ->
                                         case get_endpoint_id(Match, Call) of
                                             'undefined' -> wh_json:delete_key(<<"Ringback-ID">>, J);
                                             RingbackId -> wh_json:set_value(<<"Ringback-ID">>, RingbackId, J)
                                         end;
                                     _ ->
                                         wh_json:delete_key(<<"Ringback-ID">>, J)
                                 end
                         end
                       ],
            UpdatedSlot = lists:foldr(fun(F, J) -> F(J) end, Slot, Updaters),
            JObj = wh_json:set_value([<<"slots">>, SlotNumber], UpdatedSlot, ParkedCalls),
            case couch_mgr:save_doc(whapps_call:account_db(Call), JObj) of
                {'ok', _} ->
                    publish_usurp_control(Call),
                    PresenceId = wh_json:get_value(<<"Presence-ID">>, Slot),
                    ParkingId = wh_util:to_hex_binary(crypto:md5(PresenceId)),
                    lager:info("update presence-id '~s' with state: early", [PresenceId]),
                    whapps_call_command:presence(<<"early">>, PresenceId, ParkingId),
                    {'ok', SlotNumber, UpdatedSlot};
                {'error', 'conflict'} ->
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given the parked calls and a list of parked keys find the slot with
%% the provided call id.
%% @end
%%--------------------------------------------------------------------
-spec find_slot_by_callid(wh_json:object(), ne_binary()) ->
                                 {'ok', ne_binary(), wh_json:object()} |
                                 {'error', 'not_found'}.
find_slot_by_callid(Slots, CallId) ->
    find_slot_by_callid(wh_json:get_keys(Slots), Slots, CallId).

-spec find_slot_by_callid(ne_binaries(), wh_json:object(), ne_binary()) ->
                                 {'ok', ne_binary(), wh_json:object()} |
                                 {'error', 'not_found'}.
find_slot_by_callid([], _, _) ->
    {'error', 'not_found'};
find_slot_by_callid([SlotNumber|SlotNumbers], Slots, CallId) ->
    Slot = wh_json:get_value(SlotNumber, Slots),
    case wh_json:get_value(<<"Call-ID">>, Slot) of
        CallId -> {'ok', SlotNumber, Slot};
        _ -> find_slot_by_callid(SlotNumbers, Slots, CallId)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to retrieve the parked calls list from the datastore, if
%% the list does not exist then it returns an new empty instance
%% @end
%%--------------------------------------------------------------------
-spec get_parked_calls(whapps_call:call()) -> wh_json:object().
get_parked_calls(Call) ->
    get_parked_calls(whapps_call:account_db(Call), whapps_call:account_id(Call)).

-spec get_parked_calls(ne_binary(), ne_binary()) -> wh_json:object().
get_parked_calls(AccountDb, AccountId) ->
    case wh_cache:peek_local(?CALLFLOW_CACHE, ?PARKED_CALLS_KEY(AccountDb)) of
        {'ok', JObj} -> JObj;
        {'error', 'not_found'} ->
            fetch_parked_calls(AccountDb, AccountId)
    end.

-spec fetch_parked_calls(ne_binary(), ne_binary()) -> wh_json:object().
fetch_parked_calls(AccountDb, AccountId) ->
    case couch_mgr:open_doc(AccountDb, ?DB_DOC_NAME) of
        {'error', 'not_found'} ->
            Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            Generators = [fun(J) -> wh_json:set_value(<<"_id">>, <<"parked_calls">>, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_type">>, <<"parked_calls">>, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_account_db">>, AccountDb, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_account_id">>, AccountId, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_created">>, Timestamp, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_modified">>, Timestamp, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_vsn">>, <<"1">>, J) end
                          ,fun(J) -> wh_json:set_value(<<"slots">>, wh_json:new(), J) end],
            lists:foldr(fun(F, J) -> F(J) end, wh_json:new(), Generators);
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
                          {'ok', wh_json:object()} |
                          {'error', term()}.
cleanup_slot(SlotNumber, ParkedCallId, AccountDb) ->
    case couch_mgr:open_doc(AccountDb, ?DB_DOC_NAME) of
        {'ok', JObj} ->
            case wh_json:get_value([<<"slots">>, SlotNumber, <<"Call-ID">>], JObj) of
                ParkedCallId ->
                    lager:info("delete parked call ~s in slot ~s", [ParkedCallId, SlotNumber]),
                    case couch_mgr:save_doc(AccountDb, wh_json:delete_key([<<"slots">>, SlotNumber], JObj)) of
                        {'ok', _}=Ok ->
                            PresenceId = wh_json:get_value([<<"slots">>, SlotNumber, <<"Presence-ID">>], JObj),
                            ParkingId = wh_util:to_hex_binary(crypto:md5(PresenceId)),
                            lager:info("update presence-id '~s' with state: terminated", [PresenceId]),
                            _ = whapps_call_command:presence(<<"terminated">>, PresenceId, ParkingId),
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
-spec wait_for_pickup(ne_binary(), api_binary(), whapps_call:call()) -> any().
wait_for_pickup(SlotNumber, 'undefined', Call) ->
    lager:info("(no ringback) waiting for parked caller to be picked up or hangup"),
    _ = whapps_call_command:b_hold(Call),
    lager:info("(no ringback) parked caller has been picked up or hungup"),
    cleanup_slot(SlotNumber, cf_exe:callid(Call), whapps_call:account_db(Call));
wait_for_pickup(SlotNumber, RingbackId, Call) ->
    lager:info("waiting for parked caller to be picked up or hangup"),
    case whapps_call_command:b_hold(?DEFAULT_RINGBACK_TM, Call) of
        {'error', 'timeout'} ->
            TmpCID = <<"Parking slot ", SlotNumber/binary>>,
            ChannelUp = case whapps_call_command:b_channel_status(Call) of
                            {'ok', _} -> 'true';
                            {'error', _} -> 'false'
                     end,
            case ChannelUp andalso ringback_parker(RingbackId, SlotNumber, TmpCID, Call) of
                'answered' ->
                    lager:info("parked caller ringback was answered"),
                    cf_exe:continue(Call);
                'failed' ->
                    lager:info("ringback was not answered, continuing to hold parked call"),
                    wait_for_pickup(SlotNumber, RingbackId, Call);
                'false' ->
                    lager:info("parked call doesnt exist anymore, hangup"),
                    _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), whapps_call:account_db(Call)),
                    cf_exe:stop(Call)
            end;
        {'error', _} ->
            lager:info("parked caller has hungup"),
            _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), whapps_call:account_db(Call)),
            cf_exe:transfer(Call);
        {'ok', _} ->
            lager:info("parked caller has been picked up"),
            _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), whapps_call:account_db(Call)),
            cf_exe:transfer(Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given a freeswitch node name try to determine the IP address,
%% assumes that
%% a) the hostname is resolvable by at least this server
%% b) the IP is routable by the phone
%% c) and above, that the port is 5060
%% @end
%%--------------------------------------------------------------------
-spec get_node_ip(ne_binary()) -> ne_binary().
get_node_ip(Node) ->
    [_, Hostname] = binary:split(wh_util:to_binary(Node), <<"@">>),
    {'ok', Addresses} = inet:getaddrs(wh_util:to_list(Hostname), 'inet'),
    {A, B, C, D} = hd(Addresses),
    <<(wh_util:to_binary(A))/binary, "."
      ,(wh_util:to_binary(B))/binary, "."
      ,(wh_util:to_binary(C))/binary, "."
      ,(wh_util:to_binary(D))/binary>>.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Kill any other cf_exe or ecallmgr_call_control processes that are
%% hanging around waiting for the parked call on hold to hit the
%% timeout.
%% @end
%%--------------------------------------------------------------------
-spec publish_usurp_control(whapps_call:call()) -> 'ok'.
publish_usurp_control(Call) ->
    publish_usurp_control(cf_exe:callid(Call), Call).

-spec publish_usurp_control(ne_binary(), whapps_call:call()) -> 'ok'.
publish_usurp_control(CallId, Call) ->
    ExeFetchId = whapps_call:custom_channel_var(<<"Fetch-ID">>, Call),
    lager:info("usurp call control of ~s", [ExeFetchId]),
    Notice = [{<<"Call-ID">>, CallId}
              ,{<<"Fetch-ID">>, ExeFetchId}
              ,{<<"Reason">>, <<"park">>}
              | wh_api:default_headers(?APP_NAME, ?APP_VERSION)
             ],
    wapi_call:publish_usurp_control(CallId, Notice).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ringback the device that parked the call
%% @end
%%--------------------------------------------------------------------
-spec get_endpoint_id(api_binary(), whapps_call:call()) -> api_binary().
get_endpoint_id('undefined', _) -> 'undefined';
get_endpoint_id(Username, Call) ->
    AccountDb = whapps_call:account_db(Call),
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
-spec ringback_parker(api_binary(), ne_binary(), ne_binary(), whapps_call:call()) ->
                             'answered' | 'failed'.
ringback_parker('undefined', _, _, _) -> 'failed';
ringback_parker(EndpointId, SlotNumber, TmpCID, Call) ->
    case cf_endpoint:build(EndpointId, wh_json:from_list([{<<"can_call_self">>, 'true'}]), Call) of
        {'ok', Endpoints} ->
            lager:info("attempting to ringback endpoint ~s", [EndpointId]),
            OriginalCID = whapps_call:caller_id_name(Call),
            CleanUpFun = fun(_) ->
                                 lager:info("parking ringback was answered", []),
                                 _ = cleanup_slot(SlotNumber, cf_exe:callid(Call), whapps_call:account_db(Call)),
                                 whapps_call:set_caller_id_name(OriginalCID, Call)
                         end,
            Call1 = whapps_call:set_caller_id_name(TmpCID, Call),
            whapps_call_command:bridge(Endpoints, ?DEFAULT_TIMEOUT_S, Call1),
            case whapps_call_command:wait_for_bridge(30000, CleanUpFun, Call1) of
                {'ok', _} ->
                    lager:info("completed successful bridge to the ringback device"),
                    'answered';
                _Else ->
                    lager:info("ringback failed, returning caller to parking slot"),
                    'failed'
            end;
        _ -> 'failed'
    end.
