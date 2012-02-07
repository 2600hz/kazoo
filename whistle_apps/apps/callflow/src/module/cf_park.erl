%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 4 Oct 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_park).

-include("../callflow.hrl").

-export([handle/2]).

-define(PARKED_CALLS, <<"parked_calls">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (wh_json:json_object(), #cf_call{}) -> ok.
handle(Data, #cf_call{channel_vars=CCVs}=Call) ->
    ParkedCalls = get_parked_calls(Call),
    SlotNumber = get_slot_number(ParkedCalls, Call),
    ReferredTo = wh_json:get_ne_value(<<"Referred-To">>, CCVs, <<>>),
    case re:run(ReferredTo, "Replaces=([^;]*)", [{capture, [1], binary}]) of
        nomatch when ReferredTo =:= <<>> ->
            ?LOG("call was the result of a direct dial"),
            case wh_json:get_value(<<"action">>, Data, <<"park">>) of
                <<"park">> ->
                    ?LOG("action is to park the call"),
                    park_call(SlotNumber, ParkedCalls, undefined, Call);
                <<"retrieve">> ->
                    ?LOG("action is to retrieve a parked call"),
                    case retrieve(SlotNumber, ParkedCalls, Call) of
                        false -> cf_exe:continue(Call);
                        _ -> ok
                    end;
                <<"auto">> ->
                    ?LOG("action is to automatically determine if we should retrieve or park"),
                    case retrieve(SlotNumber, ParkedCalls, Call) of
                        false -> park_call(SlotNumber, ParkedCalls, undefined, Call);
                        _ -> ok
                    end
            end;
        nomatch ->
            ?LOG("call was the result of a blind transfer, assuming intention was to park"),
            park_call(SlotNumber, ParkedCalls, ReferredTo, Call);
        {match, [Replaces]} ->
            ?LOG("call was the result of an attended-transfer completion, updating call id"),
            {ok, SlotNumber, Slot} = update_call_id(Replaces, ParkedCalls, Call),
            wait_for_pickup(SlotNumber, wh_json:get_value(<<"Ringback-ID">>, Slot), Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the hostname of the switch
%% @end
%%--------------------------------------------------------------------
-spec get_switch_hostname/1 :: (#cf_call{}) -> undefined | ne_binary().
-spec get_switch_hostname/2 :: (undefined | ne_binary(), #cf_call{}) -> undefined | ne_binary().

get_switch_hostname(Call) ->
    get_switch_hostname(undefined, Call).

get_switch_hostname(CallId, Call) ->
    case cf_call_command:b_channel_status(CallId, Call) of
        {ok, CallerStatus} ->
            wh_json:get_ne_value(<<"Switch-Hostname">>, CallerStatus);
        _Else ->
            undefined
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the appropriate action to retrieve a parked call
%% @end
%%--------------------------------------------------------------------
-spec retrieve/3 :: (ne_binary(), wh_json:json_object(), #cf_call{}) -> boolean().
retrieve(SlotNumber, ParkedCalls, #cf_call{to_user=ToUser, to_realm=ToRealm}=Call) ->
    CallerHost = get_switch_hostname(Call),
    case wh_json:get_value([<<"slots">>, SlotNumber], ParkedCalls) of
        undefined ->
            ?LOG("They hungup? play back nobody here message", []),
            false;
        Slot ->
            case wh_json:get_ne_value(<<"Node">>, Slot) of
                undefined ->
                    ?LOG("the parked caller node is undefined"),
                    false;
                CallerHost ->
                    ParkedCall = wh_json:get_ne_value(<<"Call-ID">>, Slot),
                    case cleanup_slot(SlotNumber, ParkedCall, Call) of
                        true ->
                            publish_usurp_control(ParkedCall, Call),
                            ?LOG("pickup call id ~s", [ParkedCall]),
                            cf_call_command:b_pickup(ParkedCall, Call),
                            cf_exe:continue(Call);
                        %% if we cant clean up the slot then someone beat us to it
                        false -> false
                    end;
                OtherNode ->
                    IP = get_node_ip(OtherNode),
                    Contact = <<"sip:", ToUser/binary, "@", ToRealm/binary>>,
                    Server = <<"sip:", IP/binary, ":5060">>,
                    cf_call_command:redirect(Contact, Server, Call),
                    cf_exe:transfer(Call)
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the appropriate action to park the current call scenario
%% @end
%%--------------------------------------------------------------------
-spec park_call/4 :: (ne_binary(), wh_json:json_object(), undefined | ne_binary(), #cf_call{}) -> ok.
park_call(SlotNumber, ParkedCalls, ReferredTo, Call) ->
    ?LOG("attempting to park call in slot ~p", [SlotNumber]),
    Slot = create_slot(ReferredTo, Call),
    case {ReferredTo, save_slot(SlotNumber, Slot, ParkedCalls, Call)} of
        %% attended transfer but the provided slot number is occupied, we are still connected to the 'parker'
        %% not the 'parkee'
        {undefined, {error, occupied}} ->
            ?LOG("selected slot is occupied"),
            %% Update screen with error that the slot is occupied
            cf_call_command:b_answer(Call),
            %% playback message that caller will have to try a different slot
            ok;
        %% attended transfer and allowed to update the provided slot number, we are still connected to the 'parker'
        %% not the 'parkee'
        {undefined, _} ->
            ?LOG("playback slot number to caller"),
            %% Update screen with new slot number
            cf_call_command:b_answer(Call),
            %% Caller parked in slot number...
            cf_call_command:b_say(wh_util:to_binary(SlotNumber), Call),
            ok;
        %% blind transfer and but the provided slot number is occupied
        {_, {error, occupied}} ->
            ?LOG("blind transfer to a occupied slot, call the parker back.."),
            case wh_json:get_value(<<"Ringback-ID">>, Slot) of
                undefined -> ok;
                EndpointId -> ringback_parker(EndpointId, <<"300">>, Call)
            end,
            ok;
        %% blind transfer and able to allowed to update the provided slot number
        {_, {ok, _}} ->
            PresenceId = wh_json:get_value(<<"Presence-ID">>, Slot),
            ?LOG("update presence-id '~s' with state: early", [PresenceId]),
            cf_call_command:presence(<<"early">>, PresenceId, Call),
            wait_for_pickup(SlotNumber, wh_json:get_value(<<"Ringback-ID">>, Slot), Call), 
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the json object representing the call in the parking slot
%% @end
%%--------------------------------------------------------------------
-spec create_slot/2 :: (undefined | ne_binary(), #cf_call{}) -> wh_json:json_object().
create_slot(undefined, #cf_call{request_user=Request, from_realm=FromRealm
                                ,cid_name=CIDName, cid_number=CIDNum}=Call) ->
    CallId = cf_exe:callid(Call),
    wh_json:from_list([{<<"Call-ID">>, CallId}
                       ,{<<"Presence-ID">>, <<Request/binary, "@", FromRealm/binary>>}
                       ,{<<"Node">>, get_switch_hostname(Call)}
                       ,{<<"CID-Number">>, CIDNum}
                       ,{<<"CID-Name">>, CIDName}
                      ]);
create_slot(_, #cf_call{request_user=Request, account_db=Db, account_id=AccountId, channel_vars=CCVs
                        ,cid_name=CIDName, cid_number=CIDNum}=Call) ->
    CallId = cf_exe:callid(Call),
    FromRealm = wh_util:get_account_realm(Db, AccountId),
    Referred = wh_json:get_value(<<"Referred-By">>, CCVs),
    ReOptions = [{capture, [1], binary}],
    RingbackId = case catch(re:run(Referred, <<".*sip:(.*)@.*">>, ReOptions)) of
                     {match, [Match]} -> get_endpoint_id(Match, Call);
                     _ -> undefined
                 end,
    wh_json:from_list([{K, V} || {K, V} <- [{<<"Call-ID">>, CallId}
                                            ,{<<"Presence-ID">>, <<Request/binary, "@", FromRealm/binary>>}
                                            ,{<<"Ringback-ID">>, RingbackId}
                                            ,{<<"Node">>, get_switch_hostname(Call)}
                                            ,{<<"CID-Number">>, CIDNum}
                                            ,{<<"CID-Name">>, CIDName}
                                           ], V =/= undefined
                      ]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the provided slot number or the next available if none
%% was provided
%% @end
%%--------------------------------------------------------------------
-spec get_slot_number/2 :: (wh_json:json_object(), #cf_call{}) -> ne_binary().
get_slot_number(_, #cf_call{capture_group=CG}) when is_binary(CG) andalso size(CG) > 0 ->
    CG;
get_slot_number(ParkedCalls, _) ->
    Slots = wh_json:get_value(<<"slots">>, ParkedCalls, []),
    Next = case [wh_util:to_integer(Key) || Key <- wh_json:get_keys(Slots)] of
               [] -> 100;
               Keys ->
                   case lists:max(Keys) of
                       Max when Max < 100 -> 100;
                       Start  ->
                           hd(lists:dropwhile(fun(E) ->
                                                      lists:member(E, Keys)
                                              end, lists:seq(100, Start + 1)))
                   end
           end,
    wh_util:to_binary(Next).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Save the slot data in the parked calls object at the slot number.
%% If, on save, it conflicts then it gets the new instance
%% and tries again, determining the new slot.
%% @end
%%--------------------------------------------------------------------
-spec save_slot/4 :: (ne_binary(), wh_json:json_object(), wh_json:json_object(), #cf_call{}) -> {ok, integer()} | {error, atom()}.
-spec do_save_slot/4 :: (ne_binary(), wh_json:json_object(), wh_json:json_object(), #cf_call{}) -> {ok, integer()} | {error, atom()}.

save_slot(SlotNumber, Slot, ParkedCalls, Call) ->
    case wh_json:get_value([<<"slots">>, SlotNumber, <<"Call-ID">>], ParkedCalls) of
        undefined ->
            ?LOG("slot number '~s' does not have a call id in it, allowing use of slot", [SlotNumber]),
            do_save_slot(SlotNumber, Slot, ParkedCalls, Call);
        CallId ->
            case cf_call_command:b_channel_status(CallId, Call) of
                {ok, _} ->
                    ?LOG("slot number '~s' has an active call id in it, denying use of slot", [SlotNumber]),
                    {error, occupied};
                _Else ->
                    ?LOG("slot number '~s' has an expired call id in it, allowing use of slot", [SlotNumber]),
                    do_save_slot(SlotNumber, Slot, ParkedCalls, Call)
            end
    end.

do_save_slot(SlotNumber, Slot, ParkedCalls, #cf_call{account_db=Db}=Call) ->
    case couch_mgr:save_doc(Db, wh_json:set_value([<<"slots">>, SlotNumber], Slot, ParkedCalls)) of
        {ok, Parked} ->
            ?LOG("successfully stored call parking data in slot ~p", [SlotNumber]),
            {ok, Parked};
        {error, conflict} ->
            save_slot(SlotNumber, Slot, get_parked_calls(Call), Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% After an attended transfer we need to find the callid that we stored
%% because it was the "C-Leg" of a transfer and now we have the
%% actuall "A-Leg".  Find the old callid and update it with the new one.
%% @end
%%--------------------------------------------------------------------
-spec update_call_id/3 :: (ne_binary(), wh_json:json_object(), #cf_call{}) -> 'ok'.
update_call_id(Replaces, ParkedCalls, #cf_call{account_db=Db, channel_vars=CCVs
                                               ,cid_name=CIDName, cid_number=CIDNum}=Call) ->
    CallId = cf_exe:callid(Call),
    Slots = wh_json:get_value(<<"slots">>, ParkedCalls, wh_json:new()),
    case find_slot_by_callid(Slots, Replaces) of
        {ok, SlotNumber, Slot} ->
            ?LOG("update the call id ~s in slot '~s' with ~s", [Replaces, SlotNumber, CallId]),
            CallerHost = get_switch_hostname(Call),
            Updaters = [fun(J) -> wh_json:set_value(<<"Call-ID">>, CallId, J) end
                        ,fun(J) -> wh_json:set_value(<<"Node">>, CallerHost, J) end
                        ,fun(J) -> wh_json:set_value(<<"CID-Number">>, CIDNum, J) end
                        ,fun(J) -> wh_json:set_value(<<"CID-Name">>, CIDName, J) end
                        ,fun(J) ->  
                                 Referred = wh_json:get_value(<<"Referred-By">>, CCVs),
                                 ReOptions = [{capture, [1], binary}],
                                 case catch(re:run(Referred, <<".*sip:(.*)@.*">>, ReOptions)) of
                                     {match, [Match]} -> 
                                         case get_endpoint_id(Match, Call) of
                                             undefined -> wh_json:delete_key(<<"Ringback-ID">>, J);
                                             RingbackId -> wh_json:set_value(<<"Ringback-ID">>, RingbackId, J)
                                         end;
                                     _ -> 
                                         wh_json:delete_key(<<"Ringback-ID">>, J)
                                 end
                         end
                       ],
            UpdatedSlot = lists:foldr(fun(F, J) -> F(J) end, Slot, Updaters),
            JObj = wh_json:set_value([<<"slots">>, SlotNumber], UpdatedSlot, ParkedCalls),
            case couch_mgr:save_doc(Db, JObj) of
                {ok, _} ->
                    publish_usurp_control(Call),
                    PresenceId = wh_json:get_value(<<"Presence-ID">>, Slot),
                    ?LOG("update presence-id '~s' with state: early", [PresenceId]),
                    cf_call_command:presence(<<"early">>, PresenceId, Call),                    
                    {ok, SlotNumber, UpdatedSlot};
                {error, conflict} ->
                    update_call_id(Replaces, get_parked_calls(Call), Call)
            end;
        {error, not_found}=E ->
            E
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Given the parked calls and a list of parked keys find the slot with
%% the provided call id.
%% @end
%%--------------------------------------------------------------------
-spec find_slot_by_callid/2 :: (wh_json:json_object(), ne_binary()) -> {ok, wh_json:json_object()} | {error, not_found}.
-spec find_slot_by_callid/3 :: ([ne_binary(),...], wh_json:json_object(), ne_binary()) -> {ok, wh_json:json_object()} |
                                                                                          {error, not_found}.

find_slot_by_callid(Slots, CallId) ->
    find_slot_by_callid(wh_json:get_keys(Slots), Slots, CallId).

find_slot_by_callid([], _, _) ->
    {error, not_found};
find_slot_by_callid([SlotNumber|SlotNumbers], Slots, CallId) ->
    Slot = wh_json:get_value(SlotNumber, Slots),
    case wh_json:get_value(<<"Call-ID">>, Slot) of
        CallId -> {ok, SlotNumber, Slot};
        _ -> find_slot_by_callid(SlotNumbers, Slots, CallId)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to retrieve the parked calls list from the datastore, if
%% the list does not exist then it returns an new empty instance
%% @end
%%--------------------------------------------------------------------
-spec get_parked_calls/1 :: (#cf_call{}) -> wh_json:json_object().
get_parked_calls(#cf_call{account_db=Db, account_id=Id}) ->
    case couch_mgr:open_doc(Db, ?PARKED_CALLS) of
        {error, not_found} ->
            Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            Generators = [fun(J) -> wh_json:set_value(<<"_id">>, <<"parked_calls">>, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_type">>, <<"parked_calls">>, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_account_db">>, Db, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_account_id">>, Id, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_created">>, Timestamp, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_modified">>, Timestamp, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_vsn">>, <<"1">>, J) end
                          ,fun(J) -> wh_json:set_value(<<"slots">>, wh_json:new(), J) end],
            lists:foldr(fun(F, J) -> F(J) end, wh_json:new(), Generators);
        {ok, JObj} ->
            JObj
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
-spec cleanup_slot/3 :: (ne_binary(), ne_binary(), #cf_call{}) -> boolean().
cleanup_slot(SlotNumber, ParkedCall, #cf_call{account_db=Db}=Call) ->
    case couch_mgr:open_doc(Db, ?PARKED_CALLS) of
        {ok, JObj} ->
            case wh_json:get_value([<<"slots">>, SlotNumber, <<"Call-ID">>], JObj) of
                ParkedCall -> 
                    ?LOG("clean up matched the call id in the slot, terminating presence"),
                    PresenceId = wh_json:get_value([<<"slots">>, SlotNumber, <<"Presence-ID">>], JObj),
                    ?LOG("update presence-id '~s' with state: terminated", [PresenceId]),
                    cf_call_command:presence(<<"terminated">>, PresenceId, Call),
                    case couch_mgr:save_doc(Db, wh_json:delete_key([<<"slots">>, SlotNumber], JObj)) of
                        {ok, _} -> true;
                        {error, conflict} -> cleanup_slot(SlotNumber, ParkedCall, Call);
                        {error, _R} ->
                            ?LOG("failed to clean up our slot: ~p", [_R]),
                            false
                    end;
                _Else ->
                    ?LOG("call parked in slot ~s is ~s and we expected ~s, skipping clean up", [SlotNumber, _Else, ParkedCall]),
                    false
            end;
        {error, _R} ->
            ?LOG("failed to open the parked calls doc: ~p", [_R]),
            false
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%%
%% @end
%%--------------------------------------------------------------------
wait_for_pickup(SlotNumber, undefined, Call) ->
    cf_call_command:b_hold(Call),
    cleanup_slot(SlotNumber, cf_exe:callid(Call), Call);
wait_for_pickup(SlotNumber, RingbackId, Call) ->
    case cf_call_command:b_hold(30000, Call) of
        {error, timeout} ->
            case ringback_parker(RingbackId, <<"26">>, Call) of
                answered -> cf_exe:continue(Call);
                _ -> wait_for_pickup(SlotNumber, RingbackId, Call)
            end;
        _ ->
            ?LOG("parked caller has been picked up or hungup"),
            cleanup_slot(SlotNumber, cf_exe:callid(Call), Call),
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
-spec get_node_ip/1 :: (ne_binary()) -> ne_binary().
get_node_ip(Node) ->
    {ok, Addresses} = inet:getaddrs(wh_util:to_list(Node), inet),
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
-spec publish_usurp_control/1 :: (#cf_call{}) -> 'ok'.
-spec publish_usurp_control/2 :: (ne_binary(), #cf_call{}) -> 'ok'.

publish_usurp_control(Call) ->
    publish_usurp_control(cf_exe:callid(Call), Call).

publish_usurp_control(CallId, Call) ->
    Notice = [{<<"Call-ID">>, CallId}
              ,{<<"Control-Queue">>, cf_exe:control_queue_name(Call)}
              ,{<<"Controller-Queue">>, cf_exe:queue_name(Call)}
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
-spec get_endpoint_id/2 :: (ne_binary(), #cf_call{}) -> undefined | ne_binary().
get_endpoint_id(undefined, _) ->
    undefined;
get_endpoint_id(Username, #cf_call{account_db=Db}) ->
    ViewOptions = [{<<"key">>, Username}
                   ,{<<"limit">>, 1}
                  ],
    case couch_mgr:get_results(Db, <<"cf_attributes/sip_credentials">>, ViewOptions) of
        {ok, [Device]} -> wh_json:get_value(<<"id">>, Device);
        _ -> undefined
    end.    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ringback the device that parked the call
%% @end
%%--------------------------------------------------------------------
ringback_parker(EndpointId, Timeout, Call) ->
    case cf_endpoint:build(EndpointId, wh_json:from_list([{<<"can_call_self">>, true}]), Call) of
        {ok, Endpoints} ->
            ?LOG("attempting to ringback ~s", [EndpointId]),
            case cf_call_command:b_bridge(Endpoints, Timeout, Call) of
                {ok, _} ->
                    ?LOG("completed successful bridge to the ringback device"),
                    answered;
                _ ->
                    ?LOG("ringback failed, returning caller to parking slot"),
                    failed
            end;
        _ -> failed
    end.
