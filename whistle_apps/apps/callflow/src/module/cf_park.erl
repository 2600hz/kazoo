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

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module sends an arbitrary response back to the
%% call originator.
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> no_return()).
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId, channel_vars=CCVs}=Call) ->
    put(callid, CallId),
    ParkedCalls = get_parked_calls(Call),
    SlotNumber = get_slot_number(ParkedCalls, Call),
    ReferredTo = wh_json:get_ne_value(<<"Referred-To">>, CCVs, <<>>),
    CallerHost = get_switch_hostname(Call),
    case re:run(ReferredTo, "Replaces=([^;]*)", [{capture, [1], binary}]) of
        nomatch when ReferredTo =:= <<>> ->
            ?LOG("call was the result of a direct dial"),
            case wh_json:get_value(<<"action">>, Data, <<"park">>) of
                <<"park">> ->
                    ?LOG("action is to park the call"),
                    park_call(SlotNumber, ParkedCalls, undefined, Call),
                    CFPid ! {transfer};
                <<"retrieve">> ->
                    ?LOG("action is to retrieve a parked call"),
                    retrieve(SlotNumber, ParkedCalls, CallerHost, Call),
                    CFPid ! {hangup};
                <<"auto">> ->
                    ?LOG("action is to automatically determine if we should retrieve or park"),
                    retrieve(SlotNumber, ParkedCalls, CallerHost, Call)
                        orelse park_call(SlotNumber, ParkedCalls, undefined, Call),
                    CFPid ! {transfer}
            end;
        nomatch ->
            ?LOG("call was the result of a blind transfer, assuming intention was to park"),
            park_call(SlotNumber, ParkedCalls, ReferredTo, Call),
            CFPid ! {transfer};
        {match, [Replaces]} ->
            ?LOG("call was the result of an attended-transfer completion, updating call id"),
            update_call_id(Replaces, ParkedCalls, Call),
            CFPid ! {transfer}
    end.


get_switch_hostname(#cf_call{call_id=undefined}) ->
    undefined;
get_switch_hostname(Call) ->
    case cf_call_command:b_status(Call) of
        {ok, CallerStatus} ->
            wh_json:get_ne_value(<<"Switch-Hostname">>, CallerStatus);
        _Else ->
            undefined
    end.

get_switch_hostname(CallId, Call) ->
    get_switch_hostname(Call#cf_call{call_id=CallId}).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the appropriate action to retrieve a parked call
%% @end
%%--------------------------------------------------------------------
-spec retrieve/4 :: (SlotNumber, ParkedCalls, CallerHost, Call) -> boolean() when
      SlotNumber :: binary(),
      ParkedCalls :: json_object(),
      CallerHost :: json_object(),
      Call :: #cf_call{}.
retrieve(SlotNumber, ParkedCalls, CallerHost, #cf_call{to_user=ToUser, to_realm=ToRealm, call_id=CallId}=Call) ->
    case wh_json:get_value([<<"slots">>, SlotNumber], ParkedCalls) of
        undefined ->
            ?LOG("They hungup? play back nobody here message", []),
            false;
        Slot ->
            ParkedCall = wh_json:get_value(<<"Call-ID">>, Slot),
            case get_switch_hostname(ParkedCall, Call) of
                undefined ->
                    ?LOG("THEY HUNGUP! playback nobody here and clean up", []),
                    false;
                CallerHost ->
                    ?LOG("pickup call id ~s", [ParkedCall]),
                    Command = [{<<"Application-Name">>, <<"call_pickup">>}
                               ,{<<"Target-Call-ID">>, ParkedCall}
                               ,{<<"Call-ID">>, CallId}
                               | wh_api:default_headers(<<>>, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
                              ],
                    {ok, Payload} = wapi_dialplan:call_pickup([ KV || {_, V}=KV <- Command, V =/= undefined ]),
                    cf_call_command:send_callctrl(Payload, Call),
                    true;
                OtherNode ->
                    IP = get_node_ip(OtherNode),
                    Contact = <<"sip:", ToUser/binary, "@", ToRealm/binary>>,
                    Server = <<"sip:", IP/binary, ":5060">>,
                    redirect(Contact, Server, Call),
                    true
            end
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
-spec get_node_ip/1 :: (Node) -> binary() when
      Node :: binary().
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
%% Determine the appropriate action to park the current call scenario
%% @end
%%--------------------------------------------------------------------
-spec park_call/4 :: (SlotNumber, ParkedCalls, ReferredTo, Call) -> ok when
      SlotNumber :: binary(),
      ParkedCalls :: json_object(),
      ReferredTo :: undefined | binary(),
      Call :: #cf_call{}.
park_call(SlotNumber, ParkedCalls, ReferredTo, Call) ->
    ?LOG("attempting to park call in slot ~p", [SlotNumber]),
    Slot = create_slot(Call),
    save_slot(SlotNumber, Slot, ParkedCalls, Call),
    case ReferredTo of
        undefined ->
            ?LOG("playback slot number to caller"),
            cf_call_command:b_answer(Call),
            cf_call_command:b_say(wh_util:to_binary(SlotNumber), Call);
        _ ->
            ok
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the json object representing the call in the parking slot
%% @end
%%--------------------------------------------------------------------
-spec create_slot/1 :: (#cf_call{}) -> json_object().
create_slot(#cf_call{call_id=CallId}) ->
    wh_json:from_list([{<<"Call-ID">>, CallId}]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Returns the provided slot number or the next avaliable if none
%% was provided
%% @end
%%--------------------------------------------------------------------
-spec get_slot_number/2 :: (ParkedCalls, Call) -> binary() when
      ParkedCalls :: json_object(),
      Call :: #cf_call{}.
get_slot_number(_, #cf_call{capture_group=CG}) when is_binary(CG) andalso size(CG) > 0 ->
    CG;
get_slot_number(ParkedCalls, _) ->
    Slots = wh_json:get_value(<<"slots">>, ParkedCalls, []),
    Next = case [wh_util:to_integer(Key) || Key <- wh_json:get_keys(Slots)] of
               [] -> 100;
               Keys ->
                   hd(lists:dropwhile(fun(E) ->
                                              lists:member(E, Keys)
                                      end, lists:seq(100, lists:max(Keys) + 1)))
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
-spec save_slot/4 :: (SlotNumber, Slot, ParkedCalls, Call) -> integer() when
      SlotNumber :: binary(),
      Slot :: json_object(),
      ParkedCalls :: json_object(),
      Call :: #cf_call{}.
save_slot(SlotNumber, Slot, ParkedCalls, #cf_call{account_db=Db}=Call) ->
    case couch_mgr:save_doc(Db, wh_json:set_value([<<"slots">>, SlotNumber], Slot, ParkedCalls)) of
        {ok, Parked} ->
            ?LOG("successfully stored call parking data in slot ~p", [SlotNumber]),
            Parked;
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
-spec update_call_id/3 :: (Replaces, ParkedCalls, Call) -> ok when
      Replaces :: binary(),
      ParkedCalls :: json_object(),
      Call :: #cf_call{}.
update_call_id(Replaces, ParkedCalls, #cf_call{call_id=CallId, account_db=Db}=Call) ->
    {struct, Slots} = wh_json:get_value(<<"slots">>, ParkedCalls, ?EMPTY_JSON_OBJECT),
    Updated = lists:map(fun({Number, Props}=Slot) ->
                                case wh_json:get_value(<<"Call-ID">>, Props) of
                                    Replaces ->
                                        ?LOG("update the call id ~s in slot ~p with ~s", [Replaces, Number, CallId]),
                                        {Number, wh_json:set_value(<<"Call-ID">>, CallId, Props)};
                                    _ ->
                                        Slot
                                end
                        end, Slots),
    case couch_mgr:save_doc(Db, wh_json:set_value(<<"slots">>, {struct, Updated}, ParkedCalls)) of
        {ok, _} ->
            ok;
        {error, conflict} ->
            update_call_id(Replaces, get_parked_calls(Call), Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Attempts to retrieve the parked calls list from the datastore, if
%% the list does not exist then it returns an new empty instance
%% @end
%%--------------------------------------------------------------------
-spec get_parked_calls/1 :: (Call) -> json_object() when
      Call :: #cf_call{}.
get_parked_calls(#cf_call{account_db=Db, account_id=Id}) ->
    case couch_mgr:open_doc(Db, <<"parked_calls">>) of
        {error, not_found} ->
            Timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
            Generators = [fun(J) -> wh_json:set_value(<<"_id">>, <<"parked_calls">>, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_type">>, <<"parked_calls">>, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_account_db">>, Db, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_account_id">>, Id, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_created">>, Timestamp, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_modified">>, Timestamp, J) end
                          ,fun(J) -> wh_json:set_value(<<"pvt_vsn">>, <<"1">>, J) end
                          ,fun(J) -> wh_json:set_value(<<"slots">>, ?EMPTY_JSON_OBJECT, J) end],
            lists:foldr(fun(F, J) -> F(J) end, ?EMPTY_JSON_OBJECT, Generators);
        {ok, JObj} ->
            JObj
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Create a redirect request to the Contact on Server
%% @end
%%--------------------------------------------------------------------
-spec redirect/3 :: (Contact, Server, Call) -> ok when
      Contact :: binary(),
      Server :: binary(),
      Call :: #cf_call{}.
redirect(Contact, Server, #cf_call{call_id=CallId, ctrl_q=CtrlQ}) ->
    ?LOG("redirect to ~s on ~s", [Contact, Server]),
    Command = [{<<"Redirect-Contact">>, Contact}
               ,{<<"Redirect-Server">>, Server}
               ,{<<"Application-Name">>, <<"redirect">>}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(<<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)],
    {ok, Payload} = wapi_dialplan:redirect(Command),
    amqp_util:callctl_publish(CtrlQ, Payload),
    timer:sleep(2000).
