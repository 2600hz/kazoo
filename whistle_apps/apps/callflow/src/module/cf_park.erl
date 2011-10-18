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
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId}=Call) ->
    put(callid, CallId),
    ParkedCalls = get_parked_calls(Call),
    SlotNumber = get_slot_number(ParkedCalls, Call),

    {ok, Status} = cf_call_command:b_status(Call),

    case wh_json:get_value(<<"action">>, Data, <<"park">>) of
        <<"park">> ->
            park_call(SlotNumber, ParkedCalls, Call);
        <<"retrieve">> ->
            retrieve(SlotNumber, ParkedCalls, hangup, Call);
        <<"auto">> ->
            retrieve(SlotNumber, ParkedCalls, park, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the appropriate action to retrieve a parked call
%% @end
%%--------------------------------------------------------------------
-spec retrieve/4 :: (SlotNumber, ParkedCalls, MissingAction, Call) -> no_return() when
      SlotNumber :: binary(),
      ParkedCalls :: json_object(),
      MissingAction :: park | hangup,
      Call :: #cf_call{}.
retrieve(SlotNumber, ParkedCalls, MissingAction, #cf_call{to=To}=Call) ->
    case wh_json:get_value([<<"slots">>, SlotNumber], ParkedCalls) of
        undefined when MissingAction =:= park ->
            io:format("PARK IN SLOT ~p~n", [SlotNumber]);
        undefined ->
            io:format("They hungup?~n", []);
        Slot ->
            {ok, Status} = cf_call_command:b_status(Call),
            Node = wh_json:get_value(<<"Node">>, Status, <<"unknown">>),
            case wh_json:get_value(<<"Node">>, Slot) of
                undefined ->
                    io:format("THEY HUNGUP!~n", []);
                Node ->
                    ParkedCall = wh_json:get_value(<<"Call-ID">>, Slot),
                    io:format("Bridge to callid ~p~n", [ParkedCall]);
                OtherNode ->
                    IP = get_node_ip(OtherNode),
                    redirect(To, <<"sip:", IP/binary, ":5060">>, Call)
            end
    end.

get_node_ip(Node) ->
    [_, Server] = binary:split(Node, <<"@">>),
    {ok, Addresses} = inet:getaddrs(wh_util:to_list(Server), inet),
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
-spec park_call/3 :: (SlotNumber, ParkedCalls, Call) -> ok | integer() when
      SlotNumber :: binary(),
      ParkedCalls :: json_object(),
      Call :: #cf_call{}.
park_call(SlotNumber, ParkedCalls, Call) ->
    {ok, Status} = cf_call_command:b_status(Call),
    ReferredTo = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Referred-To">>], Status, <<>>),
    case re:run(ReferredTo, "Replaces=([^;]*)", [{capture, [1], binary}]) of
        nomatch when ReferredTo =:= <<>> ->
            ?LOG("request to park call was the result of a direct dial"),
            Slot = create_slot(Status, Call),
            save_slot(SlotNumber, Slot, ParkedCalls, Call),
            cf_call_command:b_answer(Call),
            cf_call_command:b_say(wh_util:to_binary(SlotNumber), Call);
        nomatch ->
            ?LOG("request to park call was the result of a blind transfer"),
            Slot = create_slot(Status, Call),
            save_slot(SlotNumber, Slot, ParkedCalls, Call);
        {match, [Replaces]} ->
            ?LOG("request to park call was the result of an attended-transfer completion"),
            update_call_id(Replaces, ParkedCalls, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Builds the json object representing the call in the parking slot
%% @end
%%--------------------------------------------------------------------
-spec create_slot/2 :: (Status, Call) -> integer() when
      Status :: json_object(),
      Call :: #cf_call{}.
create_slot(Status, #cf_call{call_id=CallId}) ->
    Node = wh_json:get_value(<<"Node">>, Status),
    wh_json:from_list([{<<"Call-ID">>, CallId}
                       ,{<<"Node">>, Node}]).

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
               [] -> 1;
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
                                case wh_json:get_value(<<"call_id">>, Props) of
                                    Replaces ->
                                        ?LOG("update the call id ~s in slot ~p with ~s~n", [Replaces, Number, CallId]),
                                        {Number, wh_json:set_value(<<"call_id">>, CallId, Props)};
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

redirect(Contact, Server, #cf_call{call_id=CallId, ctrl_q=CtrlQ}) ->
    Command = [{<<"Application-Name">>, <<"respond">>}
               ,{<<"Response-Code">>, <<"302">>}
               ,{<<"Response-Message">>, Contact}
               ,{<<"Redirect-Server">>, Server}
               ,{<<"Call-ID">>, CallId}
               | wh_api:default_headers(<<>>, <<"call">>, <<"command">>, <<"call_response">>, <<"0.1.0">>)],
    io:format("Redirect: ~p~n", [Command]),
    {ok, Payload} = wh_api:respond_req(Command),
    amqp_util:callctl_publish(CtrlQ, Payload).
