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
handle(Data, #cf_call{cf_pid=CFPid, call_id=CallId, ctrl_q=CtrlQ}=Call) ->
    put(callid, CallId),
    ParkedCalls = get_parked_calls(Call),
    case wh_json:get_value(<<"action">>, Data, <<"park">>) of
        <<"park">> ->
            cf_call_command:b_answer(Call),
            park_call(ParkedCalls, Call);
        <<"retrieve">> ->
            retrieve(ParkedCalls, Call)
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the appropriate action to retrieve a parked call
%% @end
%%--------------------------------------------------------------------
-spec retrieve/2 :: (ParkedCalls, Call) -> no_return() when
      ParkedCalls :: json_object(),
      Call :: json_object().
retrieve(ParkedCalls, Call) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determine the appropriate action to park the current call scenario
%% @end
%%--------------------------------------------------------------------
-spec park_call/2 :: (ParkedCalls, Call) -> ok | integer() when
      ParkedCalls :: json_object(),
      Call :: #cf_call{}.
park_call(ParkedCalls, Call) ->
    {ok, Status} = cf_call_command:b_status(Call),
    ReferredTo = wh_json:get_value([<<"Custom-Channel-Vars">>, <<"Referred-To">>], Status, <<>>),
    case re:run(ReferredTo, "Replaces=([^;]*)", [{capture, [1], binary}]) of
        {match, [Replaces]} ->
            ?LOG("request to park call was the result of an attended-transfer completion"),
            update_call_id(Replaces, ParkedCalls, Call);
        nomatch when ReferredTo =:= <<>> ->
            ?LOG("request to park call was the result of a direct dial"),
            SlotNumber = park_aleg(wh_json:get_value(<<"Node">>, Status), ParkedCalls, Call),
            cf_call_command:b_say(wh_util:to_binary(SlotNumber), Call);
        nomatch ->
            ?LOG("request to park call was the result of a blind transfer"),
            park_aleg(wh_json:get_value(<<"Node">>, Status), ParkedCalls, Call)
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
%% Attempts to but the current call into next avaliable slot
%% @end
%%--------------------------------------------------------------------
-spec park_aleg/2 :: (Node, ParkedCalls, Call) -> integer() when
      Node :: binary(),
      ParkedCalls :: json_object(),
      Call :: #cf_call{}.
park_aleg(Node, ParkedCalls, #cf_call{cf_pid=CFPid, call_id=CallId}=Call) ->
    JObj = wh_json:from_list([{<<"call_id">>, CallId}
                              ,{<<"node">>, Node}]),
    set_next_slot(JObj, ParkedCalls, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Determines the next available parking slot and places the call
%% data there.  If, on save, it conflicts then it gets the new instance
%% and tries again, determining the new slot.
%% @end
%%--------------------------------------------------------------------
-spec set_next_slot/3 :: (JObj, ParkedCalls, Call) -> integer() when
      JObj :: json_object(),
      ParkedCalls :: json_object(),
      Call :: #cf_call{}.
set_next_slot(JObj, ParkedCalls, #cf_call{account_db=Db}=Call) ->
    Slots = wh_json:get_value(<<"slots">>, ParkedCalls, []),
    Next = case [wh_util:to_integer(Key) || Key <- wh_json:get_keys(Slots)] of
               [] -> 1;
               Keys ->
                   hd(lists:dropwhile(fun(E) ->
                                              lists:member(E, Keys)
                                      end, lists:seq(1, lists:max(Keys) + 1)))
           end,
    case couch_mgr:save_doc(Db, wh_json:set_value([<<"slots">>, Next], JObj, ParkedCalls)) of
        {ok, _} ->
            ?LOG("successfully stored call parking data in slot ~p", [Next]),
            Next;
        {error, conflict} ->
            set_next_slot(JObj, get_parked_calls(Call), Call)
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
