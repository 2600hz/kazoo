-module(bh_conference).

-export([handle_event/2
         ,add_amqp_binding/2
        ]).

-include("../blackhole.hrl").

%% conference.event.{conference_id}

-spec handle_event(bh_context:context(), wh_json:object()) -> any().
handle_event(Context, EventJObj) ->
    blackhole_data_emitter:emit(bh_context:session_pid(Context), event_name(EventJObj), EventJObj).

add_amqp_binding(Binding, Context) ->
    'ok'.

%%%===================================================================
%%% Internal functions
%%%==================================================================
event_name(JObj) ->
    wh_json:get_value(<<"Event-Name">>, JObj).

fw_participant_event(JObj, Pids) ->
    Event = cleanup_binary(wh_json:get_value(<<"Event">>, JObj)),
    fw_participant_event(Event, JObj, Pids).

fw_participant_event(Event, JObj, Pids) ->
    CleanJObj = clean_participant_event(JObj),
    Id = wh_json:get_value(<<"Call-ID">>, JObj),
    blackhole_sockets:send_event(Pids
                                 ,Event
                                 ,[Id, CleanJObj]
                                ).

fw_conference_event(JObj, Pids) ->
    ConfId = wh_json:get_value(<<"Conference-ID">>, JObj),
    Event = cleanup_binary(wh_json:get_value(<<"Event">>, JObj)),
    blackhole_sockets:send_event(Pids
                                 ,Event
                                 ,[ConfId]
                                ).

clean_participant_event(JObj) ->
    RemoveKeys = [<<"Focus">>
                  ,<<"App-Version">>
                  ,<<"App-Name">>
                  ,<<"Event-Category">>
                  ,<<"Event-Name">>
                  ,<<"Msg-ID">>
                  ,<<"Node">>
                  ,<<"Server-ID">>
                  ,<<"Switch-Hostname">>
                  ,<<"Mute-Detect">>
                  ,<<"Custom-Channel-Vars">>
                 ],
    CleanKeys = [{<<"Energy-Level">>, <<"energy_level">>, fun wh_util:to_integer/1}
                 ,{<<"Current-Energy">>, <<"current_energy">>, fun wh_util:to_integer/1}
                 ,{<<"Talking">>, <<"talking">>, fun wh_util:to_boolean/1}
                 ,{<<"Speak">>, <<"mute">>, fun(X) -> not wh_util:to_boolean(X) end}
                 ,{<<"Hear">>, <<"hear">>, fun wh_util:to_boolean/1}
                 ,{<<"Video">>, <<"video">>, fun wh_util:to_boolean/1}
                 ,{<<"Floor">>, <<"floor">>, fun wh_util:to_boolean/1}
                 ,{<<"Event">>, <<"event">>, fun cleanup_binary/1}
                 ,{<<"Custom-Channel-Vars">>, <<"pin">>, fun(X) -> wh_json:get_value(<<"pin">>, X) end}
                ],
    clean_jobj(JObj, RemoveKeys, CleanKeys).


clean_jobj(JObj, RemoveKeys, []) ->
    JObj1 = wh_json:delete_keys(RemoveKeys, JObj),
    wh_json:foldl(
        fun(K, V, Acc) ->
            wh_json:set_value(cleanup_binary(K), V, Acc)
        end
        ,wh_json:new()
        ,JObj1
    );
clean_jobj(JObj, RemoveKeys, [{OldKey, NewKey} | T]) ->
    Value = wh_json:get_value(OldKey, JObj),
    J1 = wh_json:set_value(NewKey, Value, JObj),
    clean_jobj(wh_json:delete_key(OldKey, J1), RemoveKeys, T);
clean_jobj(JObj, RemoveKeys, [{OldKey, NewKey, Fun} | T]) ->
    case wh_json:get_value(OldKey, JObj) of
        'undefined' ->
            J1 = wh_json:set_value(NewKey, <<"undefined">>, JObj),
            clean_jobj(wh_json:delete_key(OldKey, J1), RemoveKeys, T);
        Value ->
            J1 = wh_json:set_value(NewKey, Fun(Value), JObj),
            clean_jobj(wh_json:delete_key(OldKey, J1), RemoveKeys, T)
    end.

cleanup_binary(Binary) ->
    String = binary:bin_to_list(Binary),
    Binary1 = binary:list_to_bin(string:to_lower(String)),
    binary:replace(Binary1, <<"-">>, <<"_">>, [global]).
