%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   Peter Defebvre
%%%-------------------------------------------------------------------
-module(bh_qubicle).

-export([
    handle_event/2,
    add_amqp_binding/2,
    rm_amqp_binding/2
]).

-include("../blackhole.hrl").

-spec handle_event(bh_context:context(), wh_json:object()) -> 'ok'.
handle_event(Context, EventJObj) ->
    lager:debug("handle_event fired for ~s ~s", [bh_context:account_id(Context), bh_context:websocket_session_id(Context)]),
    'true' = is_account_event(Context, EventJObj),
    lager:debug("valid event and emitting to ~p: ~s", [bh_context:websocket_pid(Context), event_name(EventJObj)]),
    J = wh_json:normalize_jobj(EventJObj),
    blackhole_data_emitter:emit(bh_context:websocket_pid(Context), event_name(EventJObj), J).

-spec is_account_event(bh_context:context(), wh_json:object()) -> any().
is_account_event(Context, EventJObj) ->
    wh_json:get_first_defined([<<"Account-ID">>
                               ,[<<"Custom-Channel-Vars">>, <<"Account-ID">>]
                              ], EventJObj
                             )
        =:= bh_context:account_id(Context).

-spec event_name(wh_json:object()) -> ne_binary().
event_name(JObj) ->
    case wh_json:get_value(<<"Event-Category">>, JObj) of
        'undefined' -> <<"qubicle">>;
        EventType   -> EventType
    end.

-spec add_amqp_binding(ne_binary(), bh_context:context()) -> 'ok'.
add_amqp_binding(<<"qubicle.recipient">>, _Context) ->
    blackhole_listener:add_binding('qubicle_recipient', [{'event_only', 'true'}]);

add_amqp_binding(<<"qubicle.session">>, _Context) ->
    blackhole_listener:add_binding('qubicle_session', [{'event_only', 'true'}]);

add_amqp_binding(<<"qubicle.queue">>, _Context) ->
    blackhole_listener:add_binding('qubicle_queue', [{'event_only', 'true'}]);

add_amqp_binding(_Binding, _Context) ->
    lager:debug("unmatched binding ~p", [_Binding]).

-spec rm_amqp_binding(ne_binary(), bh_context:context()) -> 'ok'.
rm_amqp_binding(<<"qubicle.recipient">>, _Context) ->
    blackhole_listener:remove_binding('qubicle_recipient', []);

rm_amqp_binding(_Binding, _Context) ->
    lager:debug("unmatched binding ~s", [_Binding]).
