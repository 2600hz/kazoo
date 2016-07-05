%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%% James Aimonetti
%%% Peter Defebvre
%%% Ben Wann
%%%-------------------------------------------------------------------
-module(bh_call).

-export([handle_event/2
        ,add_amqp_binding/2, rm_amqp_binding/2
        ]).

-include("blackhole.hrl").

-spec handle_event(bh_context:context(), kz_json:object()) -> 'ok'.
handle_event(Context, EventJObj) ->
    kz_util:put_callid(EventJObj),
    lager:debug("handle_event fired for ~s ~s", [bh_context:account_id(Context), bh_context:websocket_session_id(Context)]),
    'true' = kapi_call:event_v(EventJObj)
        andalso is_account_event(Context, EventJObj),
    lager:debug("valid event and emitting to ~p: ~s", [bh_context:websocket_pid(Context), event_name(EventJObj)]),
    blackhole_data_emitter:emit(bh_context:websocket_pid(Context), event_name(EventJObj), EventJObj).

is_account_event(Context, EventJObj) ->
    kz_json:get_first_defined([<<"Account-ID">>
                              ,[<<"Custom-Channel-Vars">>, <<"Account-ID">>]
                              ], EventJObj
                             ) =:=
        bh_context:account_id(Context).

-spec event_name(kz_json:object()) -> ne_binary().
event_name(JObj) ->
    kz_json:get_value(<<"Event-Name">>, JObj).

-spec add_amqp_binding(ne_binary(), bh_context:context()) -> 'ok'.
add_amqp_binding(<<"call.", _/binary>>, Context) ->
    blackhole_listener:add_call_binding(bh_context:account_id(Context));
add_amqp_binding(_Binding, _Context) ->
    lager:debug("unmatched binding ~s", [_Binding]),
    'ok'.

-spec rm_amqp_binding(ne_binary(), bh_context:context()) -> 'ok'.
rm_amqp_binding(<<"call.", _/binary>>, Context) ->
    blackhole_listener:remove_call_binding(bh_context:account_id(Context));
rm_amqp_binding(_Binding, _Context) ->
    lager:debug("unmatched binding ~s", [_Binding]),
    'ok'.
