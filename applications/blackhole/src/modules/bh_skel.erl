%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%% James Aimonetti
%%% Peter Defebvre
%%% Ben Wann
%%%-------------------------------------------------------------------
-module(bh_skel).

-export([handle_event/2
         ,add_amqp_binding/2
        ]).

-include("../blackhole.hrl").


-spec handle_event(bh_context:context(), wh_json:object()) -> any().
handle_event(Context, EventJObj) ->
    wh_util:put_callid(EventJObj),
    lager:debug("handle_event fired for ~s ~s", [bh_context:account_id(Context), bh_context:websocket_session_id(Context)]),
    blackhole_data_emitter:emit(bh_context:websocket_pid(Context), event_name(EventJObj), EventJObj).

event_name(JObj) ->
    wh_json:get_value(<<"Event-Name">>, JObj).

add_amqp_binding(<<"call.", _/binary>>, Context) ->
    lager:debug("adding amqp binding....."),
    blackhole_listener:add_call_binding(bh_context:account_id(Context));
add_amqp_binding(_Binding, _Context) ->
    'ok'.

