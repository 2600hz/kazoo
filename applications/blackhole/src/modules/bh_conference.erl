%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2014, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Peter Defebvre
%%%   Ben Wann
%%%-------------------------------------------------------------------
-module(bh_conference).

-export([handle_event/2
         ,add_amqp_binding/2, rm_amqp_binding/2
        ]).

%%%-------------------------------------------------------------------
%%% THIS REQUIRES WORK FROM KAZ-27 TO OPERATE
%%% THIS IS HERE ONLY AS A PLACEHOLDER FOR NOW
%%% DO NOT WORRY ABOUT IT UNTIL KAZ-27 IS DONE
%%% AND COMMITTED
%%%-------------------------------------------------------------------

-include("../blackhole.hrl").

-spec handle_event(bh_context:context(), wh_json:object()) -> _.
handle_event(Context, EventJObj) ->
    lager:debug("handling conference event ~s", [get_response_key(EventJObj)]),
    blackhole_data_emitter:emit(bh_context:websocket_pid(Context)
                                ,get_response_key(EventJObj)
                                ,EventJObj
                               ).

-spec add_amqp_binding(ne_binary(), bh_context:context()) -> 'ok'.
add_amqp_binding(<<"conference.event.", ConfId/binary>>, _Context) ->
    blackhole_listener:add_binding('conference', [{'restrict_to', [{'conference', ConfId}]}]);
add_amqp_binding(Binding, _Context) ->
    lager:debug("unmatched binding ~p", [Binding]),
    'ok'.

-spec rm_amqp_binding(ne_binary(), bh_context:context()) -> 'ok'.
rm_amqp_binding(<<"conference.event.", ConfId/binary>>, _Context) ->
    blackhole_listener:remove_binding('conference', [{'restrict_to', [{'conference', ConfId}]}]);
rm_amqp_binding(Binding, _Context) ->
    lager:debug("unmatched binding ~p", [Binding]),
    'ok'.

%%%===================================================================
%%% Internal functions
%%%==================================================================
get_response_key(JObj) ->
    wh_json:get_first_defined([<<"Application-Name">>, <<"Event-Name">>], JObj).
