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

-include_lib("blackhole/src/blackhole.hrl").

-spec handle_event(bh_context:context(), kz_json:object()) -> 'ok'.
handle_event(Context, EventJObj) ->
    lager:debug("handling conference event ~s", [get_response_key(EventJObj)]),
    blackhole_data_emitter:emit(bh_context:websocket_pid(Context)
                                ,get_response_key(EventJObj)
                                ,EventJObj
                               ).

-spec add_amqp_binding(ne_binary(), bh_context:context()) -> 'ok'.
add_amqp_binding(<<"conference.event.", ConfId/binary>>, _Context) ->
    blackhole_listener:add_binding('conference', [{'conference', ConfId}, {'restrict_to', ['event']}]);
add_amqp_binding(<<"conference.command.", ConfId/binary>>, _Context) ->
    blackhole_listener:add_binding('conference', [{'conference', ConfId}, {'restrict_to', ['command']}]);
add_amqp_binding(Binding, _Context) ->
    lager:debug("unmatched binding ~p", [Binding]),
    'ok'.

-spec rm_amqp_binding(ne_binary(), bh_context:context()) -> 'ok'.
rm_amqp_binding(<<"conference.event.", ConfId/binary>>, _Context) ->
    blackhole_listener:remove_binding('conference', [{'conference', ConfId}, {'restrict_to', ['event']}]);
rm_amqp_binding(<<"conference.command.", ConfId/binary>>, _Context) ->
    blackhole_listener:remove_binding('conference', [{'conference', ConfId}, {'restrict_to', ['command']}]);
rm_amqp_binding(Binding, _Context) ->
    lager:debug("unmatched binding ~p", [Binding]),
    'ok'.

%%%===================================================================
%%% Internal functions
%%%==================================================================
get_response_key(JObj) ->
    kz_json:get_first_defined([<<"Application-Name">>, <<"Event-Name">>], JObj).
