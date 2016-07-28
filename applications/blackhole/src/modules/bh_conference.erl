%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz Inc
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
        ,subscribe/2, unsubscribe/2
        ]).

-include("blackhole.hrl").

-spec handle_event(bh_context:context(), kz_json:object()) -> 'ok'.
handle_event(Context, EventJObj) ->
    lager:debug("handling conference event ~s", [get_response_key(EventJObj)]),
    blackhole_data_emitter:emit(bh_context:websocket_pid(Context)
                               ,get_response_key(EventJObj)
                               ,kz_json:normalize_jobj(EventJObj)
                               ).

-spec subscribe(ne_binary(), bh_context:context()) -> {'ok', bh_context:context()}.
subscribe(Context, <<"conference.command.*">> = Binding) ->
    blackhole_util:send_error_message(Context, <<"unmatched binding">>, Binding),
    {'ok', Context};
subscribe(Context, <<"conference.command.", ConfId/binary>>) ->
    BindKey = <<"conference.command.", ConfId/binary>>,
    blackhole_listener:add_binding('conference', command_binding_options(ConfId)),
    blackhole_bindings:bind(BindKey, ?MODULE, 'handle_event', Context),
    {'ok', Context};
subscribe(Context, <<"conference.event.*.*">> = Binding) ->
    blackhole_util:send_error_message(Context, <<"unmatched binding">>, Binding),
    {'ok', Context};
subscribe(Context, <<"conference.event.*.", _CallId/binary>> = Binding) ->
    blackhole_util:send_error_message(Context, <<"unmatched binding">>, Binding),
    {'ok', Context};
subscribe(Context, <<"conference.event.", Binding/binary>>) ->
    case binary:split(Binding, <<".">>, ['global']) of
        [ConfId, CallId] ->
            BindKey = kz_util:join_binary([<<"conference.event">>, ConfId, CallId], <<".">>),
            blackhole_listener:add_binding('conference', event_binding_options(ConfId, CallId)),
            blackhole_bindings:bind(BindKey, ?MODULE, 'handle_event', Context);
        _Else ->
            blackhole_util:send_error_message(Context, <<"unmatched binding">>, Binding)
    end,
    {'ok', Context};
subscribe(Binding, Context) ->
    blackhole_util:send_error_message(Context, <<"unmatched binding">>, Binding),
    {'ok', Context}.

-spec unsubscribe(bh_context:context(), ne_binary()) -> {'ok', bh_context:context()}.
unsubscribe(Context, <<"conference.command.*">> = Binding) ->
    blackhole_util:send_error_message(Context, <<"unmatched binding">>, Binding),
    {'ok', Context};
unsubscribe(<<"conference.command.", ConfId/binary>>, Context) ->
    BindKey = <<"conference.command.", ConfId/binary>>,
    blackhole_listener:remove_binding('conference', command_binding_options(ConfId)),
    blackhole_bindings:unbind(BindKey, ?MODULE, 'handle_event', Context),
    {'ok', Context};
unsubscribe(Context, <<"conference.event.*.*">> = Binding) ->
    blackhole_util:send_error_message(Context, <<"unmatched binding">>, Binding),
    {'ok', Context};
unsubscribe(Context, <<"conference.event.*.", _CallId/binary>> = Binding) ->
    blackhole_util:send_error_message(Context, <<"unmatched binding">>, Binding),
    {'ok', Context};
unsubscribe(<<"conference.event.", Binding/binary>>, Context) ->
    case binary:split(Binding, <<".">>, ['global']) of
        [ConfId, CallId] ->
            BindKey = kz_util:join_binary([<<"conference.event">>, ConfId, CallId], <<".">>),
            blackhole_listener:remove_binding('conference', event_binding_options(ConfId, CallId)),
            blackhole_bindings:unbind(BindKey, ?MODULE, 'handle_event', Context);
        _Else ->
            blackhole_util:send_error_message(Context, <<"unmatched binding">>, Binding)
    end,
    {'ok', Context};
unsubscribe(Binding, Context) ->
    blackhole_util:send_error_message(Context, <<"unmatched binding">>, Binding),
    {'ok', Context}.

%%%===================================================================
%%% Internal functions
%%%==================================================================

-spec get_response_key(kz_json:object()) -> ne_binary().
get_response_key(JObj) ->
    kz_json:get_first_defined([<<"Application-Name">>, <<"Event-Name">>], JObj).

-spec command_binding_options(ne_binary()) -> kz_proplist().
command_binding_options(ConfId) ->
    [{'conference', ConfId}
    ,{'restrict_to', ['command']}
    ,'federate'
    ].

-spec event_binding_options(ne_binary(), ne_binary()) -> kz_proplist().
event_binding_options(ConfId, CallId) ->
    [{'conference', {ConfId, CallId}}
    ,{'restrict_to', ['event']}
    ,'federate'
    ].