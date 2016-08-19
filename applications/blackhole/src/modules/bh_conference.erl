%%%-------------------------------------------------------------------
%%% @copyright (C) 2012-2016, 2600Hz Inc
%%% @doc
%%%
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Peter Defebvre
%%%   Ben Wann
%%%   Roman Galeev
%%%-------------------------------------------------------------------
-module(bh_conference).
-include("blackhole.hrl").

-export([handle_event/2, subscribe/3, unsubscribe/3]).

-spec handle_event(bh_context:context(), kz_json:object()) -> 'ok'.
handle_event(Context, EventJObj) ->
    blackhole_util:handle_event(Context, EventJObj, get_response_key(EventJObj)).

-spec subscribe(ne_binary(), bh_context:context(), kz_json:object()) -> bh_subscribe_result().
subscribe(_Context, <<"conference.command.*">> = _Binding, _JObj) ->
    {'error', <<"Unmatched binding">>};
subscribe(Context, <<"conference.command.", ConfId/binary>>, _JObj) ->
    BindKey = <<"conference.command.", ConfId/binary>>,
    blackhole_listener:add_binding('conference', command_binding_options(ConfId)),
    blackhole_bindings:bind(BindKey, ?MODULE, 'handle_event', Context),
    {'ok', Context};
subscribe(_Context, <<"conference.event.*.*">> = _Binding, _JObj) ->
    {'error', <<"Unmatched binding">>};
subscribe(_Context, <<"conference.event.*.", _CallId/binary>> = _Binding, _JObj) ->
    {'error', <<"Unmatched binding">>};
subscribe(Context, <<"conference.event.", Args/binary>> = Binding, _JObj) ->
    case binary:split(Args, <<".">>, ['global']) of
        [ConfId, CallId] ->
            blackhole_listener:add_binding('conference', event_binding_options(ConfId, CallId)),
            blackhole_bindings:bind(Binding, ?MODULE, 'handle_event', Context),
            {'ok', Context};
        _Else ->
            {'error', <<"Unmatched binding">>}
    end;
subscribe(_Binding, _Context, _JObj) ->
    {'error', <<"Unmatched binding">>}.


-spec unsubscribe(bh_context:context(), ne_binary(), kz_json:object()) -> bh_subscribe_result().
unsubscribe(_Context, <<"conference.command.*">> = _Binding, _JObj) ->
    {'error', <<"Unmatched binding">>};
unsubscribe(Context, <<"conference.command.", ConfId/binary>> = Binding, _JObj) ->
    blackhole_listener:remove_binding('conference', command_binding_options(ConfId)),
    blackhole_bindings:unbind(Binding, ?MODULE, 'handle_event', Context),
    {'ok', Context};
unsubscribe(_Context, <<"conference.event.*.*">> = _Binding, _JObj) ->
    {'error', <<"Unmatched binding">>};
unsubscribe(_Context, <<"conference.event.*.", _CallId/binary>> = _Binding, _JObj) ->
    {'error', <<"Unmatched binding">>};
unsubscribe(Context, <<"conference.event.", Args/binary>> = Binding, _JObj) ->
    case binary:split(Args, <<".">>, ['global']) of
        [ConfId, CallId] ->
            blackhole_listener:remove_binding('conference', event_binding_options(ConfId, CallId)),
            blackhole_bindings:unbind(Binding, ?MODULE, 'handle_event', Context),
            {'ok', Context};
        _Else ->
            {'error', <<"Unmatched binding">>}
    end;
unsubscribe(_Context, _Binding, _JObj) ->
    {'error', <<"Unmatched binding">>}.

%%%===================================================================
%%% Internal functions
%%%==================================================================

-spec get_response_key(kz_json:object()) -> ne_binary().
get_response_key(JObj) ->
    kz_json:get_first_defined([<<"Application-Name">>, <<"Event-Name">>], JObj).

-spec command_binding_options(ne_binary()) -> kz_proplist().
command_binding_options(ConfId) ->
    [{'restrict_to', [{'command', ConfId}]}
    ,'federate'
    ].

-spec event_binding_options(ne_binary(), ne_binary()) -> kz_proplist().
event_binding_options(ConfId, CallId) ->
    [{'restrict_to', [{'event', {ConfId, CallId}}]}
    ,'federate'
    ].
