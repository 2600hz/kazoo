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
-include("blackhole.hrl").

-export([handle_event/2, subscribe/3, unsubscribe/3]).

-define(LISTEN_TO, [
                    <<"CHANNEL_CREATE">>, <<"CHANNEL_ANSWER">>, <<"CHANNEL_DESTROY">>, <<"CHANNEL_BRIDGE">>
                   ,<<"PARK_PARKED">>, <<"PARK_RETRIEVED">>, <<"PARK_ABANDONED">>
                   ]).

-spec handle_event(bh_context:context(), kz_json:object()) -> 'ok'.
handle_event(Context, EventJObj) ->
    'true' = kapi_call:event_v(EventJObj),
    blackhole_util:handle_event(Context, EventJObj, event_name(EventJObj)).

-spec event_name(kz_json:object()) -> ne_binary().
event_name(JObj) ->
    kz_json:get_value(<<"Event-Name">>, JObj).

-spec subscribe(bh_context:context(), ne_binary(), kz_json:object()) -> bh_subscribe_result().
subscribe(Context, <<"call.*.*">>, JObj) ->
    AccountId = blackhole_util:get_account(Context, JObj),
    add_call_binding(AccountId, Context, ?LISTEN_TO),
    {'ok', Context};
subscribe(Context, <<"call.", Binding/binary>>, JObj) ->
    case binary:split(Binding, <<".">>, ['global']) of
        [Event, <<"*">>] ->
            AccountId = blackhole_util:get_account(Context, JObj),
            add_call_binding(AccountId, Context, [Event]),
            {'ok', Context};
        _ ->
            {'error', <<"Unmatched binding">>}
    end;
subscribe(_Context, _Binding, _JObj) ->
    {'error', <<"Unmatched binding">>}.

-spec unsubscribe(bh_context:context(), ne_binary(), kz_json:object()) -> bh_subscribe_result().
unsubscribe(Context, <<"call.*.*">>, JObj) ->
    AccountId = blackhole_util:get_account(Context, JObj),
    rm_call_binding(AccountId, Context, ?LISTEN_TO),
    {'ok', Context};
unsubscribe(Context, <<"call.", Binding/binary>>, JObj) ->
    case binary:split(Binding, <<".">>, ['global']) of
        [Event, <<"*">>] ->
            AccountId = blackhole_util:get_account(Context, JObj),
            rm_call_binding(AccountId, Context, [Event]),
            {'ok', Context};
        _ ->
            {'error', <<"Unmatched binding">>}
    end;
unsubscribe(_Context, _Binding, _JObj) ->
    {'error', <<"Unmatched binding">>}.

-spec add_call_binding(ne_binary(), bh_context:context(), [ne_binary()]) -> ok.
add_call_binding(_AccountId, _Context, []) -> ok;
add_call_binding(AccountId, Context, [Event | Events]) ->
    blackhole_bindings:bind(<<"call.", AccountId/binary, ".", Event/binary, ".*">>, ?MODULE, 'handle_event', Context),
    blackhole_listener:add_call_binding(AccountId, Event),
    add_call_binding(AccountId, Context, Events).

-spec rm_call_binding(ne_binary(), bh_context:context(), [ne_binary()]) -> ok.
rm_call_binding(_AccountId, _Context, []) -> ok;
rm_call_binding(AccountId, Context, [Event | Events]) ->
    blackhole_bindings:unbind(<<"call.", AccountId/binary, ".", Event/binary, ".*">>, ?MODULE, 'handle_event', Context),
    blackhole_listener:remove_call_binding(AccountId, Event),
    rm_call_binding(AccountId, Context, Events).
