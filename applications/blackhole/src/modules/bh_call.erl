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
        ,subscribe/2, unsubscribe/2
        ]).

-include("blackhole.hrl").

-define(LISTEN_TO, [
                    <<"CHANNEL_CREATE">>, <<"CHANNEL_ANSWER">>, <<"CHANNEL_DESTROY">>, <<"CHANNEL_BRIDGE">>
                   ,<<"PARK_PARKED">>, <<"PARK_RETRIEVED">>, <<"PARK_ABANDONED">>
                   ]).

-spec handle_event(bh_context:context(), kz_json:object()) -> 'ok'.
handle_event(#bh_context{binding=Binding} = Context, EventJObj) ->
    kz_util:put_callid(EventJObj),
    lager:debug("handle_event fired for ~s ~s", [bh_context:account_id(Context), bh_context:websocket_session_id(Context)]),
    'true' = kapi_call:event_v(EventJObj)
        andalso is_account_event(Context, EventJObj),
    lager:debug("valid event and emitting to ~p: ~s", [bh_context:websocket_pid(Context), event_name(EventJObj)]),
    NormJObj = kz_json:normalize_jobj(kz_json:set_value(<<"Binding">>, Binding, EventJObj)),
    blackhole_data_emitter:emit(bh_context:websocket_pid(Context), event_name(EventJObj), NormJObj).

is_account_event(Context, EventJObj) ->
    kz_json:get_first_defined([<<"Account-ID">>
                              ,[<<"Custom-Channel-Vars">>, <<"Account-ID">>]
                              ], EventJObj
                             ) =:=
        bh_context:account_id(Context).

-spec event_name(kz_json:object()) -> ne_binary().
event_name(JObj) ->
    kz_json:get_value(<<"Event-Name">>, JObj).

-spec subscribe(bh_context:context(), ne_binary()) -> {'ok', bh_context:context()}.
subscribe(Context, <<"call.*.*">>) ->
    AccountId = bh_context:account_id(Context),
    add_call_binding(AccountId, Context, ?LISTEN_TO),
    {'ok', Context};
subscribe(Context, <<"call.", Binding/binary>>) ->
    case binary:split(Binding, <<".">>, ['global']) of
        [Event, <<"*">>] ->
            AccountId = bh_context:account_id(Context),
            add_call_binding(AccountId, Context, [Event]);
        _ ->
            blackhole_util:send_error_message(Context, <<"unmatched binding">>, Binding)
    end,
    {'ok', Context};
subscribe(Context, Binding) ->
    blackhole_util:send_error_message(Context, <<"unmatched binding">>, Binding),
    {'ok', Context}.

-spec unsubscribe(bh_context:context(), ne_binary()) -> {'ok', bh_context:context()}.
unsubscribe(Context, <<"call.*.*">>) ->
    AccountId = bh_context:account_id(Context),
    rm_call_binding(AccountId, Context, ?LISTEN_TO),
    {'ok', Context};
unsubscribe(Context, <<"call.", Binding/binary>>) ->
    case binary:split(Binding, <<".">>, ['global']) of
        [Event, <<"*">>] ->
            AccountId = bh_context:account_id(Context),
            rm_call_binding(AccountId, Context, [Event]);
        _ ->
            blackhole_util:send_error_message(Context, <<"unmatched binding">>, Binding)
    end,
    {'ok', Context};
unsubscribe(Context, Binding) ->
    blackhole_util:send_error_message(Context, <<"unmatched binding">>, Binding),
    {'ok', Context}.

-spec add_call_binding(ne_binary(), bh_context:context(), [ne_binary()]) -> ok.
add_call_binding(_AccountId, _Context, []) -> ok;
add_call_binding(AccountId, Context, [Event | Events]) ->
    blackhole_bindings:bind(<<"call.", Event/binary, ".*">>, ?MODULE, 'handle_event', Context),
    blackhole_listener:add_call_binding(AccountId, Event),
    add_call_binding(AccountId, Context, Events).

-spec rm_call_binding(ne_binary(), bh_context:context(), [ne_binary()]) -> ok.
rm_call_binding(_AccountId, _Context, []) -> ok;
rm_call_binding(AccountId, Context, [Event | Events]) ->
    blackhole_bindings:unbind(<<"call.", Event/binary, ".*">>, ?MODULE, 'handle_event', Context),
    blackhole_listener:remove_call_binding(AccountId, Event),
    rm_call_binding(AccountId, Context, Events).
