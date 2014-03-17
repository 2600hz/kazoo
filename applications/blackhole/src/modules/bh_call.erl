-module(bh_call).

-export([init/0]).
-export([subscribe/3, unsubscribe/3]).
-export([create/3]).

init() ->
    _ = blackhole_bindings:bind(<<"v1.calls.*">>, ?MODULE, 'subscribe'),
    _ = blackhole_bindings:bind(<<"call.create">>, ?MODULE, 'create'),
    _ = blackhole_bindings:bind(<<"v1.calls.*">>, ?MODULE, 'unsubscribe').

subscribe(Data, SessionId, SessionPid) ->
    AccountId = wh_json:get_value(<<"account_id">>, Data),
    lager:debug("listening for call events for account: ~p", [AccountId]),
    {'ok', ListenerPid} = bh_call_sup:start_listener(AccountId),
    bh_call_listener:subscribe(ListenerPid, SessionPid),
    blackhole_handler:add_listener_to_context(ListenerPid, 'bh_call_listener', SessionId, SessionPid),
    blackhole_data_emitter:send_data(SessionPid, <<"connected">>, [AccountId]).

create(_Data, _SessionId, _SessionPid) ->
    'ok'.

unsubscribe(Data, SessionId, SessionPid) ->
    AccountId = wh_json:get_value(<<"account_id">>, Data),
    {'ok', ListenerPid} = blackhole_call_events_sup:get_listener(AccountId),
    bh_call_listener:unsubscribe(ListenerPid, SessionPid),
    blackhole_handler:remove_listener_from_context(ListenerPid,'bh_call_listener', SessionId, SessionPid).
