-module(bh_conference).

-export([init/0]).
-export([subscribe/3, unsubscribe/3]).
-export([subscription_status/3, create/3]).

init() ->
    _ = blackhole_bindings:bind(<<"conference.events.subscribe">>, ?MODULE, 'subscribe'),
    _ = blackhole_bindings:bind(<<"conference.events.subscription.status">>, ?MODULE, 'subscription_status'),
    _ = blackhole_bindings:bind(<<"conference.create">>, ?MODULE, 'create'),
    _ = blackhole_bindings:bind(<<"conference.events.unsubscribe">>, ?MODULE, 'unsubscribe').

subscribe(Data, SessionId, SessionPid) ->
    lager:debug('conference connection happened'),
    ConfId = wh_json:get_value(<<"conference_id">>, Data),
    User = wh_json:get_value(<<"user_name">>, Data),
    {'ok', ListenerPid} = bh_conference_sup:start_listener(ConfId),
    bh_conference_listener:subscribe(ListenerPid, SessionPid),
    blackhole_data_emitter:send_data(SessionPid, <<"connected">>, [User, ConfId]).

subscription_status(Data, SessionId, SessionPid) ->
    'ok'.

create(Data, SessionId, SessionPid) ->
    'ok'.

unsubscribe(Data, SessionId, SessionPid) ->
    'ok'.
