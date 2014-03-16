-module(bh_conference).

-export([init/0, subscribe/2, unsubscribe/2, create/2]).

init() ->
    [{<<"conference.events.subscribe">>, {?MODULE, 'subscribe'}}
    ,{<<"conference.events.subscription.status">>, {?MODULE, 'subscription_status'}}
    ,{<<"conference.create">>, {?MODULE, 'create'}}
    ,{<<"conference.events.unsubscribe">>, {?MODULE, 'unsubscribe'}}
    ].

subscribe(Data, SessionPid) ->
    lager:debug('conferenceconnection happened'),
    ConfId = wh_json:get_value(<<"conference_id">>, Data),
    User = wh_json:get_value(<<"user_name">>, Data),
    {'ok', ListenerPid} = bh_conference_sup:start_listener(ConfId),
    bh_conference_listener:subscribe(ListenerPid, SessionPid),
    blackhole_data_emitter:send_data(SessionPid, <<"connected">>, [User, ConfId]).

subscription_status(Data, SessionPid) ->
    

create(Data, Pid) ->
    'ok'.

unsubscribe(Data, Pid) ->
    'ok'.
