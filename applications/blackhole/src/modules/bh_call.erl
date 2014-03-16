-module(bh_call).

-export([init/0, subscribe/2, unsubscribe/2, create/2]).

init() ->
    [{<<"call.events.subscribe">>, {?MODULE, 'subscribe'}}
    ,{<<"call.create">>, {?MODULE, 'create'}}
    ,{<<"call.events.unsubscribe">>, {?MODULE, 'unsubscribe'}}
    ].

subscribe(Data, Pid) ->
    AccountId = wh_json:get_value(<<"account_id">>, Data),
    lager:debug("listening for call events for account: ~p", [AccountId]),
    {'ok', PidListener} = bh_call_sup:start_listener(AccountId),
    bh_call_listener:subscribe(PidListener, Pid).

create(Data, Pid) ->
    'ok'.

unsubscribe(Data, Pid) ->
    AccountId = wh_json:get_value(<<"account_id">>, Data),
    {'ok', PidListener} = blackhole_call_events_sup:get_listener(AccountId),
    bh_call_listener:unsubscribe(PidListener, Pid).
