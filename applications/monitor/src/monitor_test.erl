-module(monitor_test).

-export([ping/1]).

-include("../include/monitor_amqp.hrl").

ping(Dest) ->
    Api = [{<<"Event-Category">>, <<"task">>}
       ,{<<"Event-Name">>, <<"ping_req">>}
       ,{<<"App-Name">>, <<"monitor">>}
       ,{<<"App-Version">>, <<"0.1.0">>}
       ,{<<"Server-ID">>, <<"1234567890">>}
       ,{<<"Msg-ID">>, <<"monitor-ping-test-id">>}
       ,{<<"Destination">>, list_to_binary(Dest)}
       ,{<<"Count">>, <<"1">>}
      ],

    {ok, JSON} = monitor_api:ping_req(Api),
    amqp_util:monitor_publish(net_adm:localhost(), JSON, <<"application/json">>, ?KEY_AGENT_NET_REQ).

%% loop() ->
%%    receive
%%    M -> io:format("Msg: ~p~n", [M]),
%%         loop()
%%    after
%%    10000 ->
%%        io:format("Tester down~n", [])
%%    end.

%% connect() ->
%%    amqp_util:new_monitor_queue(net_adm:localhost()).
