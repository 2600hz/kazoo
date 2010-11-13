-module(tester).

-export([test/0]).

test() ->
    H = net_adm:localhost(),
    Q = amqp_util:new_targeted_queue(H, <<>>),
    amqp_util:bind_q_to_targeted(H, Q),
    amqp_util:basic_consume(H, Q),

    Req = [{<<"Event-Category">>, <<"originate">>}
	   ,{<<"Event-Name">>, <<"resource_req">>}
	   ,{<<"Server-ID">>, Q}
	   ,{<<"App-Name">>, <<"ecallmgr.tester">>}
	   ,{<<"Route">>, {<<"user:2600pbx">>, <<"4158867905">>}}
	   ,{<<"App-Version">>, <<"1.2.3.4">>}
	   ,{<<"Msg-ID">>, <<"msg1234">>}
	   ,{<<"Resource-Type">>, <<"audio">>}
	  ],
    {ok, JSON} = whistle_api:resource_req(Req),
    amqp_util:callmgr_publish(H, JSON, <<"application/json">>, <<"resource.req">>),
    loop().

loop() ->
    receive
	Msg ->
	    io:format("Test Msg: ~p~n", [Msg]),
	    loop()
    after
	5000 ->
	    io:format("Test going down~n", [])
    end.
    
