-module(monitor_test).

-export([ping/1]).

-include("../include/monitor_amqp.hrl").

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

ping(Dest) ->
    {ok, Q} = start_amqp(),

    Api = [{<<"Event-Category">>, <<"task">>}
       ,{<<"Event-Name">>, <<"ping_req">>}
       ,{<<"App-Name">>, <<"monitor">>}
       ,{<<"App-Version">>, <<"0.1.0">>}
       ,{<<"Server-ID">>, Q}
       ,{<<"Msg-ID">>, <<"monitor-ping-test-id">>}
       ,{<<"Destination">>, list_to_binary(Dest)}
       ,{<<"Count">>, <<"1">>}
      ],

    {ok, JSON} = monitor_api:ping_req(Api),
    amqp_util:monitor_publish(net_adm:localhost(), JSON, <<"application/json">>, ?KEY_AGENT_NET_REQ),    
    spawn(fun() -> loop(Q) end).

loop(Q) ->
    receive
    {_, #amqp_msg{props = Props, payload = Payload}} ->
        handle_req(Props#'P_basic'.content_type, Payload),
        loop(Q);
    M -> 
        format_log(info, "MONITOR_TEST(~p): Does this make sense to you?~n~p~n", [self(), M]),
        loop(Q)
    after
    20000 ->
        amqp_util:queue_delete(net_adm:localhost(), Q),
        format_log(info, "MONITOR_TEST(~p): goodbye cruel world~n", [self()])
    end.

start_amqp() ->
    amqp_util:targeted_exchange(net_adm:localhost()),

    Agent_Q = amqp_util:new_monitor_queue(net_adm:localhost()),

    %% Bind the queue to an exchange
    amqp_util:bind_q_to_targeted(net_adm:localhost(), Agent_Q),
    amqp_util:bind_q_to_monitor(net_adm:localhost(), Agent_Q, <<"#">>),

    %% Register a consumer to listen to the queue
    amqp_util:basic_consume(net_adm:localhost(), Agent_Q),

    {ok, Agent_Q}.

handle_req(ContentType, Payload) ->
    case ContentType of
    <<"application/json">> ->
        {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
        format_log(info, "MONITOR_TEST(~p): Recieved a message of type: ~p~nPayload: ~p~n", [self(), ContentType, Prop]);
    _ ->
        format_log(info, "MONITOR_TEST(~p): recieved unknown msg type ~p~n", [self(), ContentType])
    end.
