-module(monitor_test).

-export([ping/1, ping/2]).

-include("../include/monitor_amqp.hrl").

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

ping(Dest) ->
    ping (Dest, 1).

ping(Dest, Count) ->
    {ok, Q} = start_amqp(net_adm:localhost()),

    Api = [{<<"Event-Category">>, <<"task">>}
       ,{<<"Event-Name">>, <<"ping_req">>}
       ,{<<"App-Name">>, <<"monitor">>}
       ,{<<"App-Version">>, <<"0.1.0">>}
       ,{<<"Server-ID">>, Q}
       ,{<<"Msg-ID">>, <<"monitor-ping-test-id">>}
       ,{<<"Destination">>, monitor_util:to_binary(Dest)}
       ,{<<"Count">>, monitor_util:to_binary(Count)}
      ],

    {ok, JSON} = monitor_api:ping_req(Api),
    amqp_util:monitor_publish(net_adm:localhost(), JSON, <<"application/json">>, ?KEY_AGENT_NET_REQ),    
    loop(Q).

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

start_amqp(AHost) ->
    amqp_util:monitor_exchange(AHost),
    amqp_util:targeted_exchange(AHost),

    Agent_Q = amqp_util:new_monitor_queue(AHost),

    %% Bind the queue to the targeted exchange
    format_log(info, "MONITOR_TEST(~p): Bind ~p as a targeted queue~n", [self(), Agent_Q]),
    amqp_util:bind_q_to_targeted(AHost, Agent_Q),

    %% Bind the queue to the topic exchange
    format_log(info, "MONITOR_TEST(~p): Bind ~p for <<\"#\">>~n", [self(), Agent_Q]),
    amqp_util:bind_q_to_monitor(AHost, Agent_Q, <<"#">>),

    %% Register a consumer to listen to the queue
    amqp_util:basic_consume(AHost, Agent_Q),

    {ok, Agent_Q}.

handle_req(ContentType, Payload) ->
    case ContentType of
    <<"application/json">> ->
        {struct, Prop} = mochijson2:decode(binary_to_list(Payload)),
        format_log(info, "MONITOR_TEST(~p): Recieved a message of type: ~p~nPayload: ~p~n", [self(), ContentType, Prop]);
    _ ->
        format_log(info, "MONITOR_TEST(~p): recieved unknown msg type ~p~n", [self(), ContentType])
    end.
