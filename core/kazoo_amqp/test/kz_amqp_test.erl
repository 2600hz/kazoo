%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Test module to work on recovering after AMQP disconnect
%%%
%%% Creates a basic publisher and consumer and starts appropriate timers. When
%%% the AMQP disconnect/reconnect occurs, the test checks if publishing resumes
%%% and consuming is re-established.
%%%
%%% Run interactively in the shell with:
%%% f(P), P = kz_amqp_test:start().
%%% ... restart rabbit ...
%%% P ! stop.
%%%
%%% 'stop' will terminate both the consumer and producer
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
%%% @end
%%%-----------------------------------------------------------------------------
-module(kz_amqp_test).

-export([start/0]).

-include_lib("kazoo_amqp/include/kz_amqp.hrl").

-spec start() -> pid().
start() ->
    spawn(fun run_it/0).

run_it() ->
    Pub = start_publisher(),
    Con = start_consumer(),
    lager:info("started pub: ~p and con: ~p", [Pub, Con]),
    wait([Pub, Con]).

wait([]) -> 'ok';
wait(PidRefs) ->
    receive
        {'DOWN', Ref, 'process', Pid, _Reason} ->
            case lists:keytake(Pid, 1, PidRefs) of
                'false' ->
                    lager:info("ignoring down pid ~p", [Pid]),
                    wait(PidRefs);
                {'value', {Pid, Ref}, PRs} ->
                    lager:info("pid ~p down: ~p", [Pid, _Reason]),
                    wait(PRs)
            end;
        {Client, 'status'} ->
            Client ! {'status', PidRefs},
            wait(PidRefs);
        'stop' ->
            lager:info("stopping"),
            [P ! 'stop' || {P, _} <- PidRefs]
    end.

start_publisher() ->
    spawn_monitor(fun run_publisher/0).

run_publisher() ->
    kz_log:put_callid(<<?MODULE_STRING"-publisher">>),
    'ok' = kz_amqp_channel:requisition(),

    Payload = [{<<"Category">>, <<"amqp">>}
              ,{<<"Key">>, <<"recv">>}
               | kz_api:default_headers(<<?MODULE_STRING>>, <<"1.0">>)
              ],
    loop_publisher(Payload).

loop_publisher(Payload) ->
    loop_publisher(Payload, 2600).

loop_publisher(Payload, Timeout) ->
    Start = kz_time:start_time(),
    receive
        'stop' -> lager:info("publisher instructed to stop");
        {'kz_amqp_assignment',{'new_channel',_IsNew, _Channel}} ->
            lager:info("publisher moved to ~s channel ~p", [_IsNew, _Channel]),
            lager:info("publisher broker: ~p", [kz_amqp_channel:consumer_broker()]),
            publish_payload(Payload),
            loop_publisher(Payload);
        {'kz_amqp_assignment', 'lost_channel'} ->
            lager:info("producer has lost its channel, waiting"),
            loop_publisher(Payload, 'infinity');
        Msg ->
            lager:info("publisher recv ~p", [Msg]),
            loop_publisher(Payload, decr(Timeout, Start))
    after
        Timeout ->
            publish_payload(Payload),
            loop_publisher(Payload)
    end.

publish_payload(Payload) ->
    kapi_sysconf:publish_get_req(Payload).


start_consumer() ->
    spawn_monitor(fun run_consumer/0).

run_consumer() ->
    kz_log:put_callid(<<?MODULE_STRING"-consumer">>),
    'ok' = kz_amqp_channel:requisition(),

    loop_consumer(<<?MODULE_STRING>>).

loop_consumer(Queue) ->
    loop_consumer(Queue, 5000).

loop_consumer(Queue, Timeout) ->
    Start = erlang:monotonic_time(),
    receive
        'stop' -> lager:info("consumer stopping");
        {#'basic.deliver'{}=_Deliver
        ,#amqp_msg{props=#'P_basic'{}, payload=Payload}
        } ->
            lager:info("recv payload ~s", [Payload]),
            loop_consumer(Queue);
        {'kz_amqp_assignment',{'new_channel','false', _Channel}} ->
            lager:info("consumer channel is ~p", [_Channel]),
            Queue = kz_amqp_util:new_queue(<<?MODULE_STRING>>, []),
            'ok' = kz_amqp_util:basic_consume(Queue, [{'no_ack', 'true'}]),
            loop_consumer(Queue);
        {'kz_amqp_assignment',{'new_channel','true', _Channel}} ->
            lager:info("new consumer channel is ~p", [_Channel]),
            Queue = kz_amqp_util:new_queue(<<?MODULE_STRING>>, []),
            'ok' = kz_amqp_util:basic_consume(Queue, [{'no_ack', 'true'}]),
            loop_consumer(Queue);
        {'kz_amqp_assignment', 'lost_channel'} ->
            lager:info("consumer has lost its channel, waiting"),
            loop_consumer(Queue, 'infinity');
        #'basic.consume_ok'{consumer_tag=CTag} ->
            lager:info("consumer is consuming on tag ~s", [CTag]),
            kapi_sysconf:bind_q(Queue, ['get']),
            loop_consumer(Queue, decr(Timeout, Start));
        Msg ->
            lager:info("consumer recv ~p", [Msg]),
            loop_consumer(Queue, decr(Timeout, Start))
    after
        Timeout ->
            lager:notice("consumer timed out waiting for payload")
    end.

decr('infinity', _Start) -> 'infinity';
decr(Timeout, Start) ->
    kz_time:decr_timeout(Timeout, Start).

%% WHY a watcher?
