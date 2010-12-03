%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.com>
%%% @copyright (C) 2010, Karl Anderson
%%% @doc
%%% Responsible for runnning the call server monitoring tasks
%%% @end
%%% Created : 2 Dec 2010 by Karl Anderson <karl@2600hz.com>
%%%-------------------------------------------------------------------
-module(monitor_call_basic).

%% API
-export([start/3]).

-define(SERVER, ?MODULE).

-import(logger, [format_log/3]).
-import(proplists, [get_value/2, get_value/3]).

-include("../include/monitor_amqp.hrl").
-include("../include/monitor_agent_call.hrl").

start(AHost, Msg, Route) ->
    Msg_ID = get_value(<<"Msg-ID">>, Msg),
    {ok, Task_Q} = create_task_q(AHost),
    {ok, {CQ, Call_ID}} = originate_call_req(AHost, Msg_ID, Route, Task_Q),
    amqp_util:bind_q_to_callevt(AHost, Task_Q, Call_ID),
    answer_call(AHost, CQ, Call_ID, Task_Q),
    test_tones(AHost, CQ, Call_ID, Task_Q),
    hangup_call(AHost, CQ, Call_ID, Task_Q),
    format_log(info, "MONITOR_CALL_BASIC(~p): Task complete, thanks for all the fish~n", [self()]).

create_task_q(AHost) ->
    Q = amqp_util:new_monitor_queue(AHost),

    %% Bind the queue to the targeted exchange
    format_log(info, "MONITOR_CALL_BASIC(~p): Bind ~p as a targeted queue for task~n", [self(), Q]),
    amqp_util:bind_q_to_targeted(AHost, Q),

    %% Register a consumer to listen to the queue
    format_log(info, "MONITOR_CALL_BASIC(~p): Consume on ~p for task~n", [self(), Q]),
    amqp_util:basic_consume(AHost, Q),

    {ok, Q}.

originate_call_req(AHost, Msg_ID, Route, Server_ID) ->
    Def = monitor_api:default_headers(Server_ID, <<"originate">>, <<"resource_req">>, Msg_ID),
    Req = [
         {<<"Resource-Type">>, <<"audio">>}
        ,{<<"Route">>, Route}
    ],
    {ok, JSON} = whistle_api:resource_req(lists:append([Def, Req])),
    format_log(info, "MONITOR_CALL_BASIC(~p): Generated originate req~nPayload: ~p~n", [self(), Req]),
    amqp_util:callmgr_publish(AHost, JSON, <<"application/json">>, ?KEY_RESOURCE_REQ),
    {ok, Msg} = wait_for_msg_type(<<"originate">>, <<"resource_resp">>, 15000),
    CQ = get_value(<<"Control-Queue">>, Msg),
    Call_ID = get_value(<<"Call-ID">>, Msg),
    {ok, {CQ, Call_ID}}.

answer_call(AHost, CQ, Call_ID, Server_ID) ->
    Def = monitor_api:default_headers(Server_ID, <<"call_control">>, <<"command">>),
    Req = [
         {<<"Call-ID">>, Call_ID}
        ,{<<"Application-Name">>, <<"answer">>}
    ],
    {ok, JSON} = whistle_api:answer_req(lists:append([Def, Req])),
    amqp_util:callctl_publish(AHost, CQ, JSON).

test_tones(AHost, CQ, Call_ID, Server_ID) ->
    arm_tone_detector(AHost, CQ, Call_ID, Server_ID),
    generate_tone(AHost, CQ, Call_ID, Server_ID),
    {ok, _Msg} = wait_for_call_event_complete(<<"park">>, 2000).

hangup_call(AHost, CQ, Call_ID, Server_ID) ->
    Def = monitor_api:default_headers(Server_ID, <<"call_control">>, <<"command">>),
    Req = [
         {<<"Call-ID">>, Call_ID}
        ,{<<"Application-Name">>, <<"hangup">>}
    ],
    {ok, JSON} = whistle_api:hangup_req(lists:append([Def, Req])),
    amqp_util:callctl_publish(AHost, CQ, JSON).

arm_tone_detector(AHost, CQ, Call_ID, Server_ID) ->
    Def = monitor_api:default_headers(Server_ID, <<"call_control">>, <<"command">>),
    Req = [
         {<<"Call-ID">>, Call_ID}
        ,{<<"Application-Name">>, <<"tone_detect">>}
        ,{<<"Tone-Detect-Name">>, Call_ID}
        ,{<<"Frequencies">>, [<<"2600">>]}
        ,{<<"Sniff-Direction">>, <<"read">>}
        ,{<<"Timeout">>, <<"0">>}
    ],
    {ok, JSON} = whistle_api:tone_detect_req(lists:append([Def, Req])),
    amqp_util:callctl_publish(AHost, CQ, JSON).

generate_tone(AHost, CQ, Call_ID, Server_ID) ->
    Def = monitor_api:default_headers(Server_ID, <<"call_control">>, <<"command">>),
    Req = [
         {<<"Call-ID">>, Call_ID}
        ,{<<"Application-Name">>, <<"tone">>}
        ,{<<"Tones">>, [
            {struct, [
                 {<<"Frequencies">>, [<<"2600">>]}
                ,{<<"Duration-ON">>, <<"2000">>}
                ,{<<"Duration-OFF">>, <<"500">>}
            ]}
        ]}
    ],
    {ok, JSON} = whistle_api:tones_req(lists:append([Def, Req])),
    amqp_util:callctl_publish(AHost, CQ, JSON).

wait_for_msg_type(Category, Name, Timeout) ->
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
            {struct, Msg} = mochijson2:decode(binary_to_list(Payload)),
            case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg) } of
                { Category, Name } ->
                    {ok, Msg};
                _ ->
                    wait_for_msg_type(Category, Name, Timeout)
            end;
        _ ->
            wait_for_msg_type(Category, Name, Timeout)
    after
        Timeout ->
            {error, timeout}
    end.

wait_for_call_event_complete(Name, Timeout) ->
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
            {struct, Msg} = mochijson2:decode(binary_to_list(Payload)),
            case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg), get_value(<<"Application-Name">>, Msg) } of
                { <<"Call-Event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, Name } ->                
                    {ok, Msg};
                _ ->
                    wait_for_call_event_complete(Name, Timeout)
            end;
        _ ->
            wait_for_call_event_complete(Name, Timeout)
    after
        Timeout ->
            {error, timeout}
    end.
