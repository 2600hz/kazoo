%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_devices).

-include("callflow.hrl").

-export([handle/2]).

-define(APP_NAME, <<"cf_devices">>).
-define(APP_VERSION, <<"0.5">>).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

%% -spec(handle/2 :: (Data :: json_object(), Call :: #cf_call) -> ok).
handle({struct, Props}, #cf_call{amqp_h=AHost, ctrl_q=CtrlQ, call_id=CallId, cf_pid=CFPid}=Call) ->
    format_log(info, "CF_DEVICES(~p): Spawned to bridge to ~p with ctrl_q ~p and call_id ~p~n", [self(), Props, CtrlQ, CallId]),

    AmqpQ = amqp_util:new_queue(AHost),
    amqp_util:bind_q_to_callevt(AHost, AmqpQ, CallId),
    amqp_util:basic_consume(AHost, AmqpQ),
    
%%    set_channel_vars(Call),

    bridge_endpoint(Call),

    wait_for_call_event(<<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">>, 300000),

    CFPid ! stop,
    ok.    

wait() ->
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->
            {struct, Msg} = mochijson2:decode(binary_to_list(Payload)),
            format_log(info, "CF_DEVICES(~p): Received~nPayload: ~p~n", [self(), Msg]),
            wait();
        Payload ->
            format_log(info, "CF_DEVICES(~p): Received unknown~nPayload: ~p~n", [self(), Payload]),
            wait()
    after
        30000 -> ok
    end.    

answer_call(#cf_call{call_id=CallId} = Call) ->                               
    Command = [
                {<<"Application-Name">>, <<"answer">>}
               ,{<<"Call-ID">>, CallId}
             | whistle_api:default_headers(CallId, <<"call_control">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    format_log(info, "CF_DEVICES(~p): Sending command~nPayload: ~p~n", [self(), Command]),
    {ok, Json} = whistle_api:answer_req(Command),
    send_resp(Json, Call).


set_channel_vars(#cf_call{call_id=CallId} = Call) ->                               
    Command = [
                {<<"Application-Name">>, <<"set">>}               
               ,{<<"Custom-Channel-Vars">>, {struct, [{<<"hangup_after_bridge">>, <<"true">>},{<<"continue_on_fail">>, <<"true">>}]}}
               ,{<<"Call-ID">>, CallId}
             | whistle_api:default_headers(CallId, <<"call_control">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    format_log(info, "CF_DEVICES(~p): Sending command~nPayload: ~p~n", [self(), Command]),
    {ok, Json} = whistle_api:set_req(Command),
    send_resp(Json, Call).

bridge_endpoint(#cf_call{call_id=CallId} = Call) ->                               
    Command = [
                {<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, [{struct, [
                                              {<<"Invite-Format">>, <<"username">>}
                                             ,{<<"To-User">>, <<"karl">>}
                                             ,{<<"To-Realm">>, <<"172.16.1.185">>}                                             
                                            ]}]}
               ,{<<"Progress-Timeout">>, <<"6">>}
               ,{<<"Timeout">>, <<"30">>}
               ,{<<"Call-ID">>, CallId}
             | whistle_api:default_headers(CallId, <<"call_control">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    format_log(info, "CF_DEVICES(~p): Sending command~nPayload: ~p~n", [self(), Command]),
    {ok, Json} = whistle_api:bridge_req(Command),
    send_resp(Json, Call).
    
%%-spec(send_resp/3 :: (JSON :: iolist(), RespQ :: binary(), tuple()) -> no_return()).
send_resp(Json, #cf_call{amqp_h=AHost, ctrl_q=CtrlQ}) ->
    format_log(info, "CF_DEVICES(~p): Sent to ~p~n", [self(), CtrlQ]),
    amqp_util:callctl_publish(AHost, CtrlQ, Json, <<"application/json">>).


wait_for_call_event(Name, Application, Timeout) ->    
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->            
            {struct, Msg} = mochijson2:decode(binary_to_list(Payload)),
            format_log(info, "CF_DEVICES(~p): Received~nPayload: ~p~n", [self(), Msg]),
            case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg), get_value(<<"Application-Name">>, Msg) } of
                { <<"Call-Event">>, Name, Application } ->                                
                    {ok, Msg};
                { <<"Call-Event">>, <<"CHANNEL_HANGUP">>, _Name } ->
                    {error, channel_hungup};
                _ ->
                    wait_for_call_event(Name, Application, Timeout)
            end
    after
        Timeout ->
            {error, timeout}
    end.
