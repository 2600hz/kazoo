%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 7 April 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_offnet).

-include("../callflow.hrl").

-export([handle/2]).

-import(cf_call_command, [wait_for_unbridge/0]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> no_return()).
handle(JObj, #cf_call{call_id=CallId, request_user=ReqNum, account_id=AccountId
                      ,ctrl_q=CtrlQ, amqp_q=AmqpQ, cf_pid=CFPid}=Call) ->
    put(callid, CallId),
    Command = [
                {<<"Call-ID">>, CallId}
               ,{<<"Resource-Type">>, <<"audio">>}
               ,{<<"To-DID">>, ReqNum}
               ,{<<"Account-ID">>, AccountId}
               ,{<<"Control-Queue">>, CtrlQ}
               ,{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Flags">>, wh_json:get_value(<<"flags">>, JObj)}
               ,{<<"Timeout">>, wh_json:get_value(<<"timeout">>, JObj)}
               ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"ignore_early_media">>, JObj)}
%%               ,{<<"Outgoing-Caller-ID-Name">>, CallerIDName}
%%               ,{<<"Outgoing-Caller-ID-Number">>, CallerIDNum}
               ,{<<"Ringback">>, wh_json:get_value(<<"ringback">>, JObj)}
               | whistle_api:default_headers(AmqpQ, <<"resource">>, <<"offnet_req">>, ?APP_NAME, ?APP_VERSION)
            ],
    {ok, Payload} = whistle_api:offnet_resource_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    amqp_util:offnet_resource_publish(Payload),
    case wait_for_offnet_response(60000) of
        {ok, _R} ->
            io:format("Resource call id~n~p~n~n", [wh_json:get_value(<<"Call-ID">>, _R)]),
            io:format("OFFNET WAIT1: ~p~n~n", [wait_for_unbridge()]),
            io:format("OFFNET WAIT2: ~p~n~n", [wait_for_unbridge()]);
        {fail, Msg, Code} ->
            ?LOG("received offnet failure ~s:~s", [Code, Msg]),
            find_failure_branch([Msg, Code], Call);
        {error, Reason} ->
            ?LOG("offnet resource request error ~w", [Reason]),
            find_failure_branch([whistle_util:to_binary(Reason)], Call)
    end.

find_failure_branch([], #cf_call{cf_pid=CFPid}) ->
    CFPid ! {continue};
find_failure_branch([<<"sip:", Error/binary>>|T], #cf_call{cf_pid=CFPid}=Call) ->
    CFPid ! {attempt, Error},
    receive
        {attempt_resp, ok} ->
            ?LOG("found child branch to handle failure code ~s", [Error]);
        {attempt_resp, _} ->
            find_failure_branch(T, Call)
    after
        1000 ->
            find_failure_branch(T, Call)
    end;
find_failure_branch([Error|T], #cf_call{cf_pid=CFPid}=Call) ->
    CFPid ! {attempt, Error},
    receive
        {attempt_resp, ok} ->
            ?LOG("found child branch to handle failure ~s", [Error]);
        {attempt_resp, _} ->
            find_failure_branch(T, Call)
    after
        1000 ->
            find_failure_branch(T, Call)
    end.

-spec(wait_for_offnet_response/1 :: (Timeout :: integer()) -> tuple(ok, json_object()) | tuple(error, atom())).
wait_for_offnet_response(Timeout) ->
    Start = erlang:now(),
    receive
        {amqp_msg, {struct, _}=JObj} ->
            case { wh_json:get_value(<<"Event-Name">>, JObj), wh_json:get_value(<<"Event-Category">>, JObj) } of
                { <<"offnet_resp">>, <<"resource">> } ->
                    {ok, JObj};
                { <<"resource_error">>, <<"resource">> } ->
                    {fail, wh_json:get_value(<<"Failure-Message">>, JObj)
                     ,wh_json:get_value(<<"Failure-Code">>, JObj)};
                { <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
                    {error, channel_hungup};
                { _, <<"error">> } ->
                    {error, execution_failed};
                _ ->
		    DiffMicro = timer:now_diff(erlang:now(), Start),
                    wait_for_offnet_response(Timeout - (DiffMicro div 1000))
            end;
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            DiffMicro = timer:now_diff(erlang:now(), Start),
            wait_for_offnet_response(Timeout - (DiffMicro div 1000))
    after
        Timeout ->
            {error, timeout}
    end.
