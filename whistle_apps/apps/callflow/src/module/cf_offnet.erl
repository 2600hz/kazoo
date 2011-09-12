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

-import(cf_call_command, [wait_for_bridge/2, find_failure_branch/2]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec handle/2 :: (Data, Call) -> no_return() when
      Data :: json_object(),
      Call :: #cf_call{}.
handle(Data, #cf_call{call_id=CallId, request_user=ReqNum, account_id=AccountId, inception_during_transfer=IDT
                      ,ctrl_q=CtrlQ, amqp_q=AmqpQ, cf_pid=CFPid, owner_id=OwnerId, authorizing_id=AuthId, channel_vars=CCV}=Call) ->
    put(callid, CallId),
    {CIDNum, CIDName}
        = cf_attributes:caller_id(wh_json:get_value(<<"caller_id_options">>, Data, <<"external">>), AuthId, OwnerId, Call),
    Command = [{<<"Call-ID">>, CallId}
               ,{<<"Resource-Type">>, <<"audio">>}
               ,{<<"To-DID">>, ReqNum}
               ,{<<"Account-ID">>, AccountId}
               ,{<<"Control-Queue">>, CtrlQ}
               ,{<<"Application-Name">>, <<"bridge">>}
               ,{<<"Flags">>, wh_json:get_value(<<"flags">>, Data)}
               ,{<<"Timeout">>, wh_json:get_value(<<"timeout">>, Data)}
               ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"ignore_early_media">>, Data)}
               ,{<<"Outgoing-Caller-ID-Name">>, CIDName}
               ,{<<"Outgoing-Caller-ID-Number">>, CIDNum}
               ,{<<"Ringback">>, wh_json:get_value(<<"ringback">>, Data)}
               | wh_api:default_headers(AmqpQ, <<"resource">>, <<"offnet_req">>, ?APP_NAME, ?APP_VERSION)
            ],
    {ok, Payload} = wh_api:offnet_resource_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    amqp_util:offnet_resource_publish(Payload),
    case wait_for_offnet(60000, Call) of
        {ok, _} ->
            ?LOG("completed successful offnet bridge"),
            CFPid ! { stop };
        {fail, Reason} when IDT ->
            case wh_json:get_binary_value(<<"Transfer-Fallback">>, CCV) of
                undefined ->
                    {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
                    ?LOG("offnet request during transfer failed ~s:~s", [Cause, Code]),
                    find_failure_branch({Cause, Code}, Call)
                        orelse CFPid ! { continue };
                FallbackId ->
                    Gen = [fun(J) -> wh_json:set_value(<<"module">>, <<"device">>, J) end
                           ,fun(J) -> wh_json:set_value([<<"data">>, <<"id">>], FallbackId, J) end
                           ,fun(J) -> wh_json:set_value([<<"children">>, <<"_">>], ?EMPTY_JSON_OBJECT, J) end],
                    Flow = lists:foldr(fun(Fun, JObj) -> Fun(JObj) end, ?EMPTY_JSON_OBJECT, Gen),
                    CFPid ! {branch, Flow}
            end;
        {fail, Reason} ->
            {Cause, Code} = whapps_util:get_call_termination_reason(Reason),
            ?LOG("offnet request failed ~s:~s", [Cause, Code]),
            find_failure_branch({Cause, Code}, Call)
                orelse CFPid ! { continue };
        {error, Reason} ->
            ?LOG("offnet resource bridge error ~p", [Reason]),
            CFPid ! { continue }
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Consume Erlang messages and return on resource completion message
%% @end
%%--------------------------------------------------------------------
-spec wait_for_offnet/2 :: (Timeout, Call) -> tuple(ok | fail, json_object())
                                                  | tuple(error, channel_hungup | execution_failed | timeout) when
      Timeout :: integer(),
      Call :: #cf_call{}.
wait_for_offnet(Timeout, Call) ->
    Start = erlang:now(),
    receive
        {amqp_msg, {struct, _}=JObj} ->
            case { wh_json:get_value(<<"Event-Name">>, JObj), wh_json:get_value(<<"Event-Category">>, JObj) } of
                { <<"offnet_resp">>, <<"resource">> } ->
                    wait_for_bridge(infinity, Call);
                { <<"resource_error">>, <<"resource">> } ->
                    {fail, JObj};
                { <<"CHANNEL_HANGUP">>, <<"call_event">> } ->
                    {error, channel_hungup};
                { _, <<"error">> } ->
                    {error, execution_failed};
                _ ->
		    DiffMicro = timer:now_diff(erlang:now(), Start),
                    wait_for_offnet(Timeout - (DiffMicro div 1000), Call)
            end;
        _ ->
            %% dont let the mailbox grow unbounded if
            %%   this process hangs around...
            DiffMicro = timer:now_diff(erlang:now(), Start),
            wait_for_offnet(Timeout - (DiffMicro div 1000), Call)
    after
        Timeout ->
            {error, timeout}
    end.
