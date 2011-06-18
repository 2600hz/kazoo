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

-import(cf_call_command, [b_bridge/4, wait_for_bridge/1, wait_for_unbridge/0]).

-define(VIEW_BY_RULES, <<"resources/listing_active_by_rules">>).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> no_return()).
handle(JObj, #cf_call{call_id=CallId, request_user=ReqNum, account_id=AccountId
                      ,ctrl_q=CtrlQ, amqp_q=AmqpQ}=Call) ->
    put(callid, CallId),
    Command = [
                {<<"Application-Name">>, <<"bridge">>}
               ,{<<"Control-Queue">>, CtrlQ}
               ,{<<"Account-ID">>, AccountId}
               ,{<<"Number">>, ReqNum}
               ,{<<"Timeout">>, wh_json:get_value(<<"timeout">>, JObj)}
               ,{<<"Ignore-Early-Media">>, wh_json:get_value(<<"ignore_early_media">>, JObj)}
%%               ,{<<"Outgoing-Caller-ID-Name">>, CallerIDName}
%%               ,{<<"Outgoing-Caller-ID-Number">>, CallerIDNum}
               ,{<<"Ringback">>, wh_json:get_value(<<"ringback">>, JObj)}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"offnet">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    {ok, Payload} = whistle_api:offnet_bridge_req([ KV || {_, V}=KV <- Command, V =/= undefined ]),
    amqp_util:offnet_publish(Payload),
    io:format("~p~n", [wait_for_bridge(60000)]),
    io:format("~p~n", [wait_for_unbridge()]).
