%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 22 Feb 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_devices).

-include("../callflow.hrl").

-export([handle/2]).

-define(APP_NAME, <<"cf_devices">>).
-define(APP_VERSION, <<"0.5">>).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

-define(DEFAULT_TIMEOUT, 30).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or 
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> stop | continue).
handle({struct, Props}, #cf_call{cf_pid=CFPid}=Call) ->
    init_amqp(Call),
    {ok, Endpoint} = get_endpoint(get_value(<<"database">>, Props), get_value(<<"endpoint">>, Props)),
    bridge_endpoints([Endpoint], get_value(<<"timeout">>, Props, ?DEFAULT_TIMEOUT), Call),
    WaitTimeout = (whistle_util:to_integer(get_value(<<"timeout">>, Props, ?DEFAULT_TIMEOUT)) + 5) * 1000,
    case wait_for_bridge(WaitTimeout) of
        {_, channel_hungup} ->
            format_log(info, "CF_DEVICES(~p): Channel hungup, terminate call flow~n", [self()]),
            CFPid ! { stop };
        {error, _} ->
            format_log(info, "CF_DEVICES(~p): Bridge failed, continue call flow~n", [self()]),
            CFPid ! { continue }
    end.   

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the bridge call command
%% @end
%%--------------------------------------------------------------------
-spec(bridge_endpoints/3 :: (Props :: proplist(), Timeout :: binary(), Call :: #cf_call{}) -> ok).
bridge_endpoints(Endpoints, Timeout, #cf_call{call_id=CallId} = Call) ->
    Command = [
                {<<"Application-Name">>, <<"bridge">>}
               ,{<<"Endpoints">>, Endpoints}
               ,{<<"Continue-On-Fail">>, <<"true">>}
               ,{<<"Timeout">>, Timeout}
               ,{<<"Call-ID">>, CallId}
             | whistle_api:default_headers(CallId, <<"call_control">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
            ],
    format_log(info, "CF_DEVICES(~p): Sending command~nPayload: ~p~n", [self(), Command]),
    {ok, Json} = whistle_api:bridge_req(Command),
    send_callctrl(Json, Call).

get_endpoint(Db, Endpoint) ->
    case couch_mgr:open_doc(Db, Endpoint) of
        {ok, Doc} ->
            Props = [
                      {<<"Invite-Format">>, whapps_json:get_value(["sip", "invite-format"], Doc)}
                     ,{<<"To-User">>, whapps_json:get_value(["sip", "username"], Doc)}
                     ,{<<"To-Realm">>, whapps_json:get_value(["sip", "realm"], Doc)}
                     ,{<<"To-DID">>, whapps_json:get_value(["sip", "number"], Doc)}
                     ,{<<"Route">>, whapps_json:get_value(["sip", "url"], Doc)}
                     ,{<<"Progress-Timeout">>, whapps_json:get_value(["sip", "progress-timeout"], Doc, <<"6">>)}
                     ,{<<"Bypass-Media">>, whapps_json:get_value(["media", "bypass-media"], Doc)}
                     ,{<<"Ignore-Early-Media">>, whapps_json:get_value(["media", "ignore-early-media"], Doc)}
                     ,{<<"Codecs">>, whapps_json:get_value(["media", "codecs"], Doc)}
                    ],
            {ok, {struct, lists:filter(fun({_, undefined}) -> false; (_) -> true end, Props)}};
        Error ->
            format_log(error, "CF_DEVICES(~p): Could not locate endpoint ~p in ~p (~p)~n", [self(), Endpoint, Db, Error]),
            {error, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Waits for and determines the status of the bridge command
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_bridge/1 :: (Timeout :: integer()) -> {ok|error, channel_hangup}|{error, bridge_failed|timeout}).    
wait_for_bridge(Timeout) -> 
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->            
            {struct, Msg} = mochijson2:decode(binary_to_list(Payload)),
             case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg), get_value(<<"Application-Name">>, Msg) } of
                 { <<"call_event">>, <<"CHANNEL_BRIDGE">>, _ } ->
                     wait_for_hangup();
                 { <<"call_event">>, <<"CHANNEL_HANGUP">>, _ } ->
                     {error, channel_hungup};
                 { <<"call_event">>, <<"CHANNEL_EXECUTE_COMPLETE">>, <<"bridge">> } ->
                     {error, bridge_failed};
                 _ ->
                     wait_for_bridge(Timeout)
            end
    after
        Timeout ->
            {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% When the bridge command is successfull this waits for the call to
%% hangup
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_hangup/0 :: () -> {ok, channel_hangup}).    
wait_for_hangup() -> 
    receive
        {_, #amqp_msg{props = Props, payload = Payload}} when Props#'P_basic'.content_type == <<"application/json">> ->            
            {struct, Msg} = mochijson2:decode(binary_to_list(Payload)),
             case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg), get_value(<<"Application-Name">>, Msg) } of
                 { <<"call_event">>, <<"CHANNEL_HANGUP">>, _ } ->
                     {ok, channel_hungup};
                 _ ->
                     wait_for_hangup()
            end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes a AMQP queue and consumer to recieve call events
%% @end
%%--------------------------------------------------------------------
-spec(init_amqp/1 :: (Call :: #cf_call{}) -> no_return()).                          
init_amqp(#cf_call{amqp_h=AHost, call_id=CallId}) ->
    AmqpQ = amqp_util:new_queue(AHost),
    amqp_util:bind_q_to_callevt(AHost, AmqpQ, CallId),
    amqp_util:basic_consume(AHost, AmqpQ).    

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Sends call commands to the appropriate call control process
%% @end
%%--------------------------------------------------------------------
-spec(send_callctrl/2 :: (JSON :: iolist(), Call :: #cf_call{}) -> no_return()).
send_callctrl(Json, #cf_call{amqp_h=AHost, ctrl_q=CtrlQ}) ->
    amqp_util:callctl_publish(AHost, CtrlQ, Json, <<"application/json">>).
