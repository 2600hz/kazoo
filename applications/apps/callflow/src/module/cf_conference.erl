%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, Karl Anderson
%%% @doc
%%%
%%% @end
%%% Created : 16 Mar 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_conference).

-include("../callflow.hrl").

-export([handle/2]).

-define(APP_NAME, <<"cf_conference">>).
-define(APP_VERSION, <<"0.5">>).

-import(props, [get_value/2, get_value/3]).
-import(logger, [format_log/3]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Data payload.  Returns continue if fails to connect or 
%% stop when successfull.
%% @end
%%--------------------------------------------------------------------
-spec(handle/2 :: (Data :: json_object(), Call :: #cf_call{}) -> stop | continue).
handle(Data, #cf_call{cf_pid=CFPid}=Call) ->    
    answer(Call),
    conference(Data, Call),
    wait_for_conference(5000),
    MemberId = whapps_json:get_value([1, "Member-ID"], members(Call)),
    mess_with_member(MemberId, Call),
    stop.

mess_with_member(MemberId, Call) ->
    deaf_member(MemberId, Call),
    timer:sleep(1500),
    undeaf_member(MemberId, Call),
    timer:sleep(1500),
    mess_with_member(MemberId, Call).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to answer a call
%% @end
%%--------------------------------------------------------------------
answer(#cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
        Command = [
                    {<<"Application-Name">>, <<"answer">>}
                   ,{<<"Call-ID">>, CallId}
                   | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
                  ],
    {ok, Json} = whistle_api:answer_req(Command),
    send_callctrl(Json, Call).       


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to connect to a conference
%% @end
%%--------------------------------------------------------------------
-spec(conference/2 :: (Data :: json_object(), Call :: #cf_call{}) -> ok).
conference({struct, Props}, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->    
    Command = [
                {<<"Application-Name">>, <<"conference">>}
               ,{<<"Conference-ID">>, <<"test">>}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"call">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to mute a memeber
%% @end
%%--------------------------------------------------------------------
-spec(members/1 :: (Call :: #cf_call{}) -> ok).
members(#cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->        
    Command = [
                {<<"Application-Name">>, <<"members">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, <<"test">>}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    format_log(info, "Members: ~p", [Command]),
    {ok, Json} = whistle_api:conference_members_req(Command),
    send_callctrl(Json, Call),
    wait_for_members_response(5000).
        
%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to mute a memeber
%% @end
%%--------------------------------------------------------------------
-spec(mute_member/2 :: (MemberID :: binary(), Call :: #cf_call{}) -> ok).
mute_member(MemberId, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->    
    Command = [
                {<<"Application-Name">>, <<"mute">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, <<"test">>}
               ,{<<"Member-ID">>, MemberId}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_mute_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to unmute a memeber
%% @end
%%--------------------------------------------------------------------
-spec(unmute_member/2 :: (MemberID :: binary(), Call :: #cf_call{}) -> ok).
unmute_member(MemberId, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->    
    Command = [
                {<<"Application-Name">>, <<"unmute">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, <<"test">>}
               ,{<<"Member-ID">>, MemberId}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_unmute_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to deaf a memeber
%% @end
%%--------------------------------------------------------------------
-spec(deaf_member/2 :: (MemberID :: binary(), Call :: #cf_call{}) -> ok).
deaf_member(MemberId, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->    
    Command = [
                {<<"Application-Name">>, <<"deaf">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, <<"test">>}
               ,{<<"Member-ID">>, MemberId}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_deaf_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Generates and sends the dialplan API to undeaf a memeber
%% @end
%%--------------------------------------------------------------------
-spec(undeaf_member/2 :: (MemberID :: binary(), Call :: #cf_call{}) -> ok).
undeaf_member(MemberId, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->    
    Command = [
                {<<"Application-Name">>, <<"undeaf">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, <<"test">>}
               ,{<<"Member-ID">>, MemberId}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_undeaf_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Waits for and determines the status of the bridge command
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_members_response/1 :: (Timeout :: integer()) -> tuple(ok, json_object())|tuple(error, channel_hangup)).    
wait_for_members_response(Timeout) -> 
    receive
        {amqp_msg, {struct, Msg}} ->
            case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg), get_value(<<"Application-Name">>, Msg) } of
                { <<"conference">>, <<"response">>, <<"members">>} ->
%                    format_log(info, "!!!!!!!! ~p", [whapps_json:get_value([1], get_value(<<"Members">>, Msg))]),
                    get_value(<<"Members">>, Msg);
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _ } ->
                    {error, channel_hungup};
                _ ->
                    wait_for_members_response(Timeout)
            end
    after
        Timeout ->
            {error, timeout}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Waits for the conference to be built
%% hangup
%% @end
%%--------------------------------------------------------------------
-spec(wait_for_conference/1 :: (Timeout :: integer()) -> tuple(ok, conference_started)|tuple(error, atom())).    
wait_for_conference(Timeout) -> 
    receive
        {amqp_msg, {struct, Msg}} ->
            case { get_value(<<"Event-Category">>, Msg), get_value(<<"Event-Name">>, Msg), get_value(<<"Application-Name">>, Msg) } of
                { <<"call_event">>, <<"CHANNEL_EXECUTE">>, <<"conference">> } ->
                    {ok, conference_started};
                { <<"call_event">>, <<"CHANNEL_HANGUP">>, _ } ->
                    {error, channel_hungup};
                _ ->
                    wait_for_conference(Timeout)
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
-spec(wait_for_hangup/0 :: () -> tuple(ok, channel_hangup)).    
wait_for_hangup() -> 
    receive
        {amqp_msg, {struct, Msg}} ->
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
%% Sends call commands to the appropriate call control process
%% @end
%%--------------------------------------------------------------------
-spec(send_callctrl/2 :: (JSON :: json_object(), Call :: #cf_call{}) -> no_return()).
send_callctrl(Json, #cf_call{amqp_h=AHost, ctrl_q=CtrlQ}) ->
    amqp_util:callctl_publish(AHost, CtrlQ, Json, <<"application/json">>).
