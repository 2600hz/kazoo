%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 19 Mar 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(cf_conference_command).

-include("callflow.hrl").

-export([members/2, play/3, play/4]).
-export([mute/3, unmute/3, deaf/3, undeaf/3]).

-export([b_members/2]).

-import(cf_call_command, [wait_for_message/1, wait_for_message/2, wait_for_message/3, wait_for_message/4]).
-import(cf_call_command, [wait_for_hangup/0]).
-import(cf_call_command, [send_callctrl/2]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to retrieve a list of
%% conference memebers
%% @end
%%--------------------------------------------------------------------
-spec(members/2 :: (Conference :: binary(), Call :: #cf_call{}) -> ok).
-spec(b_members/2 :: (Conference :: binary(), Call :: #cf_call{}) -> list()).

members(ConfId, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [
                {<<"Application-Name">>, <<"members">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_members_req(Command),
    send_callctrl(Json, Call).

b_members(ConfId, Call) ->
    members(ConfId, Call),
    case wait_for_message(<<"members">>, <<"response">>, <<"conference">>) of
        {ok, Response} ->
            wh_json:get_value(<<"Members">>, Response, []);
        {error, _} ->
            []
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to play audio to all
%% participants of a conference
%% @end
%%--------------------------------------------------------------------
-spec(play/3 :: (Media :: binary(), Conference :: binary(), Call :: #cf_call{}) -> ok).
-spec(play/4 :: (Media :: binary(), MemberId :: binary(), Conference :: binary(), Call :: #cf_call{}) -> ok).

play(Media, ConfId, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [
                {<<"Application-Name">>, <<"play">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Media-Name">>, Media}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_play_req(Command),
    send_callctrl(Json, Call).

play(Media, MemberId, ConfId, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [
                {<<"Application-Name">>, <<"play">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Member-ID">>, MemberId}
               ,{<<"Media-Name">>, Media}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_play_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to mute a member of
%% the conference
%% @end
%%--------------------------------------------------------------------
-spec(mute/3 :: (MemberID :: binary(), Conference :: binary(), Call :: #cf_call{}) -> ok).

mute(MemberId, ConfId, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [
                {<<"Application-Name">>, <<"mute">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Member-ID">>, MemberId}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_mute_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to unmute a member of
%% the conference
%% @end
%%--------------------------------------------------------------------
-spec(unmute/3 :: (MemberID :: binary(), Conference :: binary(), Call :: #cf_call{}) -> ok).

unmute(MemberId, ConfId, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [
                {<<"Application-Name">>, <<"unmute">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Member-ID">>, MemberId}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_unmute_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to deaf a member of
%% the conference
%% @end
%%--------------------------------------------------------------------
-spec(deaf/3 :: (MemberID :: binary(), Conference :: binary(), Call :: #cf_call{}) -> ok).

deaf(MemberId, ConfId, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [
                {<<"Application-Name">>, <<"deaf">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Member-ID">>, MemberId}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_deaf_req(Command),
    send_callctrl(Json, Call).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to undeaf a member of
%% the conference
%% @end
%%--------------------------------------------------------------------
-spec(undeaf/3 :: (MemberID :: binary(), Conference :: binary(), Call :: #cf_call{}) -> ok).

undeaf(MemberId, ConfId, #cf_call{call_id=CallId, amqp_q=AmqpQ} = Call) ->
    Command = [
                {<<"Application-Name">>, <<"undeaf">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Member-ID">>, MemberId}
               ,{<<"Call-ID">>, CallId}
               | whistle_api:default_headers(AmqpQ, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Json} = whistle_api:conference_undeaf_req(Command),
    send_callctrl(Json, Call).
