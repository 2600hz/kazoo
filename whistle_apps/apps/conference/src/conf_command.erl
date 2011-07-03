%%%-------------------------------------------------------------------
%%% @author Karl Anderson <karl@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%%
%%% @end
%%% Created : 19 Mar 2011 by Karl Anderson <karl@2600hz.org>
%%%-------------------------------------------------------------------
-module(conf_command).

-include("conference.hrl").

-export([members/1, play/2, play/3]).
-export([mute/2, unmute/2, deaf/2, undeaf/2]).
-export([send_command/2]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to retrieve a list of
%% conference memebers
%% @end
%%--------------------------------------------------------------------
-spec(members/1 :: (Conference :: #conf{}) -> ok).

members(#conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    Command = [
                {<<"Application-Name">>, <<"members">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               | whistle_api:default_headers(Q, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = whistle_api:conference_members_req(Command),
    send_command(Payload, Conf).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to play audio to all
%% participants of a conference
%% @end
%%--------------------------------------------------------------------
-spec(play/2 :: (Media :: binary(), Conference :: #conf{}) -> ok).
-spec(play/3 :: (Media :: binary(), MemberId :: binary(), Conference :: #conf{}) -> ok).

play(Media, #conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    Command = [
                {<<"Application-Name">>, <<"play">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Media-Name">>, Media}
               | whistle_api:default_headers(Q, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = whistle_api:conference_play_req(Command),
    send_command(Payload, Conf).

play(Media, MemberId, #conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    Command = [
                {<<"Application-Name">>, <<"play">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Member-ID">>, MemberId}
               ,{<<"Media-Name">>, Media}
               | whistle_api:default_headers(Q, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = whistle_api:conference_play_req(Command),
    send_command(Payload, Conf).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to mute a member of
%% the conference
%% @end
%%--------------------------------------------------------------------
-spec(mute/2 :: (MemberID :: binary(), Conference :: #conf{}) -> ok).

mute(MemberId, #conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    Command = [
                {<<"Application-Name">>, <<"mute">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Member-ID">>, MemberId}
               | whistle_api:default_headers(Q, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = whistle_api:conference_mute_req(Command),
    send_command(Payload, Conf).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to unmute a member of
%% the conference
%% @end
%%--------------------------------------------------------------------
-spec(unmute/2 :: (MemberID :: binary(), Conference :: #conf{}) -> ok).

unmute(MemberId, #conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    Command = [
                {<<"Application-Name">>, <<"unmute">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Member-ID">>, MemberId}
               | whistle_api:default_headers(Q, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = whistle_api:conference_unmute_req(Command),
    send_command(Payload, Conf).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to deaf a member of
%% the conference
%% @end
%%--------------------------------------------------------------------
-spec(deaf/2 :: (MemberID :: binary(), Conference :: #conf{}) -> ok).

deaf(MemberId, #conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    Command = [
                {<<"Application-Name">>, <<"deaf">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Member-ID">>, MemberId}
               | whistle_api:default_headers(Q, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = whistle_api:conference_deaf_req(Command),
    send_command(Payload, Conf).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level whistle_api request to undeaf a member of
%% the conference
%% @end
%%--------------------------------------------------------------------
-spec(undeaf/2 :: (MemberID :: binary(), Conference :: #conf{}) -> ok).

undeaf(MemberId, #conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    Command = [
                {<<"Application-Name">>, <<"undeaf">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Member-ID">>, MemberId}
               | whistle_api:default_headers(Q, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = whistle_api:conference_undeaf_req(Command),
    send_command(Payload, Conf).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sends call commands to the appropriate call control process
%% @end
%%--------------------------------------------------------------------
-spec(send_command/2 :: (Payload :: binary(), Conference :: #conf{}) -> ok).
send_command(Payload, #conf{ctrl_q=CtrlQ}) ->
    amqp_util:callctl_publish(CtrlQ, Payload).
