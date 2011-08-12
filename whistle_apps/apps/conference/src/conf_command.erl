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

-export([participants/1, play/2, play/3]).
-export([mute/2, unmute/2, deaf/2, undeaf/2]).
-export([send_command/2]).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level wh_api request to retrieve a list of
%% conference memebers
%% @end
%%--------------------------------------------------------------------
-spec participants/1 :: (Conference) -> ok when
      Conference :: #conf{}.
participants(#conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    Command = [
                {<<"Application-Name">>, <<"participants">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               | wh_api:default_headers(Q, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:conference_participants_req(Command),
    send_command(Payload, Conf).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level wh_api request to play audio to all
%% participants of a conference
%% @end
%%--------------------------------------------------------------------
-spec play/2 :: (Media, Conference) -> ok when
      Media :: binary(),
      Conference :: #conf{}.
-spec play/3 :: (Media, ParticipantId, Conference) -> ok when
      Media :: binary(),
      ParticipantId :: binary(),
      Conference :: #conf{}.

play(Media, #conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    Command = [
                {<<"Application-Name">>, <<"play">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Media-Name">>, Media}
               | wh_api:default_headers(Q, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:conference_play_req(Command),
    send_command(Payload, Conf).

play(Media, ParticipantId, #conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    Command = [
                {<<"Application-Name">>, <<"play">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Participant-ID">>, ParticipantId}
               ,{<<"Media-Name">>, Media}
               | wh_api:default_headers(Q, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:conference_play_req(Command),
    send_command(Payload, Conf).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level wh_api request to mute a participant of
%% the conference
%% @end
%%--------------------------------------------------------------------
-spec mute/2 :: (ParticipantID, Conference) -> ok when
      ParticipantID :: binary(),
      Conference :: #conf{}.
mute(ParticipantId, #conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    Command = [
                {<<"Application-Name">>, <<"mute">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Participant-ID">>, ParticipantId}
               | wh_api:default_headers(Q, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:conference_mute_req(Command),
    send_command(Payload, Conf).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level wh_api request to unmute a participant of
%% the conference
%% @end
%%--------------------------------------------------------------------
-spec unmute/2 :: (ParticipantID, Conference) -> ok when
      ParticipantID :: binary(),
      Conference :: #conf{}.
unmute(ParticipantId, #conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    Command = [
                {<<"Application-Name">>, <<"unmute">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Participant-ID">>, ParticipantId}
               | wh_api:default_headers(Q, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:conference_unmute_req(Command),
    send_command(Payload, Conf).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level wh_api request to deaf a participant of
%% the conference
%% @end
%%--------------------------------------------------------------------
-spec deaf/2 :: (ParticipantID, Conference) -> ok when
      ParticipantID :: binary(),
      Conference :: #conf{}.
deaf(ParticipantId, #conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    Command = [
                {<<"Application-Name">>, <<"deaf">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Participant-ID">>, ParticipantId}
               | wh_api:default_headers(Q, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:conference_deaf_req(Command),
    send_command(Payload, Conf).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Produces the low level wh_api request to undeaf a participant of
%% the conference
%% @end
%%--------------------------------------------------------------------
-spec undeaf/2 :: (ParticipantID, Conference) -> ok when
      ParticipantID :: binary(),
      Conference :: #conf{}.
undeaf(ParticipantId, #conf{conf_id=ConfId, amqp_q=Q}=Conf) ->
    Command = [
                {<<"Application-Name">>, <<"undeaf">>}
               ,{<<"Insert-At">>, <<"now">>}
               ,{<<"Conference-ID">>, ConfId}
               ,{<<"Participant-ID">>, ParticipantId}
               | wh_api:default_headers(Q, <<"conference">>, <<"command">>, ?APP_NAME, ?APP_VERSION)
              ],
    {ok, Payload} = wh_api:conference_undeaf_req(Command),
    send_command(Payload, Conf).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sends call commands to the appropriate call control process
%% @end
%%--------------------------------------------------------------------
-spec send_command/2 :: (Payload, Conference) -> ok when
      Payload :: binary(),
      Conference :: #conf{}.
send_command(_, #conf{ctrl_q=[]}) ->
    ok;
send_command(Payload, #conf{ctrl_q=CtrlQs}) ->
    %% TODO:
    %% The use of mandatory flag is to cause a return notice if it failes. Temporary
    %% because our command q is just a participant of the conference, and not reliable.
    %% There for if we use a control q that has been torn down we need to re-try the request
    %% on another participant's channel
    amqp_util:callctl_publish(hd(CtrlQs), Payload, <<"application/json">>, [{mandatory, true}]).
%%    amqp_util:callctl_publish(CtrlQ, Payload, [{immediate, true}]).
