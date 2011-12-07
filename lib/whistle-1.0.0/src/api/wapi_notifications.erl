%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Notification messages, like voicemail left
%%% @end
%%% Created :  6 Dec 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_notifications).

-export([voicemail/1, voicemail_v/1]).

-export([bind_q/2, unbind_q/1, unbind_q/2]).

-export([publish_voicemail/1]).

-include("../wh_api.hrl").

-define(NOTIFY_VOICEMAIL_NEW, <<"notification.voicemail.new">>).

-define(VOICEMAIL_HEADERS, [<<"From-User">>, <<"From-Realm">>, <<"To-User">>, <<"To-Realm">>
				    ,<<"Account-DB">>, <<"Voicemail-Box">>, <<"Voicemail-Name">>, <<"Voicemail-Timestamp">>]).
-define(OPTIONAL_VOICEMAIL_HEADERS, [<<"Caller-ID-Name">>, <<"Caller-ID-Number">>, <<"Call-ID">>]).
-define(VOICEMAIL_VALUES, [{<<"Event-Category">>, <<"notification">>}
			       ,{<<"Event-Name">>, <<"new_voicemail">>}
			      ]).
-define(VOICEMAIL_TYPES, []).

voicemail(Prop) when is_list(Prop) ->
    case voicemail_v(Prop) of
 	true -> wh_api:build_message(Prop, ?VOICEMAIL_HEADERS, ?OPTIONAL_VOICEMAIL_HEADERS);
	false -> {error, "Proplist failed validation for voicemail"}
    end;
voicemail(JObj) ->
    voicemail(wh_json:to_proplist(JObj)).

bind_q(Q, _Props) ->
    amqp_util:callevt_exchange(),
    amqp_util:bind_q_to_callevt(Q, ?NOTIFY_VOICEMAIL_NEW).

unbind_q(Q) ->
    amqp_util:unbind_q_from_callevt(Q, ?NOTIFY_VOICEMAIL_NEW).

unbind_q(Q, _Props) ->
    amqp_util:unbind_q_from_callevt(Q, ?NOTIFY_VOICEMAIL_NEW).

-spec voicemail_v/1 :: (api_terms()) -> boolean().
voicemail_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?VOICEMAIL_HEADERS, ?VOICEMAIL_VALUES, ?VOICEMAIL_TYPES);
voicemail_v(JObj) ->
    voicemail_v(wh_json:to_proplist(JObj)).

-spec publish_voicemail/1 :: (api_terms()) -> 'ok'.
-spec publish_voicemail/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_voicemail(JObj) ->
    publish_voicemail(JObj, ?DEFAULT_CONTENT_TYPE).
publish_voicemail(Voicemail, ContentType) ->
    CallID = wh_json:get_value(<<"Call-ID">>, Voicemail),

    {ok, Payload} = wh_api:prepare_api_payload(Voicemail, ?VOICEMAIL_VALUES, fun ?MODULE:voicemail/1),
    amqp_util:callevt_publish(CallID, Payload, ?NOTIFY_VOICEMAIL_NEW, ContentType).
