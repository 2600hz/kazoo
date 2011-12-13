%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Notification messages, like voicemail left
%%% @end
%%% Created :  6 Dec 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_notifications).

-export([voicemail/1, voicemail_v/1, mwi_update/1, mwi_update_v/1]).

-export([bind_q/2, unbind_q/1, unbind_q/2]).

-export([publish_voicemail/1, publish_voicemail/2, publish_mwi_update/1, publish_mwi_update/2]).

-include("../wh_api.hrl").

-define(KEY_SIP_NOTIFY, <<"sip.notify">>).

-define(NOTIFY_VOICEMAIL_NEW, <<"notification.voicemail.new">>).

-define(VOICEMAIL_HEADERS, [<<"From-User">>, <<"From-Realm">>, <<"To-User">>, <<"To-Realm">>
				    ,<<"Account-DB">>, <<"Voicemail-Box">>, <<"Voicemail-Name">>, <<"Voicemail-Timestamp">>]).
-define(OPTIONAL_VOICEMAIL_HEADERS, [<<"Caller-ID-Name">>, <<"Caller-ID-Number">>, <<"Call-ID">>]).
-define(VOICEMAIL_VALUES, [{<<"Event-Category">>, <<"notification">>}
			       ,{<<"Event-Name">>, <<"new_voicemail">>}
			      ]).
-define(VOICEMAIL_TYPES, []).

%% Notify MWI request
-define(MWI_REQ_HEADERS, [<<"Notify-User">>, <<"Notify-Realm">>, <<"Messages-New">>, <<"Messages-Saved">>]).
-define(OPTIONAL_MWI_REQ_HEADERS, [<<"Messages-Urgent">>, <<"Messages-Urgent-Saved">>]).
-define(MWI_REQ_VALUES, [{<<"Event-Category">>, <<"notification">>}
			 ,{<<"Event-Name">>, <<"mwi">>}
                        ]).
-define(MWI_REQ_TYPES, [{<<"Messages-New">>, fun(I) -> is_integer(wh_util:to_integer(I)) end}
			,{<<"Messages-Saved">>, fun(I) -> is_integer(wh_util:to_integer(I)) end}
			,{<<"Messages-Urgent">>, fun(I) -> is_integer(wh_util:to_integer(I)) end}
			,{<<"Messages-Urgent-Saved">>, fun(I) -> is_integer(wh_util:to_integer(I)) end}
		       ]).


%%--------------------------------------------------------------------
%% @doc MWI - Update the Message Waiting Indicator on a device - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
voicemail(Prop) when is_list(Prop) ->
    case voicemail_v(Prop) of
 	true -> wh_api:build_message(Prop, ?VOICEMAIL_HEADERS, ?OPTIONAL_VOICEMAIL_HEADERS);
	false -> {error, "Proplist failed validation for voicemail"}
    end;
voicemail(JObj) ->
    voicemail(wh_json:to_proplist(JObj)).

-spec voicemail_v/1 :: (api_terms()) -> boolean().
voicemail_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?VOICEMAIL_HEADERS, ?VOICEMAIL_VALUES, ?VOICEMAIL_TYPES);
voicemail_v(JObj) ->
    voicemail_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc MWI - Update the Message Waiting Indicator on a device - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec mwi_update/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
mwi_update(Prop) when is_list(Prop) ->
    case mwi_update_v(Prop) of
	true -> wh_api:build_message(Prop, ?MWI_REQ_HEADERS, ?OPTIONAL_MWI_REQ_HEADERS);
	false -> {error, "Proplist failed validation for mwi_req"}
    end;
mwi_update(JObj) ->
    mwi_update(wh_json:to_proplist(JObj)).

-spec mwi_update_v/1 :: (api_terms()) -> boolean().
mwi_update_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MWI_REQ_HEADERS, ?MWI_REQ_VALUES, ?MWI_REQ_TYPES);
mwi_update_v(JObj) ->
    mwi_update_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
bind_q(Q, Props) ->
    bind_q_(Q, props:get_value(notices, Props, [])).

bind_q_(Q, [new_voicemail | T]) ->
    amqp_util:callevt_exchange(),
    amqp_util:bind_q_to_callevt(Q, ?NOTIFY_VOICEMAIL_NEW, other),
    bind_q_(Q, T);
bind_q_(Q, [sip_notify | T]) ->
    amqp_util:callmgr_exchange(),
    amqp_util:bind_q_to_callmgr(Q, ?KEY_SIP_NOTIFY),
    bind_q_(Q, T);
bind_q_(Q, [_|T]) ->
    bind_q_(Q, T);
bind_q_(_, []) ->
    ok.

-spec unbind_q/1 :: (ne_binary()) -> 'ok'.
-spec unbind_q/2 :: (ne_binary(), proplist()) -> 'ok'.
unbind_q(Q) ->
    unbind_q(Q, []).
unbind_q(Q, _Props) ->
    amqp_util:unbind_q_from_callevt(Q, ?NOTIFY_VOICEMAIL_NEW),
    amqp_util:unbind_q_from_callmgr(Q, ?KEY_SIP_NOTIFY).

-spec publish_voicemail/1 :: (api_terms()) -> 'ok'.
-spec publish_voicemail/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_voicemail(JObj) ->
    publish_voicemail(JObj, ?DEFAULT_CONTENT_TYPE).
publish_voicemail(Voicemail, ContentType) ->
    CallID = wh_json:get_value(<<"Call-ID">>, Voicemail),

    {ok, Payload} = wh_api:prepare_api_payload(Voicemail, ?VOICEMAIL_VALUES, fun ?MODULE:voicemail/1),
    amqp_util:callevt_publish(CallID, Payload, ?NOTIFY_VOICEMAIL_NEW, ContentType).

-spec publish_mwi_update/1 :: (api_terms()) -> 'ok'.
-spec publish_mwi_update/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_mwi_update(JObj) ->
    publish_mwi_update(JObj, ?DEFAULT_CONTENT_TYPE).
publish_mwi_update(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?MWI_REQ_VALUES, fun ?MODULE:mwi_update/1),
    amqp_util:callmgr_publish(Payload, ContentType, ?KEY_SIP_NOTIFY).
