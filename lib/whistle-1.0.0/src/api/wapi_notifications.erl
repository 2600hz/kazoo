%%%-------------------------------------------------------------------
%%% @author James Aimonetti <james@2600hz.org>
%%% @copyright (C) 2011, VoIP INC
%%% @doc
%%% Notification messages, like voicemail left
%%% @end
%%% Created :  6 Dec 2011 by James Aimonetti <james@2600hz.org>
%%%-------------------------------------------------------------------
-module(wapi_notifications).

-export([bind_q/2, unbind_q/1, unbind_q/2]).

-export([voicemail/1, voicemail_v/1]).
-export([mwi_update/1, mwi_update_v/1]).
-export([deregister/1, deregister_v/1]).
-export([pwd_recovery/1, pwd_recovery_v/1]).

-export([publish_voicemail/1, publish_voicemail/2]).
-export([publish_mwi_update/1, publish_mwi_update/2]).
-export([publish_deregister/1, publish_deregister/2]).
-export([publish_pwd_recovery/1, publish_pwd_recovery/2]).

-include("../wh_api.hrl").

-define(NOTIFY_MWI_UPDATE, <<"notifications.mwi.update">>).
-define(NOTIFY_VOICEMAIL_NEW, <<"notifications.voicemail.new">>).
-define(NOTIFY_DEREGISTER, <<"notifications.sip.deregister">>).
-define(NOTIFY_PWD_RECOVERY, <<"notifications.password.recovery">>).

%% Notify New Voicemail
-define(VOICEMAIL_HEADERS, [<<"From-User">>, <<"From-Realm">>, <<"To-User">>, <<"To-Realm">>
                                    ,<<"Account-DB">>, <<"Voicemail-Box">>, <<"Voicemail-Name">>, <<"Voicemail-Timestamp">>]).
-define(OPTIONAL_VOICEMAIL_HEADERS, [<<"Caller-ID-Name">>, <<"Caller-ID-Number">>, <<"Call-ID">>]).
-define(VOICEMAIL_VALUES, [{<<"Event-Category">>, <<"notification">>}
                               ,{<<"Event-Name">>, <<"new_voicemail">>}
                              ]).
-define(VOICEMAIL_TYPES, []).

%% Notify updated MWI
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

%% Notify Deregister
-define(DEREGISTER_HEADERS, [<<"Username">>, <<"Realm">>, <<"Account-ID">>]).
-define(OPTIONAL_DEREGISTER_HEADERS, [<<"Status">>, <<"User-Agent">>, <<"Call-ID">>, <<"Profile-Name">>, <<"Presence-Hosts">>
                                          ,<<"From-User">>, <<"From-Host">>, <<"FreeSWITCH-Hostname">>, <<"RPid">>
                                          ,<<"To-User">>, <<"To-Host">>, <<"Network-IP">>, <<"Network-Port">>
                                          ,<<"Event-Timestamp">>, <<"Contact">>, <<"Expires">>, <<"Account-DB">>
                                          ,<<"Authorizing-ID">>, <<"Suppress-Unregister-Notify">>
                                     ]).
-define(DEREGISTER_VALUES, [{<<"Event-Category">>, <<"notification">>}
                            ,{<<"Event-Name">>, <<"deregister">>}
                           ]).
-define(DEREGISTER_TYPES, []).

%% Notify Password Recovery
-define(PWD_RECOVERY_HEADERS, [<<"Email">>, <<"Password">>, <<"Account-ID">>]).
-define(OPTIONAL_PWD_RECOVERY_HEADERS, [<<"First-Name">>, <<"Last-Name">>, <<"Account-DB">>, <<"Request">>]).
-define(PWD_RECOVERY_VALUES, [{<<"Event-Category">>, <<"notification">>}
                              ,{<<"Event-Name">>, <<"password_recovery">>}
                             ]).
-define(PWD_RECOVERY_TYPES, []).

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

%%--------------------------------------------------------------------
%% @doc Deregister (unregister is a key word) - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
deregister(Prop) when is_list(Prop) ->
    case deregister_v(Prop) of
        true -> wh_api:build_message(Prop, ?DEREGISTER_HEADERS, ?OPTIONAL_DEREGISTER_HEADERS);
        false -> {error, "Proplist failed validation for deregister"}
    end;
deregister(JObj) ->
    deregister(wh_json:to_proplist(JObj)).

-spec deregister_v/1 :: (api_terms()) -> boolean().
deregister_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?DEREGISTER_HEADERS, ?DEREGISTER_VALUES, ?DEREGISTER_TYPES);
deregister_v(JObj) ->
    deregister_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Pwd_Recovery (unregister is a key word) - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
pwd_recovery(Prop) when is_list(Prop) ->
    case pwd_recovery_v(Prop) of
        true -> wh_api:build_message(Prop, ?PWD_RECOVERY_HEADERS, ?OPTIONAL_PWD_RECOVERY_HEADERS);
        false -> {error, "Proplist failed validation for pwd_recovery"}
    end;
pwd_recovery(JObj) ->
    pwd_recovery(wh_json:to_proplist(JObj)).

-spec pwd_recovery_v/1 :: (api_terms()) -> boolean().
pwd_recovery_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PWD_RECOVERY_HEADERS, ?PWD_RECOVERY_VALUES, ?PWD_RECOVERY_TYPES);
pwd_recovery_v(JObj) ->
    pwd_recovery_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (binary(), proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    amqp_util:notifications_exchange(),
    bind_to_q(Queue, props:get_value(restrict_to, Props, [])).

bind_to_q(Q, undefined) ->
    amqp_util:bind_q_to_notifications(Q, ?NOTIFY_VOICEMAIL_NEW),
    amqp_util:bind_q_to_notifications(Q, ?NOTIFY_MWI_UPDATE),
    amqp_util:bind_q_to_notifications(Q, ?NOTIFY_DEREGISTER),
    amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PWD_RECOVERY);
bind_to_q(Q, [new_voicemail|T]) ->
    amqp_util:bind_q_to_notifications(Q, ?NOTIFY_VOICEMAIL_NEW),
    bind_to_q(Q, T);
bind_to_q(Q, [mwi_update|T]) ->
    amqp_util:bind_q_to_notifications(Q, ?NOTIFY_MWI_UPDATE),
    bind_to_q(Q, T);
bind_to_q(Q, [deregister|T]) ->
    amqp_util:bind_q_to_notifications(Q, ?NOTIFY_DEREGISTER),
    bind_to_q(Q, T);
bind_to_q(Q, [pwd_recovery|T]) ->
    amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PWD_RECOVERY),
    bind_to_q(Q, T);
bind_to_q(_Q, []) ->
    ok.

-spec unbind_q/1 :: (binary()) -> 'ok'.
-spec unbind_q/2 :: (binary(), proplist()) -> 'ok'.

unbind_q(Queue) ->
    unbind_q_from(Queue, undefined).
    
unbind_q(Queue, Props) ->
    amqp_util:notifications_exchange(),
    unbind_q_from(Queue, props:get_value(restrict_to, Props, [])).

unbind_q_from(Q, undefined) ->
    amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_VOICEMAIL_NEW),
    amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_MWI_UPDATE),
    amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PWD_RECOVERY),
    amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_DEREGISTER);
unbind_q_from(Q, [new_voicemail|T]) ->
    amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_VOICEMAIL_NEW),
    unbind_q_from(Q, T);
unbind_q_from(Q, [mwi_update|T]) ->
    amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_MWI_UPDATE),
    unbind_q_from(Q, T);
unbind_q_from(Q, [deregister|T]) ->
    amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_DEREGISTER),
    unbind_q_from(Q, T);
unbind_q_from(Q, [pwd_recovery|T]) ->
    amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PWD_RECOVERY),
    unbind_q_from(Q, T);
unbind_q_from(_Q, []) ->
    ok.

-spec publish_voicemail/1 :: (api_terms()) -> 'ok'.
-spec publish_voicemail/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_voicemail(JObj) ->
    publish_voicemail(JObj, ?DEFAULT_CONTENT_TYPE).
publish_voicemail(Voicemail, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Voicemail, ?VOICEMAIL_VALUES, fun ?MODULE:voicemail/1),
    amqp_util:notifications_publish(?NOTIFY_VOICEMAIL_NEW, Payload, ContentType).

-spec publish_mwi_update/1 :: (api_terms()) -> 'ok'.
-spec publish_mwi_update/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_mwi_update(JObj) ->
    publish_mwi_update(JObj, ?DEFAULT_CONTENT_TYPE).
publish_mwi_update(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?MWI_REQ_VALUES, fun ?MODULE:mwi_update/1),
    amqp_util:notifications_publish(?NOTIFY_MWI_UPDATE, Payload, ContentType).

-spec publish_deregister/1 :: (api_terms()) -> 'ok'.
-spec publish_deregister/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_deregister(JObj) ->
    publish_deregister(JObj, ?DEFAULT_CONTENT_TYPE).
publish_deregister(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?DEREGISTER_VALUES, fun ?MODULE:deregister/1),
    amqp_util:notifications_publish(?NOTIFY_DEREGISTER, Payload, ContentType).

-spec publish_pwd_recovery/1 :: (api_terms()) -> 'ok'.
-spec publish_pwd_recovery/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_pwd_recovery(JObj) ->
    publish_pwd_recovery(JObj, ?DEFAULT_CONTENT_TYPE).
publish_pwd_recovery(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?PWD_RECOVERY_VALUES, fun ?MODULE:pwd_recovery/1),
    amqp_util:notifications_publish(?NOTIFY_PWD_RECOVERY, Payload, ContentType).
