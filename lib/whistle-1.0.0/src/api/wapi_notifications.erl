%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2012, VoIP INC
%%% @doc
%%% Notification messages, like voicemail left
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(wapi_notifications).

-export([bind_q/2, unbind_q/1, unbind_q/2]).

-export([voicemail/1, voicemail_v/1]).
-export([fax/1, fax_v/1]).
-export([mwi_update/1, mwi_update_v/1]).
-export([mwi_query/1, mwi_query_v/1]).
-export([register/1, register_v/1]).
-export([deregister/1, deregister_v/1]).
-export([presence_probe/1, presence_probe_v/1]).
-export([presence_update/1, presence_update_v/1]).
-export([pwd_recovery/1, pwd_recovery_v/1]).
-export([new_account/1, new_account_v/1]).
-export([port_request/1, port_request_v/1]).
-export([ported/1, ported_v/1]).
-export([cnam_request/1, cnam_request_v/1]).
-export([low_balance/1, low_balance_v/1]).
-export([transaction/1, transaction_v/1]).
-export([system_alert/1, system_alert_v/1]).

-export([publish_voicemail/1, publish_voicemail/2]).
-export([publish_fax/1, publish_fax/2]).
-export([publish_mwi_update/1, publish_mwi_update/2]).
-export([publish_mwi_query/1, publish_mwi_query/2]).
-export([publish_register/1, publish_register/2]).
-export([publish_deregister/1, publish_deregister/2]).
-export([publish_presence_probe/1, publish_presence_probe/2]).
-export([publish_presence_update/1, publish_presence_update/2]).
-export([publish_pwd_recovery/1, publish_pwd_recovery/2]).
-export([publish_new_account/1, publish_new_account/2]).
-export([publish_port_request/1, publish_port_request/2]).
-export([publish_ported/1, publish_ported/2]).
-export([publish_cnam_request/1, publish_cnam_request/2]).
-export([publish_low_balance/1, publish_low_balance/2]).
-export([publish_transaction/1, publish_transaction/2]).
-export([publish_system_alert/1, publish_system_alert/2]).

-include_lib("wh_api.hrl").

-define(NOTIFY_VOICEMAIL_NEW, <<"notifications.voicemail.new">>).
-define(NOTIFY_FAX_NEW, <<"notifications.fax.new">>).
-define(NOTIFY_MWI_UPDATE, <<"notifications.sip.mwi_update">>).
-define(NOTIFY_MWI_QUERY, <<"notifications.sip.mwi_query">>).
-define(NOTIFY_DEREGISTER, <<"notifications.sip.deregister">>).
-define(NOTIFY_REGISTER, <<"notifications.sip.register">>).
-define(NOTIFY_PRESENCE_UPDATE, <<"notifications.presence.update">>).
-define(NOTIFY_PRESENCE_PROBE, <<"notifications.presence.probe">>).
-define(NOTIFY_PRESENCE_IN, <<"notifications.presence.in">>).
-define(NOTIFY_PRESENCE_OUT, <<"notifications.presence.out">>).
-define(NOTIFY_PWD_RECOVERY, <<"notifications.password.recovery">>).
-define(NOTIFY_NEW_ACCOUNT, <<"notifications.account.new">>).
%% -define(NOTIFY_DELETE_ACCOUNT, <<"notifications.account.delete">>).
-define(NOTIFY_PORT_REQUEST, <<"notifications.number.port">>).
-define(NOTIFY_PORTED, <<"notifications.number.ported">>).
-define(NOTIFY_CNAM_REQUEST, <<"notifications.number.cnam">>).
-define(NOTIFY_LOW_BALANCE, <<"notifications.account.low_balance">>).
-define(NOTIFY_TRANSACTION, <<"notifications.account.transaction">>).
-define(NOTIFY_SYSTEM_ALERT, <<"notifications.system.alert">>).

%% Notify New Voicemail
-define(VOICEMAIL_HEADERS, [<<"From-User">>, <<"From-Realm">>
                            ,<<"To-User">>, <<"To-Realm">>
                            ,<<"Account-DB">>
                            ,<<"Voicemail-Box">>, <<"Voicemail-Name">>
                            ,<<"Voicemail-Timestamp">>
                           ]).
-define(OPTIONAL_VOICEMAIL_HEADERS, [<<"Voicemail-Length">>, <<"Call-ID">>
                                     ,<<"Caller-ID-Number">>, <<"Caller-ID-Name">>
                                     ,<<"Voicemail-Transcription">>, <<"Delete-After-Notify">>
                                    ]).
-define(VOICEMAIL_VALUES, [{<<"Event-Category">>, <<"notification">>}
                           ,{<<"Event-Name">>, <<"new_voicemail">>}
                          ]).
-define(VOICEMAIL_TYPES, [{<<"Delete-After-Notify">>, fun wh_util:is_boolean/1}]).

%% Notify New Fax
-define(FAX_HEADERS, [<<"From-User">>, <<"From-Realm">>
                          ,<<"To-User">>, <<"To-Realm">>
                          ,<<"Account-DB">>, <<"Fax-ID">>
                     ]).
-define(OPTIONAL_FAX_HEADERS, [<<"Caller-ID-Name">>, <<"Caller-ID-Number">>, <<"Call-ID">>
                                   ,<<"Total-Pages">>, <<"Transferred-Pages">>
                                   ,<<"Transfer-Rate">>, <<"Result-Text">>, <<"ECM-Used">>
                                   ,<<"Owner-ID">>, <<"Fax-Timestamp">>
                              ]).
-define(FAX_VALUES, [{<<"Event-Category">>, <<"notification">>}
                     ,{<<"Event-Name">>, <<"new_fax">>}
                    ]).
-define(FAX_TYPES, []).

%% Notify updated MWI
-define(MWI_REQ_HEADERS, [<<"Notify-User">>, <<"Notify-Realm">>, <<"Messages-New">>, <<"Messages-Saved">>]).
-define(OPTIONAL_MWI_REQ_HEADERS, [<<"Messages-Urgent">>, <<"Messages-Urgent-Saved">>
                                       ,<<"Call-ID">>, <<"Subscription-Call-ID">>
                                       ,<<"Switch-Nodename">>, <<"Message-Account">>
                                  ]).
-define(MWI_REQ_VALUES, [{<<"Event-Category">>, <<"notification">>}
                         ,{<<"Event-Name">>, <<"mwi">>}
                        ]).
-define(MWI_REQ_TYPES, [{<<"Messages-New">>, fun(I) -> is_integer(wh_util:to_integer(I)) end}
                        ,{<<"Messages-Saved">>, fun(I) -> is_integer(wh_util:to_integer(I)) end}
                        ,{<<"Messages-Urgent">>, fun(I) -> is_integer(wh_util:to_integer(I)) end}
                        ,{<<"Messages-Urgent-Saved">>, fun(I) -> is_integer(wh_util:to_integer(I)) end}
                       ]).

%% Notify updated MWI
-define(MWI_QUERY_HEADERS, [<<"Username">>, <<"Realm">>]).
-define(OPTIONAL_MWI_QUERY_HEADERS, [<<"Call-ID">>, <<"Subscription-Call-ID">>
                                         ,<<"Switch-Nodename">>, <<"Message-Account">>
                                    ]).
-define(MWI_QUERY_VALUES, [{<<"Event-Category">>, <<"notification">>}
                           ,{<<"Event-Name">>, <<"mwi_query">>}
                          ]).
-define(MWI_QUERY_TYPES, []).

%% Notify Presence_Probe
-define(PRESENCE_PROBE_HEADERS, [<<"From">>, <<"To">>, <<"Switch-Nodename">>]).
-define(OPTIONAL_PRESENCE_PROBE_HEADERS, [<<"From-User">>, <<"From-Realm">>, <<"To-User">>, <<"To-Realm">>
                                              ,<<"Expires">>, <<"Subscription-Call-ID">>, <<"Subscription-Type">>
                                              ,<<"Subscription">>, <<"Dialog-State">>
                                         ]).
-define(PRESENCE_PROBE_VALUES, [{<<"Event-Category">>, <<"notification">>}
                                ,{<<"Event-Name">>, <<"presence_probe">>}
                               ]).
-define(PRESENCE_PROBE_TYPES, []).

%% Notify Presence Update
-define(PRESENCE_UPDATE_HEADERS, [<<"Presence-ID">>]).
-define(OPTIONAL_PRESENCE_UPDATE_HEADERS, [<<"To">>, <<"From">>, <<"State">>
                                               ,<<"Call-ID">>, <<"Subscription-Call-ID">>
                                               ,<<"Switch-Nodename">>, <<"Dialog-State">>
                                          ]).
-define(PRESENCE_UPDATE_VALUES, [{<<"Event-Category">>, <<"notification">>}
                                 ,{<<"Event-Name">>, <<"presence_update">>}
                                ]).
-define(PRESENCE_UPDATE_TYPES, []).

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

%% Notify Register
-define(REGISTER_HEADERS, [<<"Username">>, <<"Realm">>, <<"Account-ID">>]).
-define(OPTIONAL_REGISTER_HEADERS, [<<"Status">>, <<"User-Agent">>, <<"Call-ID">>, <<"Profile-Name">>, <<"Presence-Hosts">>
                                          ,<<"From-User">>, <<"From-Host">>, <<"FreeSWITCH-Hostname">>, <<"RPid">>
                                          ,<<"To-User">>, <<"To-Host">>, <<"Network-IP">>, <<"Network-Port">>
                                          ,<<"Event-Timestamp">>, <<"Contact">>, <<"Expires">>, <<"Account-DB">>
                                          ,<<"Authorizing-ID">>, <<"Suppress-Unregister-Notify">>
                                     ]).
-define(REGISTER_VALUES, [{<<"Event-Category">>, <<"notification">>}
                            ,{<<"Event-Name">>, <<"register">>}
                           ]).
-define(REGISTER_TYPES, []).

%% Notify Password Recovery
-define(PWD_RECOVERY_HEADERS, [<<"Email">>, <<"Password">>, <<"Account-ID">>]).
-define(OPTIONAL_PWD_RECOVERY_HEADERS, [<<"First-Name">>, <<"Last-Name">>, <<"Account-DB">>, <<"Request">>]).
-define(PWD_RECOVERY_VALUES, [{<<"Event-Category">>, <<"notification">>}
                              ,{<<"Event-Name">>, <<"password_recovery">>}
                             ]).
-define(PWD_RECOVERY_TYPES, []).

%% Notify New Account
-define(NEW_ACCOUNT_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_NEW_ACCOUNT_HEADERS, [<<"Account-DB">>, <<"Account-Name">>, <<"Account-API-Key">>, <<"Account-Realm">>]).
-define(NEW_ACCOUNT_VALUES, [{<<"Event-Category">>, <<"notification">>}
                              ,{<<"Event-Name">>, <<"new_account">>}
                             ]).
-define(NEW_ACCOUNT_TYPES, []).

%% Notify Port Request
-define(PORT_REQUEST_HEADERS, [<<"Account-ID">>, <<"Number">>, <<"Port">>]).
-define(OPTIONAL_PORT_REQUEST_HEADERS, [<<"Number-State">>, <<"Local-Number">>, <<"Authorized-By">>, <<"Request">>]).
-define(PORT_REQUEST_VALUES, [{<<"Event-Category">>, <<"notification">>}
                              ,{<<"Event-Name">>, <<"port_request">>}
                             ]).
-define(PORT_REQUEST_TYPES, []).

%% Notify Ported Request
-define(PORTED_HEADERS, [<<"Account-ID">>, <<"Number">>, <<"Port">>]).
-define(OPTIONAL_PORTED_HEADERS, [<<"Number-State">>, <<"Local-Number">>, <<"Authorized-By">>, <<"Request">>]).
-define(PORTED_VALUES, [{<<"Event-Category">>, <<"notification">>}
                                ,{<<"Event-Name">>, <<"ported">>}
                               ]).
-define(PORTED_TYPES, []).

%% Notify Cnam Request
-define(CNAM_REQUEST_HEADERS, [<<"Account-ID">>, <<"Number">>, <<"Cnam">>]).
-define(OPTIONAL_CNAM_REQUEST_HEADERS, [<<"Number-State">>, <<"Local-Number">>, <<"Acquired-For">>, <<"Request">>]).
-define(CNAM_REQUEST_VALUES, [{<<"Event-Category">>, <<"notification">>}
                              ,{<<"Event-Name">>, <<"cnam_request">>}
                             ]).
-define(CNAM_REQUEST_TYPES, []).

%% Notify Low Balance
-define(LOW_BALANCE_HEADERS, [<<"Account-ID">>, <<"Current-Balance">>]).
-define(OPTIONAL_LOW_BALANCE_HEADERS, []).
-define(LOW_BALANCE_VALUES, [{<<"Event-Category">>, <<"notification">>}
                             ,{<<"Event-Name">>, <<"low_balance">>}
                            ]).
-define(LOW_BALANCE_TYPES, []).

%% Notify Transaction
-define(TRANSACTION_HEADERS, [<<"Account-ID">>, <<"Transaction">>]).
-define(OPTIONAL_TRANSACTION_HEADERS, [<<"Service-Plan">>, <<"Billing-ID">>]).
-define(TRANSACTION_VALUES, [{<<"Event-Category">>, <<"notification">>}
                             ,{<<"Event-Name">>, <<"transaction">>}
                            ]).
-define(TRANSACTION_TYPES, []).

%% Notify System Alert
-define(SYSTEM_ALERT_HEADERS, [<<"Subject">>, <<"Message">>]).
-define(OPTIONAL_SYSTEM_ALERT_HEADERS, [<<"Pid">>, <<"Module">>, <<"Line">>, <<"Request-ID">>, <<"Section">>
                                            ,<<"Node">>, <<"Details">>, <<"Account-ID">>
                                       ]).
-define(SYSTEM_ALERT_VALUES, [{<<"Event-Category">>, <<"notification">>}
                              ,{<<"Event-Name">>, <<"system_alert">>}
                             ]).
-define(SYSTEM_ALERT_TYPES, []).

%%--------------------------------------------------------------------
%% @doc
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
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
fax(Prop) when is_list(Prop) ->
    case fax_v(Prop) of
        true -> wh_api:build_message(Prop, ?FAX_HEADERS, ?OPTIONAL_FAX_HEADERS);
        false -> {error, "Proplist failed validation for fax"}
    end;
fax(JObj) ->
    fax(wh_json:to_proplist(JObj)).

-spec fax_v/1 :: (api_terms()) -> boolean().
fax_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?FAX_HEADERS, ?FAX_VALUES, ?FAX_TYPES);
fax_v(JObj) ->
    fax_v(wh_json:to_proplist(JObj)).

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
%% @doc MWI - Query the Message Waiting Indicator on a device - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec mwi_query/1 :: (api_terms()) -> {'ok', iolist()} | {'error', string()}.
mwi_query(Prop) when is_list(Prop) ->
    case mwi_query_v(Prop) of
        true -> wh_api:build_message(Prop, ?MWI_QUERY_HEADERS, ?OPTIONAL_MWI_QUERY_HEADERS);
        false -> {error, "Proplist failed validation for mwi query"}
    end;
mwi_query(JObj) ->
    mwi_query(wh_json:to_proplist(JObj)).

-spec mwi_query_v/1 :: (api_terms()) -> boolean().
mwi_query_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?MWI_QUERY_HEADERS, ?MWI_QUERY_VALUES, ?MWI_QUERY_TYPES);
mwi_query_v(JObj) ->
    mwi_query_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Register (unregister is a key word) - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
register(Prop) when is_list(Prop) ->
    case register_v(Prop) of
        true -> wh_api:build_message(Prop, ?REGISTER_HEADERS, ?OPTIONAL_REGISTER_HEADERS);
        false -> {error, "Proplist failed validation for register"}
    end;
register(JObj) ->
    register(wh_json:to_proplist(JObj)).

-spec register_v/1 :: (api_terms()) -> boolean().
register_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?REGISTER_HEADERS, ?REGISTER_VALUES, ?REGISTER_TYPES);
register_v(JObj) ->
    register_v(wh_json:to_proplist(JObj)).

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
%% @doc Presence_Probe (unregister is a key word) - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
presence_probe(Prop) when is_list(Prop) ->
    case presence_probe_v(Prop) of
        true -> wh_api:build_message(Prop, ?PRESENCE_PROBE_HEADERS, ?OPTIONAL_PRESENCE_PROBE_HEADERS);
        false -> {error, "Proplist failed validation for presence_probe"}
    end;
presence_probe(JObj) ->
    presence_probe(wh_json:to_proplist(JObj)).

-spec presence_probe_v/1 :: (api_terms()) -> boolean().
presence_probe_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PRESENCE_PROBE_HEADERS, ?PRESENCE_PROBE_VALUES, ?PRESENCE_PROBE_TYPES);
presence_probe_v(JObj) ->
    presence_probe_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Presence_Update (unregister is a key word) - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
presence_update(Prop) when is_list(Prop) ->
    case presence_update_v(Prop) of
        true -> wh_api:build_message(Prop, ?PRESENCE_UPDATE_HEADERS, ?OPTIONAL_PRESENCE_UPDATE_HEADERS);
        false -> {error, "Proplist failed validation for presence_update"}
    end;
presence_update(JObj) ->
    presence_update(wh_json:to_proplist(JObj)).

-spec presence_update_v/1 :: (api_terms()) -> boolean().
presence_update_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PRESENCE_UPDATE_HEADERS, ?PRESENCE_UPDATE_VALUES, ?PRESENCE_UPDATE_TYPES);
presence_update_v(JObj) ->
    presence_update_v(wh_json:to_proplist(JObj)).

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

%%--------------------------------------------------------------------
%% @doc New account notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
new_account(Prop) when is_list(Prop) ->
    case new_account_v(Prop) of
        true -> wh_api:build_message(Prop, ?NEW_ACCOUNT_HEADERS, ?OPTIONAL_NEW_ACCOUNT_HEADERS);
        false -> {error, "Proplist failed validation for new_account"}
    end;
new_account(JObj) ->
    new_account(wh_json:to_proplist(JObj)).

-spec new_account_v/1 :: (api_terms()) -> boolean().
new_account_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?NEW_ACCOUNT_HEADERS, ?NEW_ACCOUNT_VALUES, ?NEW_ACCOUNT_TYPES);
new_account_v(JObj) ->
    new_account_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Port request notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
port_request(Prop) when is_list(Prop) ->
    case port_request_v(Prop) of
        true -> wh_api:build_message(Prop, ?PORT_REQUEST_HEADERS, ?OPTIONAL_PORT_REQUEST_HEADERS);
        false -> {error, "Proplist failed validation for port_request"}
    end;
port_request(JObj) ->
    port_request(wh_json:to_proplist(JObj)).

-spec port_request_v/1 :: (api_terms()) -> boolean().
port_request_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PORT_REQUEST_HEADERS, ?PORT_REQUEST_VALUES, ?PORT_REQUEST_TYPES);
port_request_v(JObj) ->
    port_request_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Ported request notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
ported(Prop) when is_list(Prop) ->
    case ported_v(Prop) of
        true -> wh_api:build_message(Prop, ?PORTED_HEADERS, ?OPTIONAL_PORTED_HEADERS);
        false -> {error, "Proplist failed validation for ported"}
    end;
ported(JObj) ->
    ported(wh_json:to_proplist(JObj)).

-spec ported_v/1 :: (api_terms()) -> boolean().
ported_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?PORTED_HEADERS, ?PORTED_VALUES, ?PORTED_TYPES);
ported_v(JObj) ->
    ported_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Cnam request notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
cnam_request(Prop) when is_list(Prop) ->
    case cnam_request_v(Prop) of
        true -> wh_api:build_message(Prop, ?CNAM_REQUEST_HEADERS, ?OPTIONAL_CNAM_REQUEST_HEADERS);
        false -> {error, "Proplist failed validation for cnam_request"}
    end;
cnam_request(JObj) ->
    cnam_request(wh_json:to_proplist(JObj)).

-spec cnam_request_v/1 :: (api_terms()) -> boolean().
cnam_request_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?CNAM_REQUEST_HEADERS, ?CNAM_REQUEST_VALUES, ?CNAM_REQUEST_TYPES);
cnam_request_v(JObj) ->
    cnam_request_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Low Balance notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
low_balance(Prop) when is_list(Prop) ->
    case low_balance_v(Prop) of
        true -> wh_api:build_message(Prop, ?LOW_BALANCE_HEADERS, ?OPTIONAL_LOW_BALANCE_HEADERS);
        false -> {error, "Proplist failed validation for low_balance"}
    end;
low_balance(JObj) ->
    low_balance(wh_json:to_proplist(JObj)).

-spec low_balance_v/1 :: (api_terms()) -> boolean().
low_balance_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?LOW_BALANCE_HEADERS, ?LOW_BALANCE_VALUES, ?LOW_BALANCE_TYPES);
low_balance_v(JObj) ->
    low_balance_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Low Balance notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
transaction(Prop) when is_list(Prop) ->
    case transaction_v(Prop) of
        true -> wh_api:build_message(Prop, ?TRANSACTION_HEADERS, ?OPTIONAL_TRANSACTION_HEADERS);
        false -> {error, "Proplist failed validation for transaction"}
    end;
transaction(JObj) ->
    transaction(wh_json:to_proplist(JObj)).

-spec transaction_v/1 :: (api_terms()) -> boolean().
transaction_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?TRANSACTION_HEADERS, ?TRANSACTION_VALUES, ?TRANSACTION_TYPES);
transaction_v(JObj) ->
    transaction_v(wh_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc System alert notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
system_alert(Prop) when is_list(Prop) ->
    case system_alert_v(Prop) of
        true -> wh_api:build_message(Prop, ?SYSTEM_ALERT_HEADERS, ?OPTIONAL_SYSTEM_ALERT_HEADERS);
        false -> {error, "Proplist failed validation for system_alert"}
    end;
system_alert(JObj) ->
    system_alert(wh_json:to_proplist(JObj)).

-spec system_alert_v/1 :: (api_terms()) -> boolean().
system_alert_v(Prop) when is_list(Prop) ->
    wh_api:validate(Prop, ?SYSTEM_ALERT_HEADERS, ?SYSTEM_ALERT_VALUES, ?SYSTEM_ALERT_TYPES);
system_alert_v(JObj) ->
    system_alert_v(wh_json:to_proplist(JObj)).

-spec bind_q/2 :: (ne_binary(), wh_proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    amqp_util:notifications_exchange(),
    bind_to_q(Queue, props:get_value(restrict_to, Props)).

bind_to_q(Q, undefined) ->
    ok = amqp_util:bind_q_to_notifications(Q, <<"notifications.*.*">>);
bind_to_q(Q, [new_voicemail|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_VOICEMAIL_NEW),
    bind_to_q(Q, T);
bind_to_q(Q, [new_fax|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_FAX_NEW),
    bind_to_q(Q, T);
bind_to_q(Q, [mwi_update|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_MWI_UPDATE),
    bind_to_q(Q, T);
bind_to_q(Q, [mwi_query|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_MWI_QUERY),
    bind_to_q(Q, T);
bind_to_q(Q, [register|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_REGISTER),
    bind_to_q(Q, T);
bind_to_q(Q, [deregister|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_DEREGISTER),
    bind_to_q(Q, T);
bind_to_q(Q, [presence_update|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PRESENCE_UPDATE),
    bind_to_q(Q, T);
bind_to_q(Q, [presence_probe|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PRESENCE_PROBE),
    bind_to_q(Q, T);
bind_to_q(Q, [presence_in|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PRESENCE_IN),
    bind_to_q(Q, T);
bind_to_q(Q, [presence_out|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PRESENCE_OUT),
    bind_to_q(Q, T);
bind_to_q(Q, [pwd_recovery|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PWD_RECOVERY),
    bind_to_q(Q, T);
bind_to_q(Q, [new_account|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_NEW_ACCOUNT),
    bind_to_q(Q, T);
bind_to_q(Q, [port_request|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PORT_REQUEST),
    bind_to_q(Q, T);
bind_to_q(Q, [ported|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PORTED),
    bind_to_q(Q, T);
bind_to_q(Q, [cnam_requests|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_CNAM_REQUEST),
    bind_to_q(Q, T);
bind_to_q(Q, [low_balance|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_LOW_BALANCE),
    bind_to_q(Q, T);
bind_to_q(Q, [transaction|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_TRANSACTION),
    bind_to_q(Q, T);
bind_to_q(Q, [system_alerts|T]) ->
    ok = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_SYSTEM_ALERT),
    bind_to_q(Q, T);
bind_to_q(_Q, []) ->
    ok.

-spec unbind_q/1 :: (binary()) -> 'ok'.
-spec unbind_q/2 :: (binary(), proplist()) -> 'ok'.

unbind_q(Queue) ->
    unbind_q_from(Queue, undefined).
unbind_q(Queue, Props) ->
    amqp_util:notifications_exchange(),
    unbind_q_from(Queue, props:get_value(restrict_to, Props)).

unbind_q_from(Q, undefined) ->
    ok = amqp_util:unbind_q_from_notifications(Q, <<"notifications.*.*">>);
unbind_q_from(Q, [new_voicemail|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_VOICEMAIL_NEW),
    unbind_q_from(Q, T);
unbind_q_from(Q, [new_fax|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_FAX_NEW),
    unbind_q_from(Q, T);
unbind_q_from(Q, [mwi_update|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_MWI_UPDATE),
    unbind_q_from(Q, T);
unbind_q_from(Q, [mwi_query|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_MWI_QUERY),
    unbind_q_from(Q, T);
unbind_q_from(Q, [register|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_REGISTER),
    unbind_q_from(Q, T);
unbind_q_from(Q, [deregister|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_DEREGISTER),
    unbind_q_from(Q, T);
unbind_q_from(Q, [presence_update|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PRESENCE_UPDATE),
    unbind_q_from(Q, T);
unbind_q_from(Q, [presence_probe|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PRESENCE_PROBE),
    unbind_q_from(Q, T);
unbind_q_from(Q, [presence_in|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PRESENCE_IN),
    unbind_q_from(Q, T);
unbind_q_from(Q, [presence_out|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PRESENCE_OUT),
    unbind_q_from(Q, T);
unbind_q_from(Q, [pwd_recovery|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PWD_RECOVERY),
    unbind_q_from(Q, T);
unbind_q_from(Q, [new_account|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_NEW_ACCOUNT),
    unbind_q_from(Q, T);
unbind_q_from(Q, [port_request|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PORT_REQUEST),
    unbind_q_from(Q, T);
unbind_q_from(Q, [ported|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PORTED),
    unbind_q_from(Q, T);
unbind_q_from(Q, [cnam_request|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_CNAM_REQUEST),
    unbind_q_from(Q, T);
unbind_q_from(Q, [low_balance|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_LOW_BALANCE),
    unbind_q_from(Q, T);
unbind_q_from(Q, [transaction|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_TRANSACTION),
    unbind_q_from(Q, T);
unbind_q_from(Q, [system_alert|T]) ->
    ok = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_SYSTEM_ALERT),
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

-spec publish_fax/1 :: (api_terms()) -> 'ok'.
-spec publish_fax/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_fax(JObj) ->
    publish_fax(JObj, ?DEFAULT_CONTENT_TYPE).
publish_fax(Fax, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(Fax, ?FAX_VALUES, fun ?MODULE:fax/1),
    amqp_util:notifications_publish(?NOTIFY_FAX_NEW, Payload, ContentType).

-spec publish_mwi_update/1 :: (api_terms()) -> 'ok'.
-spec publish_mwi_update/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_mwi_update(JObj) ->
    publish_mwi_update(JObj, ?DEFAULT_CONTENT_TYPE).
publish_mwi_update(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?MWI_REQ_VALUES, fun ?MODULE:mwi_update/1),
    amqp_util:notifications_publish(?NOTIFY_MWI_UPDATE, Payload, ContentType).

-spec publish_mwi_query/1 :: (api_terms()) -> 'ok'.
-spec publish_mwi_query/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_mwi_query(JObj) ->
    publish_mwi_query(JObj, ?DEFAULT_CONTENT_TYPE).
publish_mwi_query(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?MWI_QUERY_VALUES, fun ?MODULE:mwi_query/1),
    amqp_util:notifications_publish(?NOTIFY_MWI_QUERY, Payload, ContentType).

-spec publish_register/1 :: (api_terms()) -> 'ok'.
-spec publish_register/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_register(JObj) ->
    publish_register(JObj, ?DEFAULT_CONTENT_TYPE).
publish_register(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?REGISTER_VALUES, fun ?MODULE:register/1),
    amqp_util:notifications_publish(?NOTIFY_REGISTER, Payload, ContentType).

-spec publish_deregister/1 :: (api_terms()) -> 'ok'.
-spec publish_deregister/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_deregister(JObj) ->
    publish_deregister(JObj, ?DEFAULT_CONTENT_TYPE).
publish_deregister(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?DEREGISTER_VALUES, fun ?MODULE:deregister/1),
    amqp_util:notifications_publish(?NOTIFY_DEREGISTER, Payload, ContentType).

-spec publish_presence_update/1 :: (api_terms()) -> 'ok'.
-spec publish_presence_update/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_presence_update(JObj) ->
    publish_presence_update(JObj, ?DEFAULT_CONTENT_TYPE).
publish_presence_update(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?PRESENCE_UPDATE_VALUES, fun ?MODULE:presence_update/1),
    amqp_util:notifications_publish(?NOTIFY_PRESENCE_UPDATE, Payload, ContentType).

-spec publish_presence_probe/1 :: (api_terms()) -> 'ok'.
-spec publish_presence_probe/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_presence_probe(JObj) ->
    publish_presence_probe(JObj, ?DEFAULT_CONTENT_TYPE).
publish_presence_probe(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?PRESENCE_PROBE_VALUES, fun ?MODULE:presence_probe/1),
    amqp_util:notifications_publish(?NOTIFY_PRESENCE_PROBE, Payload, ContentType).

-spec publish_pwd_recovery/1 :: (api_terms()) -> 'ok'.
-spec publish_pwd_recovery/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_pwd_recovery(JObj) ->
    publish_pwd_recovery(JObj, ?DEFAULT_CONTENT_TYPE).
publish_pwd_recovery(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?PWD_RECOVERY_VALUES, fun ?MODULE:pwd_recovery/1),
    amqp_util:notifications_publish(?NOTIFY_PWD_RECOVERY, Payload, ContentType).

-spec publish_new_account/1 :: (api_terms()) -> 'ok'.
-spec publish_new_account/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_new_account(JObj) ->
    publish_new_account(JObj, ?DEFAULT_CONTENT_TYPE).
publish_new_account(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?NEW_ACCOUNT_VALUES, fun ?MODULE:new_account/1),
    amqp_util:notifications_publish(?NOTIFY_NEW_ACCOUNT, Payload, ContentType).

-spec publish_port_request/1 :: (api_terms()) -> 'ok'.
-spec publish_port_request/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_port_request(JObj) ->
    publish_port_request(JObj, ?DEFAULT_CONTENT_TYPE).
publish_port_request(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?PORT_REQUEST_VALUES, fun ?MODULE:port_request/1),
    amqp_util:notifications_publish(?NOTIFY_PORT_REQUEST, Payload, ContentType).

-spec publish_ported/1 :: (api_terms()) -> 'ok'.
-spec publish_ported/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_ported(JObj) ->
    publish_ported(JObj, ?DEFAULT_CONTENT_TYPE).
publish_ported(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?PORTED_VALUES, fun ?MODULE:ported/1),
    amqp_util:notifications_publish(?NOTIFY_PORTED, Payload, ContentType).

-spec publish_cnam_request/1 :: (api_terms()) -> 'ok'.
-spec publish_cnam_request/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_cnam_request(JObj) ->
    publish_cnam_request(JObj, ?DEFAULT_CONTENT_TYPE).
publish_cnam_request(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?CNAM_REQUEST_VALUES, fun ?MODULE:cnam_request/1),
    amqp_util:notifications_publish(?NOTIFY_CNAM_REQUEST, Payload, ContentType).

-spec publish_low_balance/1 :: (api_terms()) -> 'ok'.
-spec publish_low_balance/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_low_balance(JObj) ->
    publish_low_balance(JObj, ?DEFAULT_CONTENT_TYPE).
publish_low_balance(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?LOW_BALANCE_VALUES, fun ?MODULE:low_balance/1),
    amqp_util:notifications_publish(?NOTIFY_LOW_BALANCE, Payload, ContentType).

-spec publish_transaction/1 :: (api_terms()) -> 'ok'.
-spec publish_transaction/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_transaction(JObj) ->
    publish_transaction(JObj, ?DEFAULT_CONTENT_TYPE).
publish_transaction(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?TRANSACTION_VALUES, fun ?MODULE:transaction/1),
    amqp_util:notifications_publish(?NOTIFY_TRANSACTION, Payload, ContentType).

-spec publish_system_alert/1 :: (api_terms()) -> 'ok'.
-spec publish_system_alert/2 :: (api_terms(), ne_binary()) -> 'ok'.
publish_system_alert(JObj) ->
    publish_system_alert(JObj, ?DEFAULT_CONTENT_TYPE).
publish_system_alert(API, ContentType) ->
    {ok, Payload} = wh_api:prepare_api_payload(API, ?SYSTEM_ALERT_VALUES, fun ?MODULE:system_alert/1),
    amqp_util:notifications_publish(?NOTIFY_SYSTEM_ALERT, Payload, ContentType).
