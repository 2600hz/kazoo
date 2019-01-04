%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2017, 2600Hz
%%% @doc
%%% Notification messages, like voicemail left
%%% @end
%%% @contributors
%%%   James Aimonetti
%%%   Karl Anderson
%%%-------------------------------------------------------------------
-module(kapi_notifications).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([voicemail_new/1, voicemail_new_v/1
        ,voicemail_full/1, voicemail_full_v/1
        ,voicemail_saved/1, voicemail_saved_v/1
        ,fax_inbound/1, fax_inbound_v/1
        ,fax_inbound_error/1, fax_inbound_error_v/1
        ,fax_outbound/1, fax_outbound_v/1
        ,fax_outbound_error/1, fax_outbound_error_v/1
        ,fax_outbound_smtp_error/1, fax_outbound_smtp_error_v/1
        ,register/1, register_v/1
        ,deregister/1, deregister_v/1
        ,first_occurrence/1, first_occurrence_v/1
        ,password_recovery/1, password_recovery_v/1
        ,new_account/1, new_account_v/1
        ,account_zone_change/1, account_zone_change_v/1
        ,new_user/1, new_user_v/1
        ,port_unconfirmed/1, port_unconfirmed_v/1
        ,port_request/1, port_request_v/1
        ,port_pending/1, port_pending_v/1
        ,port_scheduled/1, port_scheduled_v/1
        ,port_rejected/1, port_rejected_v/1
        ,port_cancel/1, port_cancel_v/1
        ,ported/1, ported_v/1
        ,port_comment/1, port_comment_v/1
        ,cnam_request/1, cnam_request_v/1
        ,low_balance/1, low_balance_v/1
        ,topup/1, topup_v/1
        ,transaction/1, transaction_v/1
        ,service_added/1, service_added_v/1
        ,system_alert/1, system_alert_v/1
        ,webhook/1, webhook_v/1
        ,webhook_disabled/1, webhook_disabled_v/1
         %% published on completion of notification
        ,notify_update/1, notify_update_v/1
        ,denied_emergency_bridge/1, denied_emergency_bridge_v/1
        ,customer_update/1, customer_update_v/1
        ,missed_call/1, missed_call_v/1
        ,skel/1, skel_v/1
        ,headers/1
        ,account_id/1, account_db/2
        ]).

-export([publish_voicemail_new/1, publish_voicemail_new/2
        ,publish_voicemail_full/1, publish_voicemail_full/2
        ,publish_voicemail_saved/1, publish_voicemail_saved/2
        ,publish_fax_inbound/1, publish_fax_inbound/2
        ,publish_fax_outbound/1, publish_fax_outbound/2
        ,publish_fax_inbound_error/1, publish_fax_inbound_error/2
        ,publish_fax_outbound_error/1, publish_fax_outbound_error/2
        ,publish_fax_outbound_smtp_error/1, publish_fax_outbound_smtp_error/2
        ,publish_register/1, publish_register/2
        ,publish_deregister/1, publish_deregister/2
        ,publish_first_occurrence/1, publish_first_occurrence/2
        ,publish_password_recovery/1, publish_password_recovery/2
        ,publish_new_account/1, publish_new_account/2
        ,publish_account_zone_change/1, publish_account_zone_change/2
        ,publish_new_user/1, publish_new_user/2
        ,publish_port_unconfirmed/1, publish_port_unconfirmed/2
        ,publish_port_request/1, publish_port_request/2
        ,publish_port_pending/1, publish_port_pending/2
        ,publish_port_scheduled/1, publish_port_scheduled/2
        ,publish_port_rejected/1, publish_port_rejected/2
        ,publish_port_cancel/1, publish_port_cancel/2
        ,publish_ported/1, publish_ported/2
        ,publish_port_comment/1, publish_port_comment/2
        ,publish_cnam_request/1, publish_cnam_request/2
        ,publish_low_balance/1, publish_low_balance/2
        ,publish_topup/1, publish_topup/2
        ,publish_transaction/1, publish_transaction/2
        ,publish_service_added/1, publish_service_added/2
        ,publish_system_alert/1, publish_system_alert/2
        ,publish_webhook/1, publish_webhook/2
        ,publish_webhook_disabled/1, publish_webhook_disabled/2
        ,publish_notify_update/2, publish_notify_update/3
        ,publish_denied_emergency_bridge/1, publish_denied_emergency_bridge/2
        ,publish_customer_update/1, publish_customer_update/2
        ,publish_missed_call/1, publish_missed_call/2
        ,publish_skel/1, publish_skel/2
        ]).

-include_lib("amqp_util.hrl").

%% supports preview mode
-define(DEFAULT_OPTIONAL_HEADERS, [<<"To">>, <<"Cc">>, <<"Bcc">>
                                  ,<<"From">>, <<"Reply-To">>
                                  ,<<"Subject">>, <<"HTML">>, <<"Text">>
                                  ,<<"Account-ID">>, <<"Account-DB">>
                                  ,<<"Preview">>, <<"Attachment-URL">>
                                  ]).

-define(NOTIFY_VOICEMAIL_SAVED, <<"notifications.voicemail.saved">>).
-define(NOTIFY_VOICEMAIL_NEW, <<"notifications.voicemail.new">>).
-define(NOTIFY_VOICEMAIL_FULL, <<"notifications.voicemail.full">>).
-define(NOTIFY_FAX_INBOUND, <<"notifications.fax.inbound">>).
-define(NOTIFY_FAX_OUTBOUND, <<"notifications.fax.outbound">>).
-define(NOTIFY_FAX_INBOUND_ERROR, <<"notifications.fax.inbound_error">>).
-define(NOTIFY_FAX_OUTBOUND_ERROR, <<"notifications.fax.outbound_error">>).
-define(NOTIFY_FAX_OUTBOUND_SMTP_ERROR, <<"notifications.fax.outbound_smtp_error">>).
-define(NOTIFY_DEREGISTER, <<"notifications.sip.deregister">>).
-define(NOTIFY_FIRST_OCCURRENCE, <<"notifications.sip.first_occurrence">>).
%%-define(NOTIFY_REGISTER_OVERWRITE, <<"notifications.sip.register_overwrite">>).
-define(NOTIFY_REGISTER, <<"notifications.sip.register">>).
-define(NOTIFY_PASSWORD_RECOVERY, <<"notifications.user.password_recovery">>).
-define(NOTIFY_NEW_ACCOUNT, <<"notifications.account.new">>).
-define(NOTIFY_ACCOUNT_ZONE_CHANGE, <<"notifications.account.zone_change">>).
-define(NOTIFY_NEW_USER, <<"notifications.user.new">>).
%% -define(NOTIFY_DELETE_ACCOUNT, <<"notifications.account.delete">>).
-define(NOTIFY_PORT_UNCONFIRMED, <<"notifications.number.port_unconfirmed">>).
-define(NOTIFY_PORT_REQUEST, <<"notifications.number.port">>).
-define(NOTIFY_PORT_PENDING, <<"notifications.number.port_pending">>).
-define(NOTIFY_PORT_SCHEDULED, <<"notifications.number.port_scheduled">>).
-define(NOTIFY_PORT_REJECTED, <<"notifications.number.port_rejected">>).
-define(NOTIFY_PORT_CANCEL, <<"notifications.number.port_cancel">>).
-define(NOTIFY_PORTED, <<"notifications.number.ported">>).
-define(NOTIFY_PORT_COMMENT, <<"notifications.number.port_comment">>).
-define(NOTIFY_CNAM_REQUEST, <<"notifications.number.cnam">>).
-define(NOTIFY_LOW_BALANCE, <<"notifications.account.low_balance">>).
-define(NOTIFY_TOPUP, <<"notifications.account.topup">>).
-define(NOTIFY_TRANSACTION, <<"notifications.account.transaction">>).
-define(NOTIFY_SERVICE_ADDED, <<"notifications.account.service_added">>).
-define(NOTIFY_SYSTEM_ALERT, <<"notifications.system.alert">>).
-define(NOTIFY_WEBHOOK_CALLFLOW, <<"notifications.webhook.callflow">>).
-define(NOTIFY_WEBHOOK_DISABLED, <<"notifications.webhook.disabled">>).
-define(NOTIFY_DENIED_EMERGENCY_BRIDGE, <<"notifications.registration.denied_emergency_bridge">>).
-define(NOTIFY_CUSTOMER_UPDATE, <<"notifications.user.customer_update">>).
-define(NOTIFY_MISSED_CALL, <<"notifications.sip.missed_call">>).
-define(NOTIFY_SKEL, <<"notifications.account.skel">>).

%% Notify Voicemail New
-define(VOICEMAIL_NEW_HEADERS, [<<"From-User">>, <<"From-Realm">>
                               ,<<"To-User">>, <<"To-Realm">>
                               ,<<"Account-ID">>
                               ,<<"Voicemail-Box">>, <<"Voicemail-ID">>
                               ,<<"Voicemail-Timestamp">>
                               ]).
-define(OPTIONAL_VOICEMAIL_NEW_HEADERS, [<<"Voicemail-Length">>, <<"Call-ID">>
                                        ,<<"Caller-ID-Number">>, <<"Caller-ID-Name">>
                                        ,<<"Voicemail-Transcription">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]).
-define(VOICEMAIL_NEW_VALUES, [{<<"Event-Category">>, <<"notification">>}
                              ,{<<"Event-Name">>, <<"voicemail_new">>}
                              ]).
-define(VOICEMAIL_NEW_TYPES, []).

%% Notify Voicemail Saved
-define(VOICEMAIL_SAVED_HEADERS, ?VOICEMAIL_NEW_HEADERS).
-define(OPTIONAL_VOICEMAIL_SAVED_HEADERS, ?OPTIONAL_VOICEMAIL_NEW_HEADERS).
-define(VOICEMAIL_SAVED_VALUES, [{<<"Event-Category">>, <<"notification">>}
                                ,{<<"Event-Name">>, <<"voicemail_saved">>}
                                ]).
-define(VOICEMAIL_SAVED_TYPES, []).

%% Notify Voicemail full
-define(VOICEMAIL_FULL_HEADERS, [<<"Account-ID">>, <<"Voicemail-Box">>
                                ,<<"Max-Message-Count">> ,<<"Message-Count">>
                                ]).
-define(OPTIONAL_VOICEMAIL_FULL_HEADERS, ?DEFAULT_OPTIONAL_HEADERS).
-define(VOICEMAIL_FULL_VALUES, [{<<"Event-Category">>, <<"notification">>}
                               ,{<<"Event-Name">>, <<"voicemail_full">>}
                               ]).
-define(VOICEMAIL_FULL_TYPES, []).

%% Notify New Fax
-define(FAX_INBOUND_HEADERS, [<<"From-User">>, <<"From-Realm">>
                             ,<<"To-User">>, <<"To-Realm">>
                             ,<<"Account-ID">>, <<"Fax-ID">>
                             ]).
-define(OPTIONAL_FAX_INBOUND_HEADERS, [<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                      ,<<"Callee-ID-Name">>, <<"Callee-ID-Number">>
                                      ,<<"Call-ID">>, <<"Fax-Info">>
                                      ,<<"Owner-ID">>, <<"FaxBox-ID">>
                                      ,<<"Fax-Notifications">>, <<"Fax-Timestamp">>
                                           | ?DEFAULT_OPTIONAL_HEADERS
                                      ]).
-define(FAX_INBOUND_VALUES, [{<<"Event-Category">>, <<"notification">>}
                            ,{<<"Event-Name">>, <<"inbound_fax">>}
                            ]).
-define(FAX_INBOUND_TYPES, []).

-define(FAX_INBOUND_ERROR_HEADERS, [<<"From-User">>, <<"From-Realm">>
                                   ,<<"To-User">>, <<"To-Realm">>
                                   ,<<"Account-ID">>
                                   ]).
-define(OPTIONAL_FAX_INBOUND_ERROR_HEADERS, [<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                            ,<<"Callee-ID-Name">>, <<"Callee-ID-Number">>
                                            ,<<"Call-ID">>, <<"Fax-Info">>, <<"Fax-ID">>
                                            ,<<"Owner-ID">>, <<"FaxBox-ID">>
                                            ,<<"Fax-Notifications">>, <<"Fax-Error">>
                                            ,<<"Fax-Timestamp">>, <<"Fax-Result-Code">>
                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                            ]).
-define(FAX_INBOUND_ERROR_VALUES, [{<<"Event-Category">>, <<"notification">>}
                                  ,{<<"Event-Name">>, <<"inbound_fax_error">>}
                                  ]).
-define(FAX_INBOUND_ERROR_TYPES, []).

-define(FAX_OUTBOUND_HEADERS, [<<"Caller-ID-Number">>, <<"Callee-ID-Number">>
                              ,<<"Account-ID">>, <<"Fax-JobId">>, <<"Fax-ID">>
                              ]).
-define(OPTIONAL_FAX_OUTBOUND_HEADERS, [<<"Caller-ID-Name">>, <<"Callee-ID-Name">>
                                       ,<<"Call-ID">>, <<"Fax-Info">>
                                       ,<<"Owner-ID">>, <<"FaxBox-ID">>
                                       ,<<"Fax-Notifications">>, <<"Fax-Timestamp">>
                                            | ?DEFAULT_OPTIONAL_HEADERS
                                       ]).
-define(FAX_OUTBOUND_VALUES, [{<<"Event-Category">>, <<"notification">>}
                             ,{<<"Event-Name">>, <<"outbound_fax">>}
                             ]).
-define(FAX_OUTBOUND_TYPES, []).

-define(FAX_OUTBOUND_ERROR_HEADERS, [<<"Fax-JobId">>, <<"Fax-ID">>]).
-define(OPTIONAL_FAX_OUTBOUND_ERROR_HEADERS, [<<"Caller-ID-Name">>, <<"Callee-ID-Name">>
                                             ,<<"Caller-ID-Number">>, <<"Callee-ID-Number">>
                                             ,<<"Call-ID">>, <<"Fax-Info">>
                                             ,<<"Owner-ID">>, <<"FaxBox-ID">>
                                             ,<<"Fax-Notifications">>, <<"Fax-Error">>
                                             ,<<"Fax-Timestamp">>
                                                  | ?DEFAULT_OPTIONAL_HEADERS
                                             ]).
-define(FAX_OUTBOUND_ERROR_VALUES, [{<<"Event-Category">>, <<"notification">>}
                                   ,{<<"Event-Name">>, <<"outbound_fax_error">>}
                                   ]).
-define(FAX_OUTBOUND_ERROR_TYPES, []).

-define(FAX_OUTBOUND_SMTP_ERROR_HEADERS, [<<"Fax-From-Email">>
                                         ,<<"Errors">>
                                         ,<<"Account-ID">>
                                         ]).
-define(OPTIONAL_FAX_OUTBOUND_SMTP_ERROR_HEADERS, [<<"Fax-To-Email">> | ?DEFAULT_OPTIONAL_HEADERS]).
-define(FAX_OUTBOUND_SMTP_ERROR_VALUES, [{<<"Event-Category">>, <<"notification">>}
                                        ,{<<"Event-Name">>, <<"outbound_smtp_fax_error">>}
                                        ]).
-define(FAX_OUTBOUND_SMTP_ERROR_TYPES, [{<<"Errors">>, fun(L) when is_list(L) -> kz_term:is_not_empty(L);
                                                          (_) -> 'false'
                                                       end
                                        }
                                       ]).

%% Notify Deregister
-define(DEREGISTER_HEADERS, [<<"Username">>, <<"Realm">>, <<"Account-ID">>]).
-define(OPTIONAL_DEREGISTER_HEADERS, [<<"Account-DB">>, <<"Authorizing-ID">>
                                     ,<<"Call-ID">>, <<"Contact">>
                                     ,<<"Event-Timestamp">>, <<"Expires">>
                                     ,<<"FreeSWITCH-Hostname">>, <<"From-Host">>, <<"From-User">>
                                     ,<<"Network-IP">>, <<"Network-Port">>
                                     ,<<"Presence-Hosts">>, <<"Profile-Name">>
                                     ,<<"RPid">>
                                     ,<<"Status">>, <<"Suppress-Unregister-Notify">>
                                     ,<<"To-Host">>, <<"To-User">>
                                     ,<<"User-Agent">>
                                          | ?DEFAULT_OPTIONAL_HEADERS
                                     ]).
-define(DEREGISTER_VALUES, [{<<"Event-Category">>, <<"notification">>}
                           ,{<<"Event-Name">>, <<"deregister">>}
                           ]).
-define(DEREGISTER_TYPES, []).

%% Notify Register
-define(REGISTER_HEADERS, [<<"Username">>, <<"Realm">>, <<"Account-ID">>]).
-define(OPTIONAL_REGISTER_HEADERS, [<<"Owner-ID">>, <<"User-Agent">>, <<"Call-ID">>
                                   ,<<"From-User">>, <<"From-Host">>
                                   ,<<"To-User">>, <<"To-Host">>
                                   ,<<"Network-IP">>, <<"Network-Port">>
                                   ,<<"Event-Timestamp">>, <<"Contact">>
                                   ,<<"Expires">>, <<"Account-DB">>
                                   ,<<"Authorizing-ID">>, <<"Authorizing-Type">>
                                   ,<<"Suppress-Unregister-Notify">>
                                        | ?DEFAULT_OPTIONAL_HEADERS
                                   ]).
-define(REGISTER_VALUES, [{<<"Event-Category">>, <<"notification">>}
                         ,{<<"Event-Name">>, <<"register">>}
                         ]).
-define(REGISTER_TYPES, []).

%% Notify First Occurrence
-define(FIRST_OCCURRENCE_HEADERS, [<<"Account-ID">>, <<"Occurrence">>]).
-define(OPTIONAL_FIRST_OCCURRENCE_HEADERS, ?DEFAULT_OPTIONAL_HEADERS).
-define(FIRST_OCCURRENCE_VALUES, [{<<"Event-Category">>, <<"notification">>}
                                 ,{<<"Event-Name">>, <<"first_occurrence">>}
                                 ]).
-define(FIRST_OCCURRENCE_TYPES, []).

%% Notify Password Recovery
-define(PASSWORD_RECOVERY_HEADERS, [<<"Account-ID">>
                                   ,<<"Email">>
                                   ,<<"Password-Reset-Link">>
                                   ]).
-define(OPTIONAL_PASSWORD_RECOVERY_HEADERS, [<<"Account-DB">>
                                            ,<<"First-Name">>
                                            ,<<"Last-Name">>
                                            ,<<"Timezone">>
                                            ,<<"User-ID">>
                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                            ]).
-define(PASSWORD_RECOVERY_VALUES, [{<<"Event-Category">>, <<"notification">>}
                                  ,{<<"Event-Name">>, <<"password_recovery">>}
                                  ]).
-define(PASSWORD_RECOVERY_TYPES, []).

%% Notify New Account
-define(NEW_ACCOUNT_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_NEW_ACCOUNT_HEADERS, [<<"Account-DB">>
                                      ,<<"Account-Name">>
                                      ,<<"Account-API-Key">>
                                      ,<<"Account-Realm">>
                                           | ?DEFAULT_OPTIONAL_HEADERS
                                      ]).
-define(NEW_ACCOUNT_VALUES, [{<<"Event-Category">>, <<"notification">>}
                            ,{<<"Event-Name">>, <<"new_account">>}
                            ]).
-define(NEW_ACCOUNT_TYPES, []).

%% Notify Account Zone Change
-define(ACCOUNT_ZONE_CHANGE_HEADERS, [<<"Account-ID">>
                                     ,<<"Zones">>
                                     ]).
-define(OPTIONAL_ACCOUNT_ZONE_CHANGE_HEADERS, ?DEFAULT_OPTIONAL_HEADERS).
-define(ACCOUNT_ZONE_CHANGE_VALUES, [{<<"Event-Category">>, <<"notification">>}
                                    ,{<<"Event-Name">>, <<"account_zone_change">>}
                                    ]).
-define(ACCOUNT_ZONE_CHANGE_TYPES, []).

%% Notify New User
-define(NEW_USER_HEADERS, [<<"Account-ID">>, <<"User-ID">>]).
-define(OPTIONAL_NEW_USER_HEADERS, [<<"Password">> | ?DEFAULT_OPTIONAL_HEADERS]).
-define(NEW_USER_VALUES, [{<<"Event-Category">>, <<"notification">>}
                         ,{<<"Event-Name">>, <<"new_user">>}
                         ]).
-define(NEW_USER_TYPES, []).

%% Notify Port Unconfirmed
-define(PORT_UNCONFIRMED_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_PORT_UNCONFIRMED_HEADERS, [<<"Authorized-By">>, <<"Port-Request-ID">>
                                           ,<<"Number-State">>, <<"Local-Number">>
                                           ,<<"Number">>, <<"Port">>, <<"Version">>
                                                | ?DEFAULT_OPTIONAL_HEADERS
                                           ]).
-define(PORT_UNCONFIRMED_VALUES, [{<<"Event-Category">>, <<"notification">>}
                                 ,{<<"Event-Name">>, <<"port_unconfirmed">>}
                                 ]).
-define(PORT_UNCONFIRMED_TYPES, []).

%% Notify Port Request
-define(PORT_REQUEST_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_PORT_REQUEST_HEADERS, [<<"Authorized-By">>, <<"Port-Request-ID">>
                                       ,<<"Number-State">>, <<"Local-Number">>
                                       ,<<"Number">>, <<"Port">>, <<"Version">>
                                            | ?DEFAULT_OPTIONAL_HEADERS
                                       ]).
-define(PORT_REQUEST_VALUES, [{<<"Event-Category">>, <<"notification">>}
                             ,{<<"Event-Name">>, <<"port_request">>}
                             ]).
-define(PORT_REQUEST_TYPES, []).

%% Notify Port Pending
-define(PORT_PENDING_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_PORT_PENDING_HEADERS, [<<"Authorized-By">>, <<"Port-Request-ID">>
                                       ,<<"Number-State">>, <<"Local-Number">>
                                       ,<<"Number">>, <<"Port">>, <<"Version">>
                                            | ?DEFAULT_OPTIONAL_HEADERS
                                       ]).
-define(PORT_PENDING_VALUES, [{<<"Event-Category">>, <<"notification">>}
                             ,{<<"Event-Name">>, <<"port_pending">>}
                             ]).
-define(PORT_PENDING_TYPES, []).

%% Notify Port Scheduled
-define(PORT_SCHEDULED_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_PORT_SCHEDULED_HEADERS, [<<"Authorized-By">>, <<"Port-Request-ID">>
                                         ,<<"Number-State">>, <<"Local-Number">>
                                         ,<<"Number">>, <<"Port">>, <<"Version">>
                                              | ?DEFAULT_OPTIONAL_HEADERS
                                         ]).
-define(PORT_SCHEDULED_VALUES, [{<<"Event-Category">>, <<"notification">>}
                               ,{<<"Event-Name">>, <<"port_scheduled">>}
                               ]).
-define(PORT_SCHEDULED_TYPES, []).

                                                % Notify Port Rejected
-define(PORT_REJECTED_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_PORT_REJECTED_HEADERS, [<<"Authorized-By">>, <<"Port-Request-ID">>
                                        ,<<"Number-State">>, <<"Local-Number">>
                                        ,<<"Number">>, <<"Port">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]).
-define(PORT_REJECTED_VALUES, [{<<"Event-Category">>, <<"notification">>}
                              ,{<<"Event-Name">>, <<"port_rejected">>}
                              ]).
-define(PORT_REJECTED_TYPES, []).

                                                % Notify Port Cancel
-define(PORT_CANCEL_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_PORT_CANCEL_HEADERS, [<<"Authorized-By">>, <<"Port-Request-ID">>
                                      ,<<"Number-State">>, <<"Local-Number">>
                                      ,<<"Number">>, <<"Port">>
                                           | ?DEFAULT_OPTIONAL_HEADERS
                                      ]).
-define(PORT_CANCEL_VALUES, [{<<"Event-Category">>, <<"notification">>}
                            ,{<<"Event-Name">>, <<"port_cancel">>}
                            ]).
-define(PORT_CANCEL_TYPES, []).

%% Notify Ported Request
-define(PORTED_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_PORTED_HEADERS, [<<"Number-State">>, <<"Local-Number">>, <<"Authorized-By">>, <<"Request">>
                                 ,<<"Port-Request-ID">>, <<"Number">>, <<"Port">>
                                      | ?DEFAULT_OPTIONAL_HEADERS
                                 ]).
-define(PORTED_VALUES, [{<<"Event-Category">>, <<"notification">>}
                       ,{<<"Event-Name">>, <<"ported">>}
                       ]).
-define(PORTED_TYPES, []).

%% Notify Ported Request
-define(PORT_COMMENT_HEADERS, [<<"Account-ID">>, <<"Comment">>]).
-define(OPTIONAL_PORT_COMMENT_HEADERS, [<<"Number-State">>, <<"Local-Number">>, <<"Authorized-By">>, <<"Request">>
                                       ,<<"Port-Request-ID">>, <<"Number">>, <<"Port">>
                                            | ?DEFAULT_OPTIONAL_HEADERS
                                       ]).
-define(PORT_COMMENT_VALUES, [{<<"Event-Category">>, <<"notification">>}
                             ,{<<"Event-Name">>, <<"port_comment">>}
                             ]).
-define(PORT_COMMENT_TYPES, []).

%% Notify Cnam Request
-define(CNAM_REQUEST_HEADERS, [<<"Account-ID">>, <<"Number">>, <<"Cnam">>]).
-define(OPTIONAL_CNAM_REQUEST_HEADERS, [<<"Number-State">>, <<"Local-Number">>, <<"Acquired-For">>, <<"Request">>
                                            | ?DEFAULT_OPTIONAL_HEADERS
                                       ]).
-define(CNAM_REQUEST_VALUES, [{<<"Event-Category">>, <<"notification">>}
                             ,{<<"Event-Name">>, <<"cnam_request">>}
                             ]).
-define(CNAM_REQUEST_TYPES, []).

%% Notify Low Balance
-define(LOW_BALANCE_HEADERS, [<<"Account-ID">>, <<"Current-Balance">>]).
-define(OPTIONAL_LOW_BALANCE_HEADERS, ?DEFAULT_OPTIONAL_HEADERS).
-define(LOW_BALANCE_VALUES, [{<<"Event-Category">>, <<"notification">>}
                            ,{<<"Event-Name">>, <<"low_balance">>}
                            ]).
-define(LOW_BALANCE_TYPES, []).

%%% Transaction Common Optional Headers
-define(COMMON_TRANSACTION_HEADERS, [<<"ID">>, <<"Add-Ons">>, <<"Discounts">>
                                    ,<<"Billing-Address">>, <<"Card-Last-Four">>, <<"Tax-Amount">>
                                    , <<"Purchase-Order">>, <<"Currency-Code">>
                                    ]).

%% Notify Top Up
-define(TOPUP_HEADERS, [<<"Account-ID">>, <<"Amount">>, <<"Timestamp">>
                       ,<<"Response">>, <<"Success">>
                       ]).
-define(OPTIONAL_TOPUP_HEADERS, ?COMMON_TRANSACTION_HEADERS ++ ?DEFAULT_OPTIONAL_HEADERS).
-define(TOPUP_VALUES, [{<<"Event-Category">>, <<"notification">>}
                      ,{<<"Event-Name">>, <<"topup">>}
                      ]).
-define(TOPUP_TYPES, []).

%% Notify Transaction
-define(TRANSACTION_HEADERS, [<<"Account-ID">>, <<"Amount">>, <<"Timestamp">>
                             ,<<"Response">>, <<"Success">>
                             ]).
-define(OPTIONAL_TRANSACTION_HEADERS, [<<"Service-Plan">>
                                           | ?COMMON_TRANSACTION_HEADERS
                                       ++ ?DEFAULT_OPTIONAL_HEADERS
                                      ]).
-define(TRANSACTION_VALUES, [{<<"Event-Category">>, <<"notification">>}
                            ,{<<"Event-Name">>, <<"transaction">>}
                            ]).
-define(TRANSACTION_TYPES, []).

%% Notify New Service Addition (from service audit log)
-define(SERVICE_ADDED_HEADERS, [<<"Account-ID">>, <<"Audit-Log">>, <<"Time-Stamp">>]).
-define(OPTIONAL_SERVICE_ADDED_HEADERS, ?DEFAULT_OPTIONAL_HEADERS).
-define(SERVICE_ADDED_VALUES, [{<<"Event-Category">>, <<"notification">>}
                              ,{<<"Event-Name">>, <<"service_added">>}
                              ]).
-define(SERVICE_ADDED_TYPES, []).

%% Notify System Alert
-define(SYSTEM_ALERT_HEADERS, [<<"Subject">>, <<"Message">>]).
-define(OPTIONAL_SYSTEM_ALERT_HEADERS, [<<"Details">>
                                       ,<<"Line">>
                                       ,<<"Module">>
                                       ,<<"Node">>
                                       ,<<"Pid">>
                                       ,<<"Request-ID">>
                                       ,<<"Section">>
                                            | ?DEFAULT_OPTIONAL_HEADERS
                                       ]).
-define(SYSTEM_ALERT_VALUES, [{<<"Event-Category">>, <<"notification">>}
                             ,{<<"Event-Name">>, <<"system_alert">>}
                             ]).
-define(SYSTEM_ALERT_TYPES, []).

%% Notify webhook
-define(WEBHOOK_HEADERS, [<<"Hook">>, <<"Data">>]).
-define(OPTIONAL_WEBHOOK_HEADERS, [<<"Timestamp">>
                                       | ?DEFAULT_OPTIONAL_HEADERS
                                  ]).
-define(WEBHOOK_VALUES, [{<<"Event-Category">>, <<"notification">>}
                        ,{<<"Event-Name">>, <<"webhook">>}
                        ]).
-define(WEBHOOK_TYPES, []).

%% Notify webhook
-define(WEBHOOK_DISABLED_HEADERS, [<<"Hook-ID">>, <<"Account-ID">>]).
-define(OPTIONAL_WEBHOOK_DISABLED_HEADERS, ?DEFAULT_OPTIONAL_HEADERS).
-define(WEBHOOK_DISABLED_VALUES, [{<<"Event-Category">>, <<"notification">>}
                                 ,{<<"Event-Name">>, <<"webhook_disabled">>}
                                 ]).
-define(WEBHOOK_DISABLED_TYPES, []).

-define(NOTIFY_UPDATE_HEADERS, [<<"Status">>]).
-define(OPTIONAL_NOTIFY_UPDATE_HEADERS, [<<"Failure-Message">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]).
-define(NOTIFY_UPDATE_VALUES, [{<<"Event-Category">>, <<"notification">>}
                              ,{<<"Event-Name">>, <<"update">>}
                              ,{<<"Status">>, [<<"completed">>
                                              ,<<"disabled">>
                                              ,<<"failed">>
                                              ,<<"ignored">>
                                              ,<<"pending">>
                                              ]
                               }
                              ]).
-define(NOTIFY_UPDATE_TYPES, []).

%% Denied Emergency Bridge
-define(DENIED_EMERGENCY_BRIDGE_HEADERS, [<<"Account-ID">>, <<"Call-ID">>]).
-define(OPTIONAL_DENIED_EMERGENCY_BRIDGE_HEADERS, [<<"Emergency-Caller-ID-Number">>
                                                  ,<<"Emergency-Caller-ID-Name">>
                                                  ,<<"Outbound-Caller-ID-Number">>
                                                  ,<<"Outbound-Caller-ID-Name">>
                                                       | ?DEFAULT_OPTIONAL_HEADERS
                                                  ]).
-define(DENIED_EMERGENCY_BRIDGE_VALUES, [{<<"Event-Category">>, <<"notification">>}
                                        ,{<<"Event-Name">>, <<"denied_emergency_bridge">>}
                                        ]).
-define(DENIED_EMERGENCY_BRIDGE_TYPES, []).

%% Customer update
-define(CUSTOMER_UPDATE_HEADERS, [<<"Account-ID">>]).
-define(OPTIONAL_CUSTOMER_UPDATE_HEADERS, [<<"Recipient-ID">>
                                          ,<<"User-Type">>
                                          ,<<"DataBag">>
                                          ,<<"Template-ID">>
                                               | ?DEFAULT_OPTIONAL_HEADERS
                                          ]).
-define(CUSTOMER_UPDATE_VALUES, [{<<"Event-Category">>, <<"notification">>}
                                ,{<<"Event-Name">>, <<"customer_update">>}
                                ]).
-define(CUSTOMER_UPDATE_TYPES, []).

%% Missed Call Alert
-define(MISSED_CALL_HEADERS, [<<"Account-ID">>,<<"Call-ID">>
                             ,<<"Call-Bridged">>, <<"Message-Left">>
                             ]).
-define(OPTIONAL_MISSED_CALL_HEADERS, [<<"From-User">>, <<"From-Realm">>
                                      ,<<"To-User">>, <<"To-Realm">>
                                      ,<<"Caller-ID-Name">>, <<"Caller-ID-Number">>
                                      ,<<"Timestamp">>, <<"Notify">>, <<"To">>
                                           | ?DEFAULT_OPTIONAL_HEADERS
                                      ]).
-define(MISSED_CALL_VALUES, [{<<"Event-Category">>, <<"notification">>}
                            ,{<<"Event-Name">>, <<"missed_call">>}
                            ]).
-define(MISSED_CALL_TYPES, []).

%% Skeleton
-define(SKEL_HEADERS, [<<"Account-ID">>, <<"User-ID">>]).
-define(OPTIONAL_SKEL_HEADERS, ?DEFAULT_OPTIONAL_HEADERS).
-define(SKEL_VALUES, [{<<"Event-Category">>, <<"notification">>}
                     ,{<<"Event-Name">>, <<"skel">>}
                     ]).
-define(SKEL_TYPES, []).

-spec account_id(kz_json:object()) -> api_binary().
account_id(JObj) ->
    Paths = [<<"account_id">>
            ,[<<"account">>, <<"_id">>]
            ,<<"pvt_account_id">>
            ,<<"_id">>, <<"id">>
            ,<<"Account-ID">>
            ,[<<"details">>, <<"account_id">>]
            ,[<<"Details">>, <<"Account-ID">>]
            ,[<<"details">>, <<"custom_channel_vars">>, <<"account_id">>]
            ,[<<"Details">>, <<"Custom-Channel-Vars">>, <<"Account-ID">>]
            ],
    kz_json:get_first_defined(Paths, JObj).

-spec account_db(kz_json:object(), boolean()) -> api_ne_binary().
account_db(JObj, StrictMODB) ->
    Paths = [<<"account_db">>, <<"pvt_account_db">>, <<"Account-DB">>],
    case kz_json:get_first_defined(Paths, JObj) of
        'undefined' ->
            case account_id(JObj) of
                'undefined' -> 'undefined';
                AccountId -> kz_util:format_account_db(AccountId)
            end;
        ?MATCH_MODB_SUFFIX_RAW(_, _, _)=Db -> maybe_strict_modb(Db, StrictMODB);
        ?MATCH_MODB_SUFFIX_UNENCODED(_, _, _)=Db -> maybe_strict_modb(Db, StrictMODB);
        ?MATCH_MODB_SUFFIX_ENCODED(_, _, _)=Db -> maybe_strict_modb(Db, StrictMODB);
        ?MATCH_ACCOUNT_RAW(_)=Db -> kz_util:format_account_db(Db);
        ?MATCH_ACCOUNT_UNENCODED(_)=Db -> kz_util:format_account_db(Db);
        ?MATCH_ACCOUNT_ENCODED(_)=Db -> kz_util:format_account_db(Db);
        ?MATCH_ACCOUNT_encoded(_)=Db -> kz_util:format_account_db(Db);
        OtherDb -> OtherDb
    end.

-spec maybe_strict_modb(kz_term:ne_binary(), boolean()) -> kz_term:ne_binary().
maybe_strict_modb(Db, 'true') ->
    kz_util:format_account_modb(Db, 'encoded');
maybe_strict_modb(Db, 'false') ->
    kz_util:format_account_db(Db).

-spec headers(ne_binary()) -> ne_binaries().
headers(<<"voicemail_new">>) ->
    ?VOICEMAIL_NEW_HEADERS ++ ?OPTIONAL_VOICEMAIL_NEW_HEADERS;
headers(<<"voicemail_full">>) ->
    ?VOICEMAIL_FULL_HEADERS ++ ?OPTIONAL_VOICEMAIL_FULL_HEADERS;
headers(<<"voicemail_saved">>) ->
    ?VOICEMAIL_SAVED_HEADERS ++ ?OPTIONAL_VOICEMAIL_SAVED_HEADERS;
headers(<<"fax_inbound_to_email">>) ->
    ?FAX_INBOUND_HEADERS ++ ?OPTIONAL_FAX_INBOUND_HEADERS;
headers(<<"fax_inbound_error_to_email">>) ->
    ?FAX_INBOUND_ERROR_HEADERS ++ ?OPTIONAL_FAX_INBOUND_ERROR_HEADERS;
headers(<<"fax_outbound_to_email">>) ->
    ?FAX_OUTBOUND_HEADERS ++ ?OPTIONAL_FAX_OUTBOUND_HEADERS;
headers(<<"fax_outbound_error_to_email">>) ->
    ?FAX_OUTBOUND_ERROR_HEADERS ++ ?OPTIONAL_FAX_OUTBOUND_ERROR_HEADERS;
headers(<<"fax_outbound_smtp_error">>) ->
    ?FAX_OUTBOUND_SMTP_ERROR_HEADERS ++ ?OPTIONAL_FAX_OUTBOUND_SMTP_ERROR_HEADERS;
headers(<<"low_balance">>) ->
    ?LOW_BALANCE_HEADERS ++ ?OPTIONAL_LOW_BALANCE_HEADERS;
headers(<<"new_account">>) ->
    ?NEW_ACCOUNT_HEADERS ++ ?OPTIONAL_NEW_ACCOUNT_HEADERS;
headers(<<"account_zone_change">>) ->
    ?ACCOUNT_ZONE_CHANGE_HEADERS ++ ?OPTIONAL_ACCOUNT_ZONE_CHANGE_HEADERS;
headers(<<"new_user">>) ->
    ?NEW_USER_HEADERS ++ ?OPTIONAL_NEW_USER_HEADERS;
headers(<<"deregister">>) ->
    ?DEREGISTER_HEADERS ++ ?OPTIONAL_DEREGISTER_HEADERS;
headers(<<"first_occurrence">>) ->
    ?FIRST_OCCURRENCE_HEADERS ++ ?OPTIONAL_FIRST_OCCURRENCE_HEADERS;
headers(<<"transaction">>) ->
    ?TRANSACTION_HEADERS ++ ?OPTIONAL_TRANSACTION_HEADERS;
headers(<<"service_added">>) ->
    ?SERVICE_ADDED_HEADERS ++ ?OPTIONAL_SERVICE_ADDED_HEADERS;
headers(<<"password_recovery">>) ->
    ?PASSWORD_RECOVERY_HEADERS ++ ?OPTIONAL_PASSWORD_RECOVERY_HEADERS;
headers(<<"system_alert">>) ->
    ?SYSTEM_ALERT_HEADERS ++ ?OPTIONAL_SYSTEM_ALERT_HEADERS;
headers(<<"cnam_request">>) ->
    ?CNAM_REQUEST_HEADERS ++ ?OPTIONAL_CNAM_REQUEST_HEADERS;
headers(<<"topup">>) ->
    ?TOPUP_HEADERS ++ ?OPTIONAL_TOPUP_HEADERS;
headers(<<"port_unconfirmed">>) ->
    ?PORT_UNCONFIRMED_HEADERS ++ ?OPTIONAL_PORT_UNCONFIRMED_HEADERS;
headers(<<"port_request">>) ->
    ?PORT_REQUEST_HEADERS ++ ?OPTIONAL_PORT_REQUEST_HEADERS;
headers(<<"port_pending">>) ->
    ?PORT_PENDING_HEADERS ++ ?OPTIONAL_PORT_PENDING_HEADERS;
headers(<<"port_scheduled">>) ->
    ?PORT_SCHEDULED_HEADERS ++ ?OPTIONAL_PORT_SCHEDULED_HEADERS;
headers(<<"port_cancel">>) ->
    ?PORT_CANCEL_HEADERS ++ ?OPTIONAL_PORT_CANCEL_HEADERS;
headers(<<"port_rejected">>) ->
    ?PORT_REJECTED_HEADERS ++ ?OPTIONAL_PORT_REJECTED_HEADERS;
headers(<<"ported">>) ->
    ?PORTED_HEADERS ++ ?OPTIONAL_PORTED_HEADERS;
headers(<<"port_comment">>) ->
    ?PORT_COMMENT_HEADERS ++ ?OPTIONAL_PORT_COMMENT_HEADERS;
headers(<<"webhook_disabled">>) ->
    ?WEBHOOK_DISABLED_HEADERS ++ ?OPTIONAL_WEBHOOK_DISABLED_HEADERS;
headers(<<"denied_emergency_bridge">>) ->
    ?DENIED_EMERGENCY_BRIDGE_HEADERS ++ ?OPTIONAL_DENIED_EMERGENCY_BRIDGE_HEADERS;
headers(<<"customer_update">>) ->
    ?CUSTOMER_UPDATE_HEADERS ++ ?OPTIONAL_CUSTOMER_UPDATE_HEADERS;
headers(<<"missed_call">>) ->
    ?MISSED_CALL_HEADERS ++ ?OPTIONAL_MISSED_CALL_HEADERS;
headers(<<"skel">>) ->
    ?SKEL_HEADERS ++ ?OPTIONAL_SKEL_HEADERS;
headers(_Notification) ->
    lager:warning("no notification headers for ~s", [_Notification]),
    [].

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec voicemail_new(api_terms()) -> api_formatter_return().
voicemail_new(Prop) when is_list(Prop) ->
    case voicemail_new_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?VOICEMAIL_NEW_HEADERS, ?OPTIONAL_VOICEMAIL_NEW_HEADERS);
        'false' -> {'error', "Proplist failed validation for voicemail_new"}
    end;
voicemail_new(JObj) -> voicemail_new(kz_json:to_proplist(JObj)).

-spec voicemail_new_v(api_terms()) -> boolean().
voicemail_new_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?VOICEMAIL_NEW_HEADERS, ?VOICEMAIL_NEW_VALUES, ?VOICEMAIL_NEW_TYPES);
voicemail_new_v(JObj) -> voicemail_new_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec voicemail_saved(api_terms()) -> api_formatter_return().
voicemail_saved(Prop) when is_list(Prop) ->
    case voicemail_saved_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?VOICEMAIL_SAVED_HEADERS, ?OPTIONAL_VOICEMAIL_SAVED_HEADERS);
        'false' -> {'error', "Proplist failed validation for voicemail_saved"}
    end;
voicemail_saved(JObj) -> voicemail_saved(kz_json:to_proplist(JObj)).

-spec voicemail_saved_v(api_terms()) -> boolean().
voicemail_saved_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?VOICEMAIL_SAVED_HEADERS, ?VOICEMAIL_SAVED_VALUES, ?VOICEMAIL_SAVED_TYPES);
voicemail_saved_v(JObj) -> voicemail_saved_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec voicemail_full(api_terms()) -> api_formatter_return().
voicemail_full(Prop) when is_list(Prop) ->
    case voicemail_full_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?VOICEMAIL_FULL_HEADERS, ?OPTIONAL_VOICEMAIL_FULL_HEADERS);
        'false' -> {'error', "Proplist failed validation for voicemail_full"}
    end;
voicemail_full(JObj) -> voicemail_full(kz_json:to_proplist(JObj)).

-spec voicemail_full_v(api_terms()) -> boolean().
voicemail_full_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?VOICEMAIL_FULL_HEADERS, ?VOICEMAIL_FULL_VALUES, ?VOICEMAIL_FULL_TYPES);
voicemail_full_v(JObj) -> voicemail_full_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec fax_inbound(api_terms()) -> api_formatter_return().
fax_inbound(Prop) when is_list(Prop) ->
    case fax_inbound_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?FAX_INBOUND_HEADERS, ?OPTIONAL_FAX_INBOUND_HEADERS);
        'false' -> {'error', "Proplist failed validation for inbound_fax"}
    end;
fax_inbound(JObj) -> fax_inbound(kz_json:to_proplist(JObj)).

-spec fax_inbound_v(api_terms()) -> boolean().
fax_inbound_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FAX_INBOUND_HEADERS, ?FAX_INBOUND_VALUES, ?FAX_INBOUND_TYPES);
fax_inbound_v(JObj) -> fax_inbound_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec fax_outbound(api_terms()) -> api_formatter_return().
fax_outbound(Prop) when is_list(Prop) ->
    case fax_outbound_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?FAX_OUTBOUND_HEADERS, ?OPTIONAL_FAX_OUTBOUND_HEADERS);
        'false' -> {'error', "Proplist failed validation for outbound_fax"}
    end;
fax_outbound(JObj) -> fax_outbound(kz_json:to_proplist(JObj)).

-spec fax_outbound_v(api_terms()) -> boolean().
fax_outbound_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FAX_OUTBOUND_HEADERS, ?FAX_OUTBOUND_VALUES, ?FAX_OUTBOUND_TYPES);
fax_outbound_v(JObj) -> fax_outbound_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec fax_inbound_error(api_terms()) -> api_formatter_return().
fax_inbound_error(Prop) when is_list(Prop) ->
    case fax_inbound_error_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?FAX_INBOUND_ERROR_HEADERS, ?OPTIONAL_FAX_INBOUND_ERROR_HEADERS);
        'false' -> {'error', "Proplist failed validation for inbound_fax_error"}
    end;
fax_inbound_error(JObj) -> fax_inbound_error(kz_json:to_proplist(JObj)).

-spec fax_inbound_error_v(api_terms()) -> boolean().
fax_inbound_error_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FAX_INBOUND_ERROR_HEADERS, ?FAX_INBOUND_ERROR_VALUES, ?FAX_INBOUND_ERROR_TYPES);
fax_inbound_error_v(JObj) -> fax_inbound_error_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec fax_outbound_error(api_terms()) -> api_formatter_return().
fax_outbound_error(Prop) when is_list(Prop) ->
    case fax_outbound_error_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?FAX_OUTBOUND_ERROR_HEADERS, ?OPTIONAL_FAX_OUTBOUND_ERROR_HEADERS);
        'false' -> {'error', "Proplist failed validation for outbound_fax_error"}
    end;
fax_outbound_error(JObj) -> fax_outbound_error(kz_json:to_proplist(JObj)).

-spec fax_outbound_error_v(api_terms()) -> boolean().
fax_outbound_error_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FAX_OUTBOUND_ERROR_HEADERS, ?FAX_OUTBOUND_ERROR_VALUES, ?FAX_OUTBOUND_ERROR_TYPES);
fax_outbound_error_v(JObj) -> fax_outbound_error_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec fax_outbound_smtp_error(api_terms()) -> api_formatter_return().
fax_outbound_smtp_error(Prop) when is_list(Prop) ->
    case fax_outbound_smtp_error_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?FAX_OUTBOUND_SMTP_ERROR_HEADERS, ?OPTIONAL_FAX_OUTBOUND_SMTP_ERROR_HEADERS);
        'false' -> {'error', "Proplist failed validation for outbound_smtp_fax_error"}
    end;
fax_outbound_smtp_error(JObj) -> fax_outbound_smtp_error(kz_json:to_proplist(JObj)).

-spec fax_outbound_smtp_error_v(api_terms()) -> boolean().
fax_outbound_smtp_error_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FAX_OUTBOUND_SMTP_ERROR_HEADERS, ?FAX_OUTBOUND_SMTP_ERROR_VALUES, ?FAX_OUTBOUND_SMTP_ERROR_TYPES);
fax_outbound_smtp_error_v(JObj) -> fax_outbound_smtp_error_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Register (unregister is a key word) - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec register(api_terms()) -> api_formatter_return().
register(Prop) when is_list(Prop) ->
    case register_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?REGISTER_HEADERS, ?OPTIONAL_REGISTER_HEADERS);
        'false' -> {'error', "Proplist failed validation for register"}
    end;
register(JObj) -> register(kz_json:to_proplist(JObj)).

-spec register_v(api_terms()) -> boolean().
register_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?REGISTER_HEADERS, ?REGISTER_VALUES, ?REGISTER_TYPES);
register_v(JObj) -> register_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Deregister (unregister is a key word) - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec deregister(api_terms()) -> api_formatter_return().
deregister(Prop) when is_list(Prop) ->
    case deregister_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DEREGISTER_HEADERS, ?OPTIONAL_DEREGISTER_HEADERS);
        'false' -> {'error', "Proplist failed validation for deregister"}
    end;
deregister(JObj) -> deregister(kz_json:to_proplist(JObj)).

-spec deregister_v(api_terms()) -> boolean().
deregister_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DEREGISTER_HEADERS, ?DEREGISTER_VALUES, ?DEREGISTER_TYPES);
deregister_v(JObj) -> deregister_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc First Call or registration notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec first_occurrence(api_terms()) -> api_formatter_return().
first_occurrence(Prop) when is_list(Prop) ->
    case first_occurrence_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?FIRST_OCCURRENCE_HEADERS, ?OPTIONAL_FIRST_OCCURRENCE_HEADERS);
        'false' -> {'error', "Proplist failed validation for first_occurrence"}
    end;
first_occurrence(JObj) -> first_occurrence(kz_json:to_proplist(JObj)).

-spec first_occurrence_v(api_terms()) -> boolean().
first_occurrence_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?FIRST_OCCURRENCE_HEADERS, ?FIRST_OCCURRENCE_VALUES, ?FIRST_OCCURRENCE_TYPES);
first_occurrence_v(JObj) -> first_occurrence_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Password_Recovery (unregister is a key word) - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec password_recovery(api_terms()) -> api_formatter_return().
password_recovery(Prop) when is_list(Prop) ->
    case password_recovery_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PASSWORD_RECOVERY_HEADERS, ?OPTIONAL_PASSWORD_RECOVERY_HEADERS);
        'false' -> {'error', "Proplist failed validation for password_recovery"}
    end;
password_recovery(JObj) -> password_recovery(kz_json:to_proplist(JObj)).

-spec password_recovery_v(api_terms()) -> boolean().
password_recovery_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PASSWORD_RECOVERY_HEADERS, ?PASSWORD_RECOVERY_VALUES, ?PASSWORD_RECOVERY_TYPES);
password_recovery_v(JObj) -> password_recovery_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc New account notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec new_account(api_terms()) -> api_formatter_return().
new_account(Prop) when is_list(Prop) ->
    case new_account_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?NEW_ACCOUNT_HEADERS, ?OPTIONAL_NEW_ACCOUNT_HEADERS);
        'false' -> {'error', "Proplist failed validation for new_account"}
    end;
new_account(JObj) -> new_account(kz_json:to_proplist(JObj)).

-spec new_account_v(api_terms()) -> boolean().
new_account_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?NEW_ACCOUNT_HEADERS, ?NEW_ACCOUNT_VALUES, ?NEW_ACCOUNT_TYPES);
new_account_v(JObj) -> new_account_v(kz_json:to_proplist(JObj)).

-spec account_zone_change(api_terms()) -> api_formatter_return().
account_zone_change(Prop) when is_list(Prop) ->
    case account_zone_change_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?ACCOUNT_ZONE_CHANGE_HEADERS, ?OPTIONAL_ACCOUNT_ZONE_CHANGE_HEADERS);
        'false' -> {'error', "Proplist failed validation for new_account"}
    end;
account_zone_change(JObj) -> account_zone_change(kz_json:to_proplist(JObj)).

-spec account_zone_change_v(api_terms()) -> boolean().
account_zone_change_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?ACCOUNT_ZONE_CHANGE_HEADERS, ?ACCOUNT_ZONE_CHANGE_VALUES, ?ACCOUNT_ZONE_CHANGE_TYPES);
account_zone_change_v(JObj) -> account_zone_change_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc New user notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec new_user(api_terms()) -> api_formatter_return().
new_user(Prop) when is_list(Prop) ->
    case new_user_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?NEW_USER_HEADERS, ?OPTIONAL_NEW_USER_HEADERS);
        'false' -> {'error', "Proplist failed validation for new_user"}
    end;
new_user(JObj) -> new_user(kz_json:to_proplist(JObj)).

-spec new_user_v(api_terms()) -> boolean().
new_user_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?NEW_USER_HEADERS, ?NEW_USER_VALUES, ?NEW_USER_TYPES);
new_user_v(JObj) -> new_user_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Port unconfirmed notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec port_unconfirmed(api_terms()) -> api_formatter_return().
port_unconfirmed(Prop) when is_list(Prop) ->
    case port_unconfirmed_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PORT_UNCONFIRMED_HEADERS, ?OPTIONAL_PORT_UNCONFIRMED_HEADERS);
        'false' -> {'error', "Proplist failed validation for port_unconfirmed"}
    end;
port_unconfirmed(JObj) -> port_unconfirmed(kz_json:to_proplist(JObj)).

-spec port_unconfirmed_v(api_terms()) -> boolean().
port_unconfirmed_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PORT_UNCONFIRMED_HEADERS, ?PORT_UNCONFIRMED_VALUES, ?PORT_UNCONFIRMED_TYPES);
port_unconfirmed_v(JObj) -> port_unconfirmed_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Port request notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec port_request(api_terms()) -> api_formatter_return().
port_request(Prop) when is_list(Prop) ->
    case port_request_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PORT_REQUEST_HEADERS, ?OPTIONAL_PORT_REQUEST_HEADERS);
        'false' -> {'error', "Proplist failed validation for port_request"}
    end;
port_request(JObj) -> port_request(kz_json:to_proplist(JObj)).

-spec port_request_v(api_terms()) -> boolean().
port_request_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PORT_REQUEST_HEADERS, ?PORT_REQUEST_VALUES, ?PORT_REQUEST_TYPES);
port_request_v(JObj) -> port_request_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Port pending notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec port_pending(api_terms()) -> api_formatter_return().
port_pending(Prop) when is_list(Prop) ->
    case port_pending_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PORT_PENDING_HEADERS, ?OPTIONAL_PORT_PENDING_HEADERS);
        'false' -> {'error', "Proplist failed validation for port_pending"}
    end;
port_pending(JObj) -> port_pending(kz_json:to_proplist(JObj)).

-spec port_pending_v(api_terms()) -> boolean().
port_pending_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PORT_PENDING_HEADERS, ?PORT_PENDING_VALUES, ?PORT_PENDING_TYPES);
port_pending_v(JObj) -> port_pending_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Port scheduled notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec port_scheduled(api_terms()) -> api_formatter_return().
port_scheduled(Prop) when is_list(Prop) ->
    case port_scheduled_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PORT_SCHEDULED_HEADERS, ?OPTIONAL_PORT_SCHEDULED_HEADERS);
        'false' -> {'error', "Proplist failed validation for port_scheduled"}
    end;
port_scheduled(JObj) -> port_scheduled(kz_json:to_proplist(JObj)).

-spec port_scheduled_v(api_terms()) -> boolean().
port_scheduled_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PORT_SCHEDULED_HEADERS, ?PORT_SCHEDULED_VALUES, ?PORT_SCHEDULED_TYPES);
port_scheduled_v(JObj) -> port_scheduled_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Port rejected notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec port_rejected(api_terms()) -> api_formatter_return().
port_rejected(Prop) when is_list(Prop) ->
    case port_rejected_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PORT_REJECTED_HEADERS, ?OPTIONAL_PORT_REJECTED_HEADERS);
        'false' -> {'error', "Proplist failed validation for port_rejected"}
    end;
port_rejected(JObj) -> port_rejected(kz_json:to_proplist(JObj)).

-spec port_rejected_v(api_terms()) -> boolean().
port_rejected_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PORT_REJECTED_HEADERS, ?PORT_REJECTED_VALUES, ?PORT_REJECTED_TYPES);
port_rejected_v(JObj) -> port_rejected_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Port cancel notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec port_cancel(api_terms()) -> api_formatter_return().
port_cancel(Prop) when is_list(Prop) ->
    case port_cancel_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PORT_CANCEL_HEADERS, ?OPTIONAL_PORT_CANCEL_HEADERS);
        'false' -> {'error', "Proplist failed validation for port_cancel"}
    end;
port_cancel(JObj) -> port_cancel(kz_json:to_proplist(JObj)).

-spec port_cancel_v(api_terms()) -> boolean().
port_cancel_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PORT_CANCEL_HEADERS, ?PORT_CANCEL_VALUES, ?PORT_CANCEL_TYPES);
port_cancel_v(JObj) -> port_cancel_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Ported request notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec ported(api_terms()) -> api_formatter_return().
ported(Prop) when is_list(Prop) ->
    case ported_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PORTED_HEADERS, ?OPTIONAL_PORTED_HEADERS);
        'false' -> {'error', "Proplist failed validation for ported"}
    end;
ported(JObj) -> ported(kz_json:to_proplist(JObj)).

-spec ported_v(api_terms()) -> boolean().
ported_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PORTED_HEADERS, ?PORTED_VALUES, ?PORTED_TYPES);
ported_v(JObj) -> ported_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Port comment request notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec port_comment(api_terms()) -> api_formatter_return().
port_comment(Prop) when is_list(Prop) ->
    case port_comment_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?PORT_COMMENT_HEADERS, ?OPTIONAL_PORT_COMMENT_HEADERS);
        'false' -> {'error', "Proplist failed validation for port_comment"}
    end;
port_comment(JObj) -> port_comment(kz_json:to_proplist(JObj)).

-spec port_comment_v(api_terms()) -> boolean().
port_comment_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?PORT_COMMENT_HEADERS, ?PORT_COMMENT_VALUES, ?PORT_COMMENT_TYPES);
port_comment_v(JObj) -> port_comment_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Cnam request notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec cnam_request(api_terms()) -> api_formatter_return().
cnam_request(Prop) when is_list(Prop) ->
    case cnam_request_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CNAM_REQUEST_HEADERS, ?OPTIONAL_CNAM_REQUEST_HEADERS);
        'false' -> {'error', "Proplist failed validation for cnam_request"}
    end;
cnam_request(JObj) -> cnam_request(kz_json:to_proplist(JObj)).

-spec cnam_request_v(api_terms()) -> boolean().
cnam_request_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CNAM_REQUEST_HEADERS, ?CNAM_REQUEST_VALUES, ?CNAM_REQUEST_TYPES);
cnam_request_v(JObj) -> cnam_request_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Low Balance notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec low_balance(api_terms()) -> api_formatter_return().
low_balance(Prop) when is_list(Prop) ->
    case low_balance_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?LOW_BALANCE_HEADERS, ?OPTIONAL_LOW_BALANCE_HEADERS);
        'false' -> {'error', "Proplist failed validation for low_balance"}
    end;
low_balance(JObj) -> low_balance(kz_json:to_proplist(JObj)).

-spec low_balance_v(api_terms()) -> boolean().
low_balance_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?LOW_BALANCE_HEADERS, ?LOW_BALANCE_VALUES, ?LOW_BALANCE_TYPES);
low_balance_v(JObj) -> low_balance_v(kz_json:to_proplist(JObj)).


%%--------------------------------------------------------------------
%% @doc Topup notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec topup(api_terms()) -> api_formatter_return().
topup(Prop) when is_list(Prop) ->
    case topup_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?TOPUP_HEADERS, ?OPTIONAL_TOPUP_HEADERS);
        'false' -> {'error', "Proplist failed validation for topup"}
    end;
topup(JObj) -> topup(kz_json:to_proplist(JObj)).

-spec topup_v(api_terms()) -> boolean().
topup_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?TOPUP_HEADERS, ?TOPUP_VALUES, ?TOPUP_TYPES);
topup_v(JObj) -> topup_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc Low Balance notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec transaction(api_terms()) -> api_formatter_return().
transaction(Prop) when is_list(Prop) ->
    case transaction_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?TRANSACTION_HEADERS, ?OPTIONAL_TRANSACTION_HEADERS);
        'false' -> {'error', "Proplist failed validation for transaction"}
    end;
transaction(JObj) -> transaction(kz_json:to_proplist(JObj)).

-spec transaction_v(api_terms()) -> boolean().
transaction_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?TRANSACTION_HEADERS, ?TRANSACTION_VALUES, ?TRANSACTION_TYPES);
transaction_v(JObj) -> transaction_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc New service adition notification for reseller - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec service_added(api_terms()) -> api_formatter_return().
service_added(Prop) when is_list(Prop) ->
    case service_added_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SERVICE_ADDED_HEADERS, ?OPTIONAL_SERVICE_ADDED_HEADERS);
        'false' -> {'error', "Proplist failed validation for service_added"}
    end;
service_added(JObj) -> service_added(kz_json:to_proplist(JObj)).

-spec service_added_v(api_terms()) -> boolean().
service_added_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SERVICE_ADDED_HEADERS, ?SERVICE_ADDED_VALUES, ?SERVICE_ADDED_TYPES);
service_added_v(JObj) -> service_added_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc System alert notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec system_alert(api_terms()) -> api_formatter_return().
system_alert(Prop) when is_list(Prop) ->
    case system_alert_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SYSTEM_ALERT_HEADERS, ?OPTIONAL_SYSTEM_ALERT_HEADERS);
        'false' -> {'error', "Proplist failed validation for system_alert"}
    end;
system_alert(JObj) -> system_alert(kz_json:to_proplist(JObj)).

-spec system_alert_v(api_terms()) -> boolean().
system_alert_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SYSTEM_ALERT_HEADERS, ?SYSTEM_ALERT_VALUES, ?SYSTEM_ALERT_TYPES);
system_alert_v(JObj) -> system_alert_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc webhook notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec webhook(api_terms()) -> api_formatter_return().
webhook(Prop) when is_list(Prop) ->
    case webhook_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?WEBHOOK_HEADERS, ?OPTIONAL_WEBHOOK_HEADERS);
        'false' -> {'error', "Proplist failed validation for webhook"}
    end;
webhook(JObj) -> webhook(kz_json:to_proplist(JObj)).

-spec webhook_v(api_terms()) -> boolean().
webhook_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?WEBHOOK_HEADERS, ?WEBHOOK_VALUES, ?WEBHOOK_TYPES);
webhook_v(JObj) -> webhook_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc webhook notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec webhook_disabled(api_terms()) -> api_formatter_return().
webhook_disabled(Prop) when is_list(Prop) ->
    case webhook_disabled_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?WEBHOOK_DISABLED_HEADERS, ?OPTIONAL_WEBHOOK_DISABLED_HEADERS);
        'false' -> {'error', "Proplist failed validation for webhook_disabled"}
    end;
webhook_disabled(JObj) -> webhook_disabled(kz_json:to_proplist(JObj)).

-spec webhook_disabled_v(api_terms()) -> boolean().
webhook_disabled_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?WEBHOOK_DISABLED_HEADERS, ?WEBHOOK_DISABLED_VALUES, ?WEBHOOK_DISABLED_TYPES);
webhook_disabled_v(JObj) -> webhook_disabled_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc System alert notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec notify_update(api_terms()) -> api_formatter_return().
notify_update(Prop) when is_list(Prop) ->
    case notify_update_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?NOTIFY_UPDATE_HEADERS, ?OPTIONAL_NOTIFY_UPDATE_HEADERS);
        'false' -> {'error', "Proplist failed validation for notify_update"}
    end;
notify_update(JObj) -> notify_update(kz_json:to_proplist(JObj)).

-spec notify_update_v(api_terms()) -> boolean().
notify_update_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?NOTIFY_UPDATE_HEADERS, ?NOTIFY_UPDATE_VALUES, ?NOTIFY_UPDATE_TYPES);
notify_update_v(JObj) -> notify_update_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc denied_emergency_bridge notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec denied_emergency_bridge(api_terms()) -> api_formatter_return().
denied_emergency_bridge(Prop) when is_list(Prop) ->
    case denied_emergency_bridge_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?DENIED_EMERGENCY_BRIDGE_HEADERS, ?OPTIONAL_DENIED_EMERGENCY_BRIDGE_HEADERS);
        'false' -> {'error', "Proplist failed validation for denied_emergency_bridge"}
    end;
denied_emergency_bridge(JObj) -> denied_emergency_bridge(kz_json:to_proplist(JObj)).

-spec denied_emergency_bridge_v(api_terms()) -> boolean().
denied_emergency_bridge_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?DENIED_EMERGENCY_BRIDGE_HEADERS, ?DENIED_EMERGENCY_BRIDGE_VALUES, ?DENIED_EMERGENCY_BRIDGE_TYPES);
denied_emergency_bridge_v(JObj) -> denied_emergency_bridge_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc customer_update notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%-------------------------------------------------------------------
-spec customer_update(api_terms()) -> api_formatter_return().
customer_update(Prop) when is_list(Prop) ->
    case customer_update_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?CUSTOMER_UPDATE_HEADERS, ?OPTIONAL_CUSTOMER_UPDATE_HEADERS);
        'false' -> {'error', "Proplist failed validation for customer_update"}
    end;
customer_update(JObj) -> customer_update(kz_json:to_proplist(JObj)).

-spec customer_update_v(api_terms()) -> boolean().
customer_update_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?CUSTOMER_UPDATE_HEADERS, ?CUSTOMER_UPDATE_VALUES, ?CUSTOMER_UPDATE_TYPES);
customer_update_v(JObj) -> customer_update_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc missed_call notification
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec missed_call(api_terms()) -> api_formatter_return().
missed_call(Prop) when is_list(Prop) ->
    case missed_call_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?MISSED_CALL_HEADERS, ?OPTIONAL_MISSED_CALL_HEADERS);
        'false' -> {'error', "Proplist failed validation for missed_call"}
    end;
missed_call(JObj) -> missed_call(kz_json:to_proplist(JObj)).

-spec missed_call_v(api_terms()) -> boolean().
missed_call_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?MISSED_CALL_HEADERS, ?MISSED_CALL_VALUES, ?MISSED_CALL_TYPES);
missed_call_v(JObj) -> missed_call_v(kz_json:to_proplist(JObj)).

%%--------------------------------------------------------------------
%% @doc skel notification - see wiki
%% Takes proplist, creates JSON string or error
%% @end
%%--------------------------------------------------------------------
-spec skel(api_terms()) -> api_formatter_return().
skel(Prop) when is_list(Prop) ->
    case skel_v(Prop) of
        'true' -> kz_api:build_message(Prop, ?SKEL_HEADERS, ?OPTIONAL_SKEL_HEADERS);
        'false' -> {'error', "Proplist failed validation for skel"}
    end;
skel(JObj) -> skel(kz_json:to_proplist(JObj)).

-spec skel_v(api_terms()) -> boolean().
skel_v(Prop) when is_list(Prop) ->
    kz_api:validate(Prop, ?SKEL_HEADERS, ?SKEL_VALUES, ?SKEL_TYPES);
skel_v(JObj) -> skel_v(kz_json:to_proplist(JObj)).

-type restriction() :: 'new_voicemail' |
                       'voicemail_saved' |
                       'voicemail_full' |
                       'inbound_fax' |
                       'outbound_fax' |
                       'new_fax' |
                       'inbound_fax_error' |
                       'outbound_fax_error' |
                       'outbound_smtp_fax_error' |
                       'fax_error' |
                       'register' |
                       'deregister' |
                       'password_recovery' |
                       'first_occurrence' |
                       'new_account' |
                       'new_user' |
                       'port_unconfirmed' |
                       'port_request' |
                       'port_pending' |
                       'port_scheduled' |
                       'port_cancel' |
                       'port_rejected' |
                       'ported' |
                       'port_comment' |
                       'cnam_requests' |
                       'low_balance' |
                       'topup' |
                       'transaction' |
                       'system_alerts' |
                       'webhook' |
                       'webhook_disabled' |
                       'denied_emergency_bridge' |
                       'customer_update' |
                       'service_added' |
                       'missed_call' |
                       'skel'.
-type restrictions() :: [restriction()].
-type option() :: {'restrict_to', restrictions()}.
-type options() :: [option()].

-spec bind_q(ne_binary(), options()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props)).

-spec bind_to_q(ne_binary(), restrictions() | 'undefined') -> 'ok'.
bind_to_q(Q, 'undefined') ->
    'ok' = amqp_util:bind_q_to_notifications(Q, <<"notifications.*.*">>);
bind_to_q(Q, ['new_voicemail'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_VOICEMAIL_NEW),
    bind_to_q(Q, T);
bind_to_q(Q, ['voicemail_saved'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_VOICEMAIL_SAVED),
    bind_to_q(Q, T);
bind_to_q(Q, ['voicemail_full'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_VOICEMAIL_FULL),
    bind_to_q(Q, T);
bind_to_q(Q, ['inbound_fax'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_FAX_INBOUND),
    bind_to_q(Q, T);
bind_to_q(Q, ['outbound_fax'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_FAX_OUTBOUND),
    bind_to_q(Q, T);
bind_to_q(Q, ['new_fax'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_FAX_INBOUND),
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_FAX_OUTBOUND),
    bind_to_q(Q, T);
bind_to_q(Q, ['inbound_fax_error'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_FAX_INBOUND_ERROR),
    bind_to_q(Q, T);
bind_to_q(Q, ['outbound_fax_error'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_FAX_OUTBOUND_ERROR),
    bind_to_q(Q, T);
bind_to_q(Q, ['outbound_smtp_fax_error'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_FAX_OUTBOUND_SMTP_ERROR),
    bind_to_q(Q, T);
bind_to_q(Q, ['fax_error'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_FAX_INBOUND_ERROR),
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_FAX_OUTBOUND_ERROR),
    bind_to_q(Q, T);
bind_to_q(Q, ['register'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_REGISTER),
    bind_to_q(Q, T);
bind_to_q(Q, ['deregister'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_DEREGISTER),
    bind_to_q(Q, T);
bind_to_q(Q, ['password_recovery'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PASSWORD_RECOVERY),
    bind_to_q(Q, T);
bind_to_q(Q, ['first_occurrence'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_FIRST_OCCURRENCE),
    bind_to_q(Q, T);
bind_to_q(Q, ['new_account'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_NEW_ACCOUNT),
    bind_to_q(Q, T);
bind_to_q(Q, ['account_zone_change'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_ACCOUNT_ZONE_CHANGE),
    bind_to_q(Q, T);
bind_to_q(Q, ['new_user'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_NEW_USER),
    bind_to_q(Q, T);
bind_to_q(Q, ['port_unconfirmed'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PORT_UNCONFIRMED),
    bind_to_q(Q, T);
bind_to_q(Q, ['port_request'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PORT_REQUEST),
    bind_to_q(Q, T);
bind_to_q(Q, ['port_pending'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PORT_PENDING),
    bind_to_q(Q, T);
bind_to_q(Q, ['port_scheduled'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PORT_SCHEDULED),
    bind_to_q(Q, T);
bind_to_q(Q, ['port_rejected'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PORT_REJECTED),
    bind_to_q(Q, T);
bind_to_q(Q, ['port_cancel'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PORT_CANCEL),
    bind_to_q(Q, T);
bind_to_q(Q, ['ported'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PORTED),
    bind_to_q(Q, T);
bind_to_q(Q, ['port_comment'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_PORT_COMMENT),
    bind_to_q(Q, T);
bind_to_q(Q, ['cnam_requests'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_CNAM_REQUEST),
    bind_to_q(Q, T);
bind_to_q(Q, ['low_balance'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_LOW_BALANCE),
    bind_to_q(Q, T);
bind_to_q(Q, ['topup'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_TOPUP),
    bind_to_q(Q, T);
bind_to_q(Q, ['transaction'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_TRANSACTION),
    bind_to_q(Q, T);
bind_to_q(Q, ['system_alerts'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_SYSTEM_ALERT),
    bind_to_q(Q, T);
bind_to_q(Q, ['webhook'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_WEBHOOK_CALLFLOW),
    bind_to_q(Q, T);
bind_to_q(Q, ['webhook_disabled'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_WEBHOOK_DISABLED),
    bind_to_q(Q, T);
bind_to_q(Q, ['denied_emergency_bridge'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_DENIED_EMERGENCY_BRIDGE),
    bind_to_q(Q, T);
bind_to_q(Q, ['customer_update'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_CUSTOMER_UPDATE),
    bind_to_q(Q, T);
bind_to_q(Q, ['missed_call'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_MISSED_CALL),
    bind_to_q(Q, T);
bind_to_q(Q, ['skel'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_SKEL),
    bind_to_q(Q, T);
bind_to_q(Q, ['service_added'|T]) ->
    'ok' = amqp_util:bind_q_to_notifications(Q, ?NOTIFY_SERVICE_ADDED),
    bind_to_q(Q, T);
bind_to_q(_Q, []) ->
    'ok'.

-spec unbind_q(ne_binary(), options()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q_from(Queue, props:get_value('restrict_to', Props)).

-spec unbind_q_from(ne_binary(), restrictions() | 'undefined') -> 'ok'.
unbind_q_from(Q, 'undefined') ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, <<"notifications.*.*">>);
unbind_q_from(Q, ['new_voicemail'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_VOICEMAIL_NEW),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['voicemail_full'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_VOICEMAIL_FULL),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['inbound_fax'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q,?NOTIFY_FAX_INBOUND),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['outbound_fax'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q,?NOTIFY_FAX_OUTBOUND),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['new_fax'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q,?NOTIFY_FAX_INBOUND),
    'ok' = amqp_util:unbind_q_from_notifications(Q,?NOTIFY_FAX_OUTBOUND),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['inbound_fax_error'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q,?NOTIFY_FAX_INBOUND_ERROR),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['outbound_fax_error'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q,?NOTIFY_FAX_OUTBOUND_ERROR),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['outbound_smtp_fax_error'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q,?NOTIFY_FAX_OUTBOUND_SMTP_ERROR),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['fax_error'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q,?NOTIFY_FAX_OUTBOUND_ERROR),
    'ok' = amqp_util:unbind_q_from_notifications(Q,?NOTIFY_FAX_INBOUND_ERROR),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['register'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_REGISTER),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['deregister'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_DEREGISTER),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['password_recovery'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PASSWORD_RECOVERY),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['first_occurrence'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_FIRST_OCCURRENCE),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['new_account'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_NEW_ACCOUNT),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['account_zone_change'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_ACCOUNT_ZONE_CHANGE),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['new_user'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_NEW_USER),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['port_unconfirmed'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PORT_UNCONFIRMED),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['port_request'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PORT_REQUEST),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['port_pending'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PORT_PENDING),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['port_scheduled'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PORT_SCHEDULED),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['port_rejected'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PORT_REJECTED),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['port_cancel'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PORT_CANCEL),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['ported'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PORTED),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['port_comment'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_PORT_COMMENT),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['cnam_request'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_CNAM_REQUEST),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['low_balance'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_LOW_BALANCE),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['topup'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_TOPUP),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['transaction'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_TRANSACTION),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['system_alert'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_SYSTEM_ALERT),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['webhook'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_WEBHOOK_CALLFLOW),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['webhook_disabled'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_WEBHOOK_DISABLED),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['denied_emergency_bridge'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_DENIED_EMERGENCY_BRIDGE),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['customer_update'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_CUSTOMER_UPDATE),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['missed_call'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_MISSED_CALL),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['skel'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_SKEL),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['service_added'|T]) ->
    'ok' = amqp_util:unbind_q_from_notifications(Q, ?NOTIFY_SERVICE_ADDED),
    unbind_q_from(Q, T);
unbind_q_from(_Q, []) ->
    'ok'.

%%--------------------------------------------------------------------
%% @doc
%% declare the exchanges used by this API
%% @end
%%--------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    amqp_util:notifications_exchange().

-spec publish_voicemail_saved(api_terms()) -> 'ok'.
-spec publish_voicemail_saved(api_terms(), ne_binary()) -> 'ok'.
publish_voicemail_saved(JObj) -> publish_voicemail_saved(JObj, ?DEFAULT_CONTENT_TYPE).
publish_voicemail_saved(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?VOICEMAIL_SAVED_VALUES, fun voicemail_saved/1),
    amqp_util:notifications_publish(?NOTIFY_VOICEMAIL_SAVED, Payload, ContentType).

-spec publish_voicemail_new(api_terms()) -> 'ok'.
-spec publish_voicemail_new(api_terms(), ne_binary()) -> 'ok'.
publish_voicemail_new(JObj) -> publish_voicemail_new(JObj, ?DEFAULT_CONTENT_TYPE).
publish_voicemail_new(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?VOICEMAIL_NEW_VALUES, fun voicemail_new/1),
    amqp_util:notifications_publish(?NOTIFY_VOICEMAIL_NEW, Payload, ContentType).

-spec publish_voicemail_full(api_terms()) -> 'ok'.
-spec publish_voicemail_full(api_terms(), ne_binary()) -> 'ok'.
publish_voicemail_full(JObj) -> publish_voicemail_full(JObj, ?DEFAULT_CONTENT_TYPE).
publish_voicemail_full(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?VOICEMAIL_FULL_VALUES, fun voicemail_full/1),
    amqp_util:notifications_publish(?NOTIFY_VOICEMAIL_FULL, Payload, ContentType).

-spec publish_fax_inbound(api_terms()) -> 'ok'.
-spec publish_fax_inbound(api_terms(), ne_binary()) -> 'ok'.
publish_fax_inbound(JObj) -> publish_fax_inbound(JObj, ?DEFAULT_CONTENT_TYPE).
publish_fax_inbound(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API,?FAX_INBOUND_VALUES, fun fax_inbound/1),
    amqp_util:notifications_publish(?NOTIFY_FAX_INBOUND, Payload, ContentType).

-spec publish_fax_outbound(api_terms()) -> 'ok'.
-spec publish_fax_outbound(api_terms(), ne_binary()) -> 'ok'.
publish_fax_outbound(JObj) -> publish_fax_outbound(JObj, ?DEFAULT_CONTENT_TYPE).
publish_fax_outbound(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?FAX_OUTBOUND_VALUES, fun fax_outbound/1),
    amqp_util:notifications_publish(?NOTIFY_FAX_OUTBOUND, Payload, ContentType).

-spec publish_fax_inbound_error(api_terms()) -> 'ok'.
-spec publish_fax_inbound_error(api_terms(), ne_binary()) -> 'ok'.
publish_fax_inbound_error(JObj) -> publish_fax_inbound_error(JObj, ?DEFAULT_CONTENT_TYPE).
publish_fax_inbound_error(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?FAX_INBOUND_ERROR_VALUES, fun fax_inbound_error/1),
    amqp_util:notifications_publish(?NOTIFY_FAX_INBOUND_ERROR, Payload, ContentType).

-spec publish_fax_outbound_error(api_terms()) -> 'ok'.
-spec publish_fax_outbound_error(api_terms(), ne_binary()) -> 'ok'.
publish_fax_outbound_error(JObj) -> publish_fax_outbound_error(JObj, ?DEFAULT_CONTENT_TYPE).
publish_fax_outbound_error(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?FAX_OUTBOUND_ERROR_VALUES, fun fax_outbound_error/1),
    amqp_util:notifications_publish(?NOTIFY_FAX_OUTBOUND_ERROR, Payload, ContentType).

-spec publish_fax_outbound_smtp_error(api_terms()) -> 'ok'.
-spec publish_fax_outbound_smtp_error(api_terms(), ne_binary()) -> 'ok'.
publish_fax_outbound_smtp_error(JObj) -> publish_fax_outbound_smtp_error(JObj, ?DEFAULT_CONTENT_TYPE).
publish_fax_outbound_smtp_error(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?FAX_OUTBOUND_SMTP_ERROR_VALUES, fun fax_outbound_smtp_error/1),
    amqp_util:notifications_publish(?NOTIFY_FAX_OUTBOUND_SMTP_ERROR, Payload, ContentType).

-spec publish_register(api_terms()) -> 'ok'.
-spec publish_register(api_terms(), ne_binary()) -> 'ok'.
publish_register(JObj) -> publish_register(JObj, ?DEFAULT_CONTENT_TYPE).
publish_register(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?REGISTER_VALUES, fun register/1),
    amqp_util:notifications_publish(?NOTIFY_REGISTER, Payload, ContentType).

-spec publish_deregister(api_terms()) -> 'ok'.
-spec publish_deregister(api_terms(), ne_binary()) -> 'ok'.
publish_deregister(JObj) -> publish_deregister(JObj, ?DEFAULT_CONTENT_TYPE).
publish_deregister(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?DEREGISTER_VALUES, fun deregister/1),
    amqp_util:notifications_publish(?NOTIFY_DEREGISTER, Payload, ContentType).

-spec publish_password_recovery(api_terms()) -> 'ok'.
-spec publish_password_recovery(api_terms(), ne_binary()) -> 'ok'.
publish_password_recovery(JObj) -> publish_password_recovery(JObj, ?DEFAULT_CONTENT_TYPE).
publish_password_recovery(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?PASSWORD_RECOVERY_VALUES, fun password_recovery/1),
    amqp_util:notifications_publish(?NOTIFY_PASSWORD_RECOVERY, Payload, ContentType).

-spec publish_first_occurrence(api_terms()) -> 'ok'.
-spec publish_first_occurrence(api_terms(), ne_binary()) -> 'ok'.
publish_first_occurrence(JObj) -> publish_first_occurrence(JObj, ?DEFAULT_CONTENT_TYPE).
publish_first_occurrence(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?FIRST_OCCURRENCE_VALUES, fun first_occurrence/1),
    amqp_util:notifications_publish(?NOTIFY_FIRST_OCCURRENCE, Payload, ContentType).

-spec publish_new_account(api_terms()) -> 'ok'.
-spec publish_new_account(api_terms(), ne_binary()) -> 'ok'.
publish_new_account(JObj) -> publish_new_account(JObj, ?DEFAULT_CONTENT_TYPE).
publish_new_account(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?NEW_ACCOUNT_VALUES, fun new_account/1),
    amqp_util:notifications_publish(?NOTIFY_NEW_ACCOUNT, Payload, ContentType).

-spec publish_account_zone_change(api_terms()) -> 'ok'.
-spec publish_account_zone_change(api_terms(), ne_binary()) -> 'ok'.
publish_account_zone_change(JObj) -> publish_account_zone_change(JObj, ?DEFAULT_CONTENT_TYPE).
publish_account_zone_change(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?ACCOUNT_ZONE_CHANGE_VALUES, fun account_zone_change/1),
    amqp_util:notifications_publish(?NOTIFY_ACCOUNT_ZONE_CHANGE, Payload, ContentType).

-spec publish_new_user(api_terms()) -> 'ok'.
-spec publish_new_user(api_terms(), ne_binary()) -> 'ok'.
publish_new_user(JObj) -> publish_new_user(JObj, ?DEFAULT_CONTENT_TYPE).
publish_new_user(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?NEW_USER_VALUES, fun new_user/1),
    amqp_util:notifications_publish(?NOTIFY_NEW_USER, Payload, ContentType).

-spec publish_port_unconfirmed(api_terms()) -> 'ok'.
-spec publish_port_unconfirmed(api_terms(), ne_binary()) -> 'ok'.
publish_port_unconfirmed(JObj) -> publish_port_unconfirmed(JObj, ?DEFAULT_CONTENT_TYPE).
publish_port_unconfirmed(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?PORT_UNCONFIRMED_VALUES, fun port_unconfirmed/1),
    amqp_util:notifications_publish(?NOTIFY_PORT_UNCONFIRMED, Payload, ContentType).

-spec publish_port_request(api_terms()) -> 'ok'.
-spec publish_port_request(api_terms(), ne_binary()) -> 'ok'.
publish_port_request(JObj) -> publish_port_request(JObj, ?DEFAULT_CONTENT_TYPE).
publish_port_request(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?PORT_REQUEST_VALUES, fun port_request/1),
    amqp_util:notifications_publish(?NOTIFY_PORT_REQUEST, Payload, ContentType).

-spec publish_port_pending(api_terms()) -> 'ok'.
-spec publish_port_pending(api_terms(), ne_binary()) -> 'ok'.
publish_port_pending(JObj) -> publish_port_pending(JObj, ?DEFAULT_CONTENT_TYPE).
publish_port_pending(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?PORT_PENDING_VALUES, fun port_pending/1),
    amqp_util:notifications_publish(?NOTIFY_PORT_PENDING, Payload, ContentType).

-spec publish_port_scheduled(api_terms()) -> 'ok'.
-spec publish_port_scheduled(api_terms(), ne_binary()) -> 'ok'.
publish_port_scheduled(JObj) -> publish_port_scheduled(JObj, ?DEFAULT_CONTENT_TYPE).
publish_port_scheduled(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?PORT_SCHEDULED_VALUES, fun port_scheduled/1),
    amqp_util:notifications_publish(?NOTIFY_PORT_SCHEDULED, Payload, ContentType).

-spec publish_port_rejected(api_terms()) -> 'ok'.
-spec publish_port_rejected(api_terms(), ne_binary()) -> 'ok'.
publish_port_rejected(JObj) -> publish_port_rejected(JObj, ?DEFAULT_CONTENT_TYPE).
publish_port_rejected(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?PORT_REJECTED_VALUES, fun port_rejected/1),
    amqp_util:notifications_publish(?NOTIFY_PORT_REJECTED, Payload, ContentType).

-spec publish_port_cancel(api_terms()) -> 'ok'.
-spec publish_port_cancel(api_terms(), ne_binary()) -> 'ok'.
publish_port_cancel(JObj) -> publish_port_cancel(JObj, ?DEFAULT_CONTENT_TYPE).
publish_port_cancel(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?PORT_CANCEL_VALUES, fun port_cancel/1),
    amqp_util:notifications_publish(?NOTIFY_PORT_CANCEL, Payload, ContentType).

-spec publish_ported(api_terms()) -> 'ok'.
-spec publish_ported(api_terms(), ne_binary()) -> 'ok'.
publish_ported(JObj) -> publish_ported(JObj, ?DEFAULT_CONTENT_TYPE).
publish_ported(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?PORTED_VALUES, fun ported/1),
    amqp_util:notifications_publish(?NOTIFY_PORTED, Payload, ContentType).

-spec publish_port_comment(api_terms()) -> 'ok'.
-spec publish_port_comment(api_terms(), ne_binary()) -> 'ok'.
publish_port_comment(JObj) -> publish_port_comment(JObj, ?DEFAULT_CONTENT_TYPE).
publish_port_comment(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?PORT_COMMENT_VALUES, fun port_comment/1),
    amqp_util:notifications_publish(?NOTIFY_PORT_COMMENT, Payload, ContentType).

-spec publish_cnam_request(api_terms()) -> 'ok'.
-spec publish_cnam_request(api_terms(), ne_binary()) -> 'ok'.
publish_cnam_request(JObj) -> publish_cnam_request(JObj, ?DEFAULT_CONTENT_TYPE).
publish_cnam_request(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?CNAM_REQUEST_VALUES, fun cnam_request/1),
    amqp_util:notifications_publish(?NOTIFY_CNAM_REQUEST, Payload, ContentType).

-spec publish_low_balance(api_terms()) -> 'ok'.
-spec publish_low_balance(api_terms(), ne_binary()) -> 'ok'.
publish_low_balance(JObj) -> publish_low_balance(JObj, ?DEFAULT_CONTENT_TYPE).
publish_low_balance(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?LOW_BALANCE_VALUES, fun low_balance/1),
    amqp_util:notifications_publish(?NOTIFY_LOW_BALANCE, Payload, ContentType).

-spec publish_topup(api_terms()) -> 'ok'.
-spec publish_topup(api_terms(), ne_binary()) -> 'ok'.
publish_topup(JObj) -> publish_topup(JObj, ?DEFAULT_CONTENT_TYPE).
publish_topup(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?TOPUP_VALUES, fun topup/1),
    amqp_util:notifications_publish(?NOTIFY_TOPUP, Payload, ContentType).

-spec publish_transaction(api_terms()) -> 'ok'.
-spec publish_transaction(api_terms(), ne_binary()) -> 'ok'.
publish_transaction(JObj) -> publish_transaction(JObj, ?DEFAULT_CONTENT_TYPE).
publish_transaction(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?TRANSACTION_VALUES, fun transaction/1),
    amqp_util:notifications_publish(?NOTIFY_TRANSACTION, Payload, ContentType).

-spec publish_service_added(api_terms()) -> 'ok'.
-spec publish_service_added(api_terms(), ne_binary()) -> 'ok'.
publish_service_added(JObj) -> publish_service_added(JObj, ?DEFAULT_CONTENT_TYPE).
publish_service_added(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?SERVICE_ADDED_VALUES, fun service_added/1),
    amqp_util:notifications_publish(?NOTIFY_SERVICE_ADDED, Payload, ContentType).

-spec publish_system_alert(api_terms()) -> 'ok'.
-spec publish_system_alert(api_terms(), ne_binary()) -> 'ok'.
publish_system_alert(JObj) -> publish_system_alert(JObj, ?DEFAULT_CONTENT_TYPE).
publish_system_alert(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?SYSTEM_ALERT_VALUES, fun system_alert/1),
    amqp_util:notifications_publish(?NOTIFY_SYSTEM_ALERT, Payload, ContentType).

-spec publish_webhook(api_terms()) -> 'ok'.
-spec publish_webhook(api_terms(), ne_binary()) -> 'ok'.
publish_webhook(JObj) -> publish_webhook(JObj, ?DEFAULT_CONTENT_TYPE).
publish_webhook(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?WEBHOOK_VALUES, fun webhook/1),
    amqp_util:notifications_publish(?NOTIFY_WEBHOOK_CALLFLOW, Payload, ContentType).

-spec publish_webhook_disabled(api_terms()) -> 'ok'.
-spec publish_webhook_disabled(api_terms(), ne_binary()) -> 'ok'.
publish_webhook_disabled(JObj) -> publish_webhook_disabled(JObj, ?DEFAULT_CONTENT_TYPE).
publish_webhook_disabled(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?WEBHOOK_DISABLED_VALUES, fun webhook_disabled/1),
    amqp_util:notifications_publish(?NOTIFY_WEBHOOK_DISABLED, Payload, ContentType).

-spec publish_notify_update(ne_binary(), api_terms()) -> 'ok'.
-spec publish_notify_update(ne_binary(), api_terms(), ne_binary()) -> 'ok'.
publish_notify_update(RespQ, JObj) -> publish_notify_update(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).
publish_notify_update(RespQ, API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?NOTIFY_UPDATE_VALUES, fun notify_update/1),
    amqp_util:targeted_publish(RespQ, Payload, ContentType).

-spec publish_denied_emergency_bridge(api_terms()) -> 'ok'.
-spec publish_denied_emergency_bridge(api_terms(), ne_binary()) -> 'ok'.
publish_denied_emergency_bridge(JObj) -> publish_denied_emergency_bridge(JObj, ?DEFAULT_CONTENT_TYPE).
publish_denied_emergency_bridge(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?DENIED_EMERGENCY_BRIDGE_VALUES, fun denied_emergency_bridge/1),
    amqp_util:notifications_publish(?NOTIFY_DENIED_EMERGENCY_BRIDGE, Payload, ContentType).

-spec publish_customer_update(api_terms()) -> 'ok'.
-spec publish_customer_update(api_terms(), ne_binary()) -> 'ok'.
publish_customer_update(JObj) -> publish_customer_update(JObj, ?DEFAULT_CONTENT_TYPE).
publish_customer_update(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?CUSTOMER_UPDATE_VALUES, fun customer_update/1),
    amqp_util:notifications_publish(?NOTIFY_CUSTOMER_UPDATE, Payload, ContentType).

-spec publish_missed_call(api_terms()) -> 'ok'.
-spec publish_missed_call(api_terms(), ne_binary()) -> 'ok'.
publish_missed_call(JObj) -> publish_missed_call(JObj, ?DEFAULT_CONTENT_TYPE).
publish_missed_call(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?MISSED_CALL_VALUES, fun missed_call/1),
    amqp_util:notifications_publish(?NOTIFY_MISSED_CALL, Payload, ContentType).

-spec publish_skel(api_terms()) -> 'ok'.
-spec publish_skel(api_terms(), ne_binary()) -> 'ok'.
publish_skel(JObj) -> publish_skel(JObj, ?DEFAULT_CONTENT_TYPE).
publish_skel(API, ContentType) ->
    {'ok', Payload} = kz_api:prepare_api_payload(API, ?SKEL_VALUES, fun skel/1),
    amqp_util:notifications_publish(?NOTIFY_SKEL, Payload, ContentType).
