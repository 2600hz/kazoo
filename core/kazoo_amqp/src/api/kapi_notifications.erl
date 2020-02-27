%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Notification messages, like voicemail left.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @author Hesaam Farhang
%%% @end
%%%-----------------------------------------------------------------------------
-module(kapi_notifications).

-export([bind_q/2, unbind_q/2]).
-export([declare_exchanges/0]).

-export([api_definitions/0, api_definition/1]).

-export([headers/1
        ,account_id/1, account_db/2
        ]).

-export([%% Account notifications
         account_zone_change/1, account_zone_change_v/1
        ,bill_reminder/1, bill_reminder_v/1
        ,low_balance/1, low_balance_v/1
        ,new_account/1, new_account_v/1
        ,service_added/1, service_added_v/1
        ,topup/1, topup_v/1
        ,transaction/1, transaction_v/1

         %% Fax notifications
        ,fax_inbound/1, fax_inbound_v/1
        ,fax_inbound_error/1, fax_inbound_error_v/1
        ,fax_outbound/1, fax_outbound_v/1
        ,fax_outbound_error/1, fax_outbound_error_v/1
        ,fax_outbound_smtp_error/1, fax_outbound_smtp_error_v/1

         %% Number and Port notifications
        ,cnam_request/1, cnam_request_v/1
        ,port_cancel/1, port_cancel_v/1
        ,port_comment/1, port_comment_v/1
        ,port_pending/1, port_pending_v/1
        ,port_rejected/1, port_rejected_v/1
        ,port_request/1, port_request_v/1
        ,port_scheduled/1, port_scheduled_v/1
        ,port_unconfirmed/1, port_unconfirmed_v/1
        ,ported/1, ported_v/1

         %% Register notifications
        ,denied_emergency_bridge/1, denied_emergency_bridge_v/1

         %% SIP notifications
        ,deregister/1, deregister_v/1
        ,first_occurrence/1, first_occurrence_v/1
        ,missed_call/1, missed_call_v/1
        ,register/1, register_v/1

         %% System notifications
        ,system_alert/1, system_alert_v/1

         %% User notifications
        ,customer_update/1, customer_update_v/1
        ,new_user/1, new_user_v/1
        ,password_recovery/1, password_recovery_v/1

         %% Voicemail notifications
        ,voicemail_full/1, voicemail_full_v/1
        ,voicemail_new/1, voicemail_new_v/1
        ,voicemail_saved/1, voicemail_saved_v/1
        ,voicemail_deleted/1, voicemail_deleted_v/1

         %% Webhook notifications
        ,webhook/1, webhook_v/1
        ,webhook_disabled/1, webhook_disabled_v/1

         %% published on completion of notification
        ,notify_update/1, notify_update_v/1

         %% skeleton notification
        ,skel/1, skel_v/1

         %% number_feature_manual_action
        ,number_feature_manual_action/1, number_feature_manual_action_v/1
        ]).

-export([%% Account notifications
         publish_account_zone_change/1, publish_account_zone_change/2
        ,publish_bill_reminder/1, publish_bill_reminder/2
        ,publish_low_balance/1, publish_low_balance/2
        ,publish_new_account/1, publish_new_account/2
        ,publish_service_added/1, publish_service_added/2
        ,publish_topup/1, publish_topup/2
        ,publish_transaction/1, publish_transaction/2

         %% Fax notifications
        ,publish_fax_inbound/1, publish_fax_inbound/2
        ,publish_fax_inbound_error/1, publish_fax_inbound_error/2
        ,publish_fax_outbound/1, publish_fax_outbound/2
        ,publish_fax_outbound_error/1, publish_fax_outbound_error/2
        ,publish_fax_outbound_smtp_error/1, publish_fax_outbound_smtp_error/2

         %% Number and Port notifications
        ,publish_cnam_request/1, publish_cnam_request/2
        ,publish_port_cancel/1, publish_port_cancel/2
        ,publish_port_comment/1, publish_port_comment/2
        ,publish_port_pending/1, publish_port_pending/2
        ,publish_port_rejected/1, publish_port_rejected/2
        ,publish_port_request/1, publish_port_request/2
        ,publish_port_scheduled/1, publish_port_scheduled/2
        ,publish_port_unconfirmed/1, publish_port_unconfirmed/2
        ,publish_ported/1, publish_ported/2

         %% Register notifications
        ,publish_denied_emergency_bridge/1, publish_denied_emergency_bridge/2

         %% SIP notifications
        ,publish_deregister/1, publish_deregister/2
        ,publish_first_occurrence/1, publish_first_occurrence/2
        ,publish_missed_call/1, publish_missed_call/2
        ,publish_register/1, publish_register/2

         %% System notifications
        ,publish_system_alert/1, publish_system_alert/2

         %% User notifications
        ,publish_customer_update/1, publish_customer_update/2
        ,publish_new_user/1, publish_new_user/2
        ,publish_password_recovery/1, publish_password_recovery/2

         %% Voicemail notifications
        ,publish_voicemail_full/1, publish_voicemail_full/2
        ,publish_voicemail_new/1, publish_voicemail_new/2
        ,publish_voicemail_saved/1, publish_voicemail_saved/2
        ,publish_voicemail_deleted/1, publish_voicemail_deleted/2

         %% Webhook notifications
        ,publish_webhook/1, publish_webhook/2
        ,publish_webhook_disabled/1, publish_webhook_disabled/2

         %% published on completion of notification
        ,publish_notify_update/2, publish_notify_update/3

         %% skeleton notification
        ,publish_skel/1, publish_skel/2

         %% number_feature_manual_action
        ,publish_number_feature_manual_action/1, publish_number_feature_manual_action/2
        ]).

-include_lib("kz_amqp_util.hrl").

-define(DEFAULT_OPTIONAL_HEADERS, [<<"To">>, <<"Cc">>, <<"Bcc">>
                                  ,<<"From">>, <<"Reply-To">>
                                  ,<<"Subject">>, <<"HTML">>, <<"Text">>
                                  ,<<"Account-ID">>, <<"Account-DB">>
                                  ,<<"Preview">>, <<"Attachment-URL">>
                                  ]).

-define(BINDING_STRING(Category, Name), <<"notifications.", (Category)/binary, ".", (Name)/binary>>).

-define(NOTIFY_VALUES(Name), [{<<"Event-Category">>, <<"notification">>}
                             ,{<<"Event-Name">>, Name}
                             ]).


%%%=============================================================================
%%% Internal Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Notify Update Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec notify_update_definition() -> kapi_definition:api().
notify_update_definition() ->
    #kapi_definition{name = <<"notify_update">>
                    ,friendly_name = <<"Notify Status Update">>
                    ,description = <<"This event is triggered by notification consumer, e.g. teletype application, to send status of notification being process to publisher">>
                    ,build_fun = fun notify_update/1
                    ,validate_fun = fun notify_update_v/1
                    ,publish_fun = fun publish_notify_update/2
                    ,required_headers = [<<"Status">>]
                    ,optional_headers = [<<"Failure-Message">>
                                        ,<<"Metadata">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = [{<<"Status">>, [<<"completed">>
                                              ,<<"disabled">>
                                              ,<<"failed">>
                                              ,<<"ignored">>
                                              ,<<"pending">>
                                              ]}
                               | ?NOTIFY_VALUES(<<"notify_update">>)
                              ]
                    ,types = []
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Skeleton API Notification definition.
%% @end
%%------------------------------------------------------------------------------
-spec skel_definition() -> kapi_definition:api().
skel_definition() ->
    #kapi_definition{name = <<"skel">>
                    ,friendly_name = <<"Example Notification">>
                    ,description = <<"An example notification, this event should never be triggered">>
                    ,build_fun = fun skel/1
                    ,validate_fun = fun skel_v/1
                    ,publish_fun = fun publish_skel/1
                    ,binding = ?BINDING_STRING(<<"account">>, <<"skel">>)
                    ,restrict_to = 'skel'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"User-ID">>
                                        ]
                    ,optional_headers = ?DEFAULT_OPTIONAL_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"skel">>)
                    ,types = []
                    }.


%%%=============================================================================
%%% Account Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Account Zone Notification Change API definition.
%% @end
%%------------------------------------------------------------------------------
-spec account_zone_change_definition() -> kapi_definition:api().
account_zone_change_definition() ->
    #kapi_definition{name = <<"account_zone_change">>
                    ,friendly_name = <<"Account Zone Change">>
                    ,description = <<"This event is triggered when an end user requests the home zone of an account is changed">>
                    ,build_fun = fun account_zone_change/1
                    ,validate_fun = fun account_zone_change_v/1
                    ,publish_fun = fun publish_account_zone_change/1
                    ,binding = ?BINDING_STRING(<<"account">>, <<"zone_change">>)
                    ,restrict_to = 'account_zone_change'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Zones">>
                                        ]
                    ,optional_headers = ?DEFAULT_OPTIONAL_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"account_zone_change">>)
                    ,types = []
                    }.

%%------------------------------------------------------------------------------
%% @doc Bill Reminder Notification (service invoices) API definition.
%% @end
%%------------------------------------------------------------------------------
-spec bill_reminder_definition() -> kapi_definition:api().
bill_reminder_definition() ->
    #kapi_definition{name = <<"bill_reminder">>
                    ,friendly_name = <<"Bill Reminder">>
                    ,description = <<"This event is triggered before a few days before the end of the month to"
                                     "remind account's owners of estimated service plan charges"
                                   >>
                    ,build_fun = fun bill_reminder/1
                    ,validate_fun = fun bill_reminder_v/1
                    ,publish_fun = fun publish_bill_reminder/1
                    ,binding = ?BINDING_STRING(<<"account">>, <<"reminder">>)
                    ,restrict_to = 'bill_reminder'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Due-Date">>
                                        ,<<"Items">>
                                        ,<<"Timestamp">>
                                        ]
                    ,optional_headers = [<<"Payment-Token">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"bill_reminder">>)
                    ,types = [{<<"Account-ID">>, fun kz_term:is_ne_binary/1}
                             ,{<<"Due-Date">>, fun kz_term:is_pos_integer/1}
                             ,{<<"Items">>, fun kz_json:are_json_objects/1}
                             ,{<<"Payment-Token">>, fun kz_json:is_json_object/1}
                             ,{<<"Timestamp">>, fun kz_term:is_pos_integer/1}
                             ]
                    }.

%%------------------------------------------------------------------------------
%% @doc Get Low Balance Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec low_balance_definition() -> kapi_definition:api().
low_balance_definition() ->
    #kapi_definition{name = <<"low_balance">>
                    ,friendly_name = <<"Account Low Balance">>
                    ,description = <<"This event is triggered when an account is found with a balance below the notification threshold">>
                    ,build_fun = fun low_balance/1
                    ,validate_fun = fun low_balance_v/1
                    ,publish_fun = fun publish_low_balance/1
                    ,binding = ?BINDING_STRING(<<"account">>, <<"low_balance">>)
                    ,restrict_to = 'low_balance'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Current-Balance">>
                                        ]
                    ,optional_headers = ?DEFAULT_OPTIONAL_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"low_balance">>)
                    ,types = []
                    }.
%%------------------------------------------------------------------------------
%% @doc Get New Account Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec new_account_definition() -> kapi_definition:api().
new_account_definition() ->
    #kapi_definition{name = <<"new_account">>
                    ,friendly_name = <<"New Account">>
                    ,description = <<"This event is triggered when an end user creates a new account">>
                    ,build_fun = fun new_account/1
                    ,validate_fun = fun new_account_v/1
                    ,publish_fun = fun publish_new_account/1
                    ,binding = ?BINDING_STRING(<<"account">>, <<"new">>)
                    ,restrict_to = 'new_account'
                    ,required_headers = [<<"Account-ID">>]
                    ,optional_headers = [<<"Account-API-Key">>
                                        ,<<"Account-DB">>
                                        ,<<"Account-Name">>
                                        ,<<"Account-Realm">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"new_account">>)
                    ,types = []
                    }.
%%------------------------------------------------------------------------------
%% @doc Get New Service Notification Addition (from service audit log) API definition.
%% @end
%%------------------------------------------------------------------------------
-spec service_added_definition() -> kapi_definition:api().
service_added_definition() ->
    #kapi_definition{name = <<"service_added">>
                    ,friendly_name = <<"Service Added">>
                    ,description = <<"This event is triggered when an account's billable quantities change">>
                    ,build_fun = fun service_added/1
                    ,validate_fun = fun service_added_v/1
                    ,publish_fun = fun publish_service_added/1
                    ,binding = ?BINDING_STRING(<<"account">>, <<"service_added">>)
                    ,restrict_to = 'service_added'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Audit-Log">>
                                        ,<<"Items">>
                                        ,<<"Timestamp">>
                                        ]
                    ,optional_headers = ?DEFAULT_OPTIONAL_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"service_added">>)
                    ,types = [{<<"Items">>, fun kz_json:are_json_objects/1}]
                    }.

%%% Transaction and Top-up common optional headers
-define(COMMON_TRANSACTION_HEADERS, [<<"Add-Ons">>
                                    ,<<"Billing-Address">>
                                    ,<<"Card-Last-Four">>
                                    ,<<"Currency-Code">>
                                    ,<<"Discounts">>
                                    ,<<"ID">>
                                    ,<<"Purchase-Order">>
                                    ,<<"Tax-Amount">>
                                         | ?DEFAULT_OPTIONAL_HEADERS
                                    ]).

%%-----------------------------------------------------------------------------%% Top-up
%% @doc Get same headers Notification for top-up and transaction API definition.
%% @end
%%------------------------------------------------------------------------------
-spec topup_definition() -> kapi_definition:api().
topup_definition() ->
    #kapi_definition{name = <<"topup">>
                    ,friendly_name = <<"Automatic Account Top-up">>
                    ,description = <<"This event is triggered when an account automatic top-up is attempted">>
                    ,build_fun = fun topup/1
                    ,validate_fun = fun topup_v/1
                    ,publish_fun = fun publish_topup/1
                    ,binding = ?BINDING_STRING(<<"account">>, <<"topup">>)
                    ,restrict_to = 'topup'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Amount">>
                                        ,<<"Response">>
                                        ,<<"Success">>
                                        ,<<"Timestamp">>
                                        ]
                    ,optional_headers = ?COMMON_TRANSACTION_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"topup">>)
                    ,types = []
                    }.

%%-----------------------------------------------------------------------------%% Transaction
%% @doc Get same headers Notification for top-up and transaction API definition.
%% @end
%%------------------------------------------------------------------------------
-spec transaction_definition() -> kapi_definition:api().
transaction_definition() ->
    #kapi_definition{name = <<"transaction">>
                    ,friendly_name = <<"Transaction Completed">>
                    ,description = <<"This event is triggered when a transaction is attempted">>
                    ,build_fun = fun transaction/1
                    ,validate_fun = fun transaction_v/1
                    ,publish_fun = fun publish_transaction/1
                    ,binding = ?BINDING_STRING(<<"account">>, <<"transaction">>)
                    ,restrict_to = 'transaction'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Amount">>
                                        ,<<"Response">>
                                        ,<<"Success">>
                                        ,<<"Timestamp">>
                                        ]
                    ,optional_headers = [<<"Service-Plan">> | ?COMMON_TRANSACTION_HEADERS]
                    ,values = ?NOTIFY_VALUES(<<"transaction">>)
                    ,types = []
                    }.


%%%=============================================================================
%%% Fax Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Fax Inbound Notification (New Inbound Fax) API definition.
%% @end
%%------------------------------------------------------------------------------
-spec inbound_fax_definition() -> kapi_definition:api().
inbound_fax_definition() ->
    #kapi_definition{name = <<"inbound_fax">>
                    ,friendly_name = <<"Successful Fax Reception">>
                    ,description = <<"This event is triggered when a fax is successfully received">>
                    ,build_fun = fun fax_inbound/1
                    ,validate_fun = fun fax_inbound_v/1
                    ,publish_fun = fun publish_fax_inbound/1
                    ,binding = ?BINDING_STRING(<<"fax">>, <<"inbound">>)
                    ,restrict_to = 'inbound_fax'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Fax-ID">>
                                        ,<<"From-Realm">>
                                        ,<<"From-User">>
                                        ,<<"To-Realm">>
                                        ,<<"To-User">>
                                        ]
                    ,optional_headers = [<<"Call-ID">>
                                        ,<<"Callee-ID-Name">>
                                        ,<<"Callee-ID-Number">>
                                        ,<<"Caller-ID-Name">>
                                        ,<<"Caller-ID-Number">>
                                        ,<<"Fax-Info">>
                                        ,<<"Fax-Notifications">>
                                        ,<<"Fax-Timestamp">>
                                        ,<<"FaxBox-ID">>
                                        ,<<"Owner-ID">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"inbound_fax">>)
                    ,types = []
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Fax Inbound Notification Error API definition.
%% @end
%%------------------------------------------------------------------------------
-spec inbound_fax_error_definition() -> kapi_definition:api().
inbound_fax_error_definition() ->
    #kapi_definition{name = <<"inbound_fax_error">>
                    ,friendly_name = <<"Fax Reception Error">>
                    ,description = <<"This event is triggered when receiving a fax fails">>
                    ,build_fun = fun fax_inbound_error/1
                    ,validate_fun = fun fax_inbound_error_v/1
                    ,publish_fun = fun publish_fax_inbound_error/1
                    ,binding = ?BINDING_STRING(<<"fax">>, <<"inbound_error">>)
                    ,restrict_to = 'inbound_fax_error'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"From-Realm">>
                                        ,<<"From-User">>
                                        ,<<"To-Realm">>
                                        ,<<"To-User">>
                                        ]
                    ,optional_headers = [<<"Call-ID">>
                                        ,<<"Callee-ID-Name">>
                                        ,<<"Callee-ID-Number">>
                                        ,<<"Caller-ID-Name">>
                                        ,<<"Caller-ID-Number">>
                                        ,<<"Fax-Error">>
                                        ,<<"Fax-Info">>
                                        ,<<"Fax-ID">>
                                        ,<<"Fax-Notifications">>
                                        ,<<"Fax-Result-Code">>
                                        ,<<"Fax-Timestamp">>
                                        ,<<"FaxBox-ID">>
                                        ,<<"Owner-ID">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"inbound_fax_error">>)
                    ,types = []
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Fax Outbound Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec outbound_fax_definition() -> kapi_definition:api().
outbound_fax_definition() ->
    #kapi_definition{name = <<"outbound_fax">>
                    ,friendly_name = <<"Successful Fax Transmission">>
                    ,description = <<"This event is triggered when a fax is successfully transmitted">>
                    ,build_fun = fun fax_outbound/1
                    ,validate_fun = fun fax_outbound_v/1
                    ,publish_fun = fun publish_fax_outbound/1
                    ,binding = ?BINDING_STRING(<<"fax">>, <<"outbound">>)
                    ,restrict_to = 'outbound_fax'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Callee-ID-Number">>
                                        ,<<"Caller-ID-Number">>
                                        ,<<"Fax-ID">>
                                        ,<<"Fax-JobId">>
                                        ]
                    ,optional_headers = [<<"Call-ID">>
                                        ,<<"Callee-ID-Name">>
                                        ,<<"Caller-ID-Name">>
                                        ,<<"Fax-Info">>
                                        ,<<"Fax-Notifications">>
                                        ,<<"Fax-Timestamp">>
                                        ,<<"FaxBox-ID">>
                                        ,<<"Owner-ID">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"outbound_fax">>)
                    ,types = []
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Fax Outbound Notification Error API definition.
%% @end
%%------------------------------------------------------------------------------
-spec outbound_fax_error_definition() -> kapi_definition:api().
outbound_fax_error_definition() ->
    #kapi_definition{name = <<"outbound_fax_error">>
                    ,friendly_name = <<"Fax Transmission Error">>
                    ,description = <<"This event is triggered when transmitting a fax fails">>
                    ,build_fun = fun fax_outbound_error/1
                    ,validate_fun = fun fax_outbound_error_v/1
                    ,publish_fun = fun publish_fax_outbound_error/1
                    ,binding = ?BINDING_STRING(<<"fax">>, <<"outbound_error">>)
                    ,restrict_to = 'outbound_fax_error'
                    ,required_headers = [<<"Fax-ID">>
                                        ,<<"Fax-JobId">>
                                        ]
                    ,optional_headers = [<<"Call-ID">>
                                        ,<<"Callee-ID-Name">>
                                        ,<<"Callee-ID-Number">>
                                        ,<<"Caller-ID-Name">>
                                        ,<<"Caller-ID-Number">>
                                        ,<<"Fax-Error">>
                                        ,<<"Fax-Info">>
                                        ,<<"Fax-Notifications">>
                                        ,<<"Fax-Timestamp">>
                                        ,<<"FaxBox-ID">>
                                        ,<<"Owner-ID">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"outbound_fax_error">>)
                    ,types = []
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Fax Outbound Notification SMTP Error API definition.
%% @end
%%------------------------------------------------------------------------------
-spec outbound_smtp_fax_error_definition() -> kapi_definition:api().
outbound_smtp_fax_error_definition() ->
    #kapi_definition{name = <<"outbound_smtp_fax_error">>
                    ,friendly_name = <<"Invalid Email-to-Fax Email">>
                    ,description = <<"This event is triggered when the received email-to-fax email is invalid">>
                    ,build_fun = fun fax_outbound_smtp_error/1
                    ,validate_fun = fun fax_outbound_smtp_error_v/1
                    ,publish_fun = fun publish_fax_outbound_smtp_error/1
                    ,binding = ?BINDING_STRING(<<"fax">>, <<"outbound_smtp_error">>)
                    ,restrict_to = 'outbound_smtp_fax_error'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Fax-From-Email">>
                                        ,<<"Errors">>
                                        ,<<"Timestamp">>
                                        ]
                    ,optional_headers = [<<"Fax-To-Email">>
                                        ,<<"FaxBox-ID">>
                                        ,<<"FaxBox-Name">>
                                        ,<<"FaxBox-Timezone">>
                                        ,<<"Number">>
                                        ,<<"Owner-ID">>
                                        ,<<"Original-Number">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"outbound_smtp_fax_error">>)
                    ,types = [{<<"Errors">>, fun(L) when is_list(L) -> kz_term:is_not_empty(L);
                                                (_) -> 'false'
                                             end
                              }
                             ]
                    }.

%%%=============================================================================
%%% Number and Port Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Cnam Request Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec cnam_request_definition() -> kapi_definition:api().
cnam_request_definition() ->
    #kapi_definition{name = <<"cnam_request">>
                    ,friendly_name = <<"CNAM Update">>
                    ,description = <<"This event is triggered when an end user would like the CNAM for a number changed">>
                    ,build_fun = fun cnam_request/1
                    ,validate_fun = fun cnam_request_v/1
                    ,publish_fun = fun publish_cnam_request/1
                    ,binding = ?BINDING_STRING(<<"number">>, <<"cnam_request">>)
                    ,restrict_to = 'cnam_request'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Number">>
                                        ,<<"Cnam">>
                                        ]
                    ,optional_headers = [<<"Acquired-For">>
                                        ,<<"Local-Number">>
                                        ,<<"Number-State">>
                                        ,<<"Request">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"cnam_request">>)
                    ,types = []
                    }.

-define(PORT_OPTIONAL_HEADERS, [<<"Authorized-By">>
                               ,<<"Local-Number">>
                               ,<<"Number">>
                               ,<<"Number-State">>
                               ,<<"Port">>
                               ,<<"Reason">>
                               ,<<"Version">> %% for stupid notify app (port_request)
                                    | ?DEFAULT_OPTIONAL_HEADERS
                               ]).
%%------------------------------------------------------------------------------
%% @doc Get Port Cancel Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec port_cancel_definition() -> kapi_definition:api().
port_cancel_definition() ->
    #kapi_definition{name = <<"port_cancel">>
                    ,friendly_name = <<"Port Cancel">>
                    ,description = <<"This event is triggered when a port request is canceled">>
                    ,build_fun = fun port_cancel/1
                    ,validate_fun = fun port_cancel_v/1
                    ,publish_fun = fun publish_port_cancel/1
                    ,binding = ?BINDING_STRING(<<"number">>, <<"port_cancel">>)
                    ,restrict_to = 'port_cancel'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Port-Request-ID">>
                                        ]
                    ,optional_headers = ?PORT_OPTIONAL_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"port_cancel">>)
                    ,types = [{<<"Reason">>, fun kz_json:is_json_object/1}]
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Port Comment Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec port_comment_definition() -> kapi_definition:api().
port_comment_definition() ->
    #kapi_definition{name = <<"port_comment">>
                    ,friendly_name = <<"Port Comment">>
                    ,description = <<"This event is triggered when a comment is left on a port request">>
                    ,build_fun = fun port_comment/1
                    ,validate_fun = fun port_comment_v/1
                    ,publish_fun = fun publish_port_comment/1
                    ,binding = ?BINDING_STRING(<<"number">>, <<"port_comment">>)
                    ,restrict_to = 'port_comment'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Port-Request-ID">>
                                        ,<<"Comment">>
                                        ]
                    ,optional_headers = ?PORT_OPTIONAL_HEADERS -- [<<"Reason">>]
                    ,values = ?NOTIFY_VALUES(<<"port_comment">>)
                    ,types = []
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Port Pending Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec port_pending_definition() -> kapi_definition:api().
port_pending_definition() ->
    #kapi_definition{name = <<"port_pending">>
                    ,friendly_name = <<"Port Pending">>
                    ,description = <<"This event is triggered when a port request is accepted and submitted to a carrier">>
                    ,build_fun = fun port_pending/1
                    ,validate_fun = fun port_pending_v/1
                    ,publish_fun = fun publish_port_pending/1
                    ,binding = ?BINDING_STRING(<<"number">>, <<"port_pending">>)
                    ,restrict_to = 'port_pending'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Port-Request-ID">>
                                        ]
                    ,optional_headers = ?PORT_OPTIONAL_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"port_pending">>)
                    ,types = [{<<"Reason">>, fun kz_json:is_json_object/1}]
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Port Rejected Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec port_rejected_definition() -> kapi_definition:api().
port_rejected_definition() ->
    #kapi_definition{name = <<"port_rejected">>
                    ,friendly_name = <<"Port Rejected">>
                    ,description = <<"This event is triggered when a port request is rejected">>
                    ,build_fun = fun port_rejected/1
                    ,validate_fun = fun port_rejected_v/1
                    ,publish_fun = fun publish_port_rejected/1
                    ,binding = ?BINDING_STRING(<<"number">>, <<"port_rejected">>)
                    ,restrict_to = 'port_rejected'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Port-Request-ID">>
                                        ]
                    ,optional_headers = ?PORT_OPTIONAL_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"port_rejected">>)
                    ,types = [{<<"Reason">>, fun kz_json:is_json_object/1}]
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Port Request Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec port_request_definition() -> kapi_definition:api().
port_request_definition() ->
    #kapi_definition{name = <<"port_request">>
                    ,friendly_name = <<"Port Request">>
                    ,description = <<"This event is triggered when a port is submitted for processing">>
                    ,build_fun = fun port_request/1
                    ,validate_fun = fun port_request_v/1
                    ,publish_fun = fun publish_port_request/1
                    ,binding = ?BINDING_STRING(<<"number">>, <<"port_request">>)
                    ,restrict_to = 'port_request'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Port-Request-ID">>
                                        ]
                    ,optional_headers = ?PORT_OPTIONAL_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"port_request">>)
                    ,types = [{<<"Reason">>, fun kz_json:is_json_object/1}]
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Port Scheduled Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec port_scheduled_definition() -> kapi_definition:api().
port_scheduled_definition() ->
    #kapi_definition{name = <<"port_scheduled">>
                    ,friendly_name = <<"Port Scheduled">>
                    ,description = <<"This event is triggered when a port is accepted by a carrier and scheduled">>
                    ,build_fun = fun port_scheduled/1
                    ,validate_fun = fun port_scheduled_v/1
                    ,publish_fun = fun publish_port_scheduled/1
                    ,binding = ?BINDING_STRING(<<"number">>, <<"port_scheduled">>)
                    ,restrict_to = 'port_scheduled'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Port-Request-ID">>
                                        ]
                    ,optional_headers = ?PORT_OPTIONAL_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"port_scheduled">>)
                    ,types = [{<<"Reason">>, fun kz_json:is_json_object/1}]
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Port Unconfirmed Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec port_unconfirmed_definition() -> kapi_definition:api().
port_unconfirmed_definition() ->
    #kapi_definition{name = <<"port_unconfirmed">>
                    ,friendly_name = <<"Port Unconfirmed">>
                    ,description = <<"This event is triggered when a port is created, prior to submitting">>
                    ,build_fun = fun port_unconfirmed/1
                    ,validate_fun = fun port_unconfirmed_v/1
                    ,publish_fun = fun publish_port_unconfirmed/1
                    ,binding = ?BINDING_STRING(<<"number">>, <<"port_unconfirmed">>)
                    ,restrict_to = 'port_unconfirmed'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Port-Request-ID">>
                                        ]
                    ,optional_headers = ?PORT_OPTIONAL_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"port_unconfirmed">>)
                    ,types = [{<<"Reason">>, fun kz_json:is_json_object/1}]
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Ported API Notification definition.
%% @end
%%------------------------------------------------------------------------------
-spec ported_definition() -> kapi_definition:api().
ported_definition() ->
    #kapi_definition{name = <<"ported">>
                    ,friendly_name = <<"Ported">>
                    ,description = <<"This event is triggered when a port request for number is completed">>
                    ,build_fun = fun ported/1
                    ,validate_fun = fun ported_v/1
                    ,publish_fun = fun publish_ported/1
                    ,binding = ?BINDING_STRING(<<"number">>, <<"ported">>)
                    ,restrict_to = 'ported'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Port-Request-ID">>
                                        ]
                    ,optional_headers = ?PORT_OPTIONAL_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"ported">>)
                    ,types = [{<<"Reason">>, fun kz_json:is_json_object/1}]
                    }.


%%%=============================================================================
%%% Register Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Denied Emergency Notification Bridge API definition.
%% @end
%%------------------------------------------------------------------------------
-spec denied_emergency_bridge_definition() -> kapi_definition:api().
denied_emergency_bridge_definition() ->
    #kapi_definition{name = <<"denied_emergency_bridge">>
                    ,friendly_name = <<"Emergency Call Failed">>
                    ,description = <<"This event is triggered when a call to an number classified as emergency fails">>
                    ,build_fun = fun denied_emergency_bridge/1
                    ,validate_fun = fun denied_emergency_bridge_v/1
                    ,publish_fun = fun publish_denied_emergency_bridge/1
                    ,binding = ?BINDING_STRING(<<"registration">>, <<"denied_emergency_bridge">>)
                    ,restrict_to = 'denied_emergency_bridge'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Call-ID">>
                                        ]
                    ,optional_headers = [<<"Emergency-Caller-ID-Name">>
                                        ,<<"Emergency-Caller-ID-Number">>
                                        ,<<"Outbound-Caller-ID-Name">>
                                        ,<<"Outbound-Caller-ID-Number">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"denied_emergency_bridge">>)
                    ,types = []
                    }.


%%%=============================================================================
%%% SIP Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Deregister API Notification definition.
%% @end
%%------------------------------------------------------------------------------
-spec deregister_definition() -> kapi_definition:api().
deregister_definition() ->
    #kapi_definition{name = <<"deregister">>
                    ,friendly_name = <<"De-Registration">>
                    ,description = <<"This event is triggered when a device fails to re-register and the contact expires">>
                    ,build_fun = fun deregister/1
                    ,validate_fun = fun deregister_v/1
                    ,publish_fun = fun publish_deregister/1
                    ,binding = ?BINDING_STRING(<<"sip">>, <<"deregister">>)
                    ,restrict_to = 'deregister'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Username">>
                                        ,<<"Realm">>
                                        ]
                    ,optional_headers = [<<"Account-DB">>
                                        ,<<"Authorizing-ID">>
                                        ,<<"Call-ID">>
                                        ,<<"Contact">>
                                        ,<<"Event-Timestamp">>
                                        ,<<"Expires">>
                                        ,<<"FreeSWITCH-Hostname">>
                                        ,<<"From-Host">>
                                        ,<<"From-User">>
                                        ,<<"Network-IP">>
                                        ,<<"Network-Port">>
                                        ,<<"Presence-Hosts">>
                                        ,<<"Profile-Name">>
                                        ,<<"RPid">>
                                        ,<<"Status">>
                                        ,<<"Suppress-Unregister-Notify">>
                                        ,<<"To-Host">>
                                        ,<<"To-User">>
                                        ,<<"User-Agent">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"deregister">>)
                    ,types = []
                    }.
%%------------------------------------------------------------------------------
%% @doc Get First Occurrence Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec first_occurrence_definition() -> kapi_definition:api().
first_occurrence_definition() ->
    #kapi_definition{name = <<"first_occurrence">>
                    ,friendly_name = <<"Account First Occurrence">>
                    ,description = <<"This event is triggered when an end user registers the first device and/or places the first call on an account">>
                    ,build_fun = fun first_occurrence/1
                    ,validate_fun = fun first_occurrence_v/1
                    ,publish_fun = fun publish_first_occurrence/1
                    ,binding = ?BINDING_STRING(<<"sip">>, <<"first_occurrence">>)
                    ,restrict_to = 'first_occurrence'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Occurrence">>
                                        ]
                    ,optional_headers = ?DEFAULT_OPTIONAL_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"first_occurrence">>)
                    ,types = []
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Missed Call Notification Alert API definition.
%% @end
%%------------------------------------------------------------------------------
-spec missed_call_definition() -> kapi_definition:api().
missed_call_definition() ->
    #kapi_definition{name = <<"missed_call">>
                    ,friendly_name = <<"Missed Call">>
                    ,description = <<"This event is triggered when an corresponding missed call action in a callflow is invoked">>
                    ,build_fun = fun missed_call/1
                    ,validate_fun = fun missed_call_v/1
                    ,publish_fun = fun publish_missed_call/1
                    ,binding = ?BINDING_STRING(<<"sip">>, <<"missed_call">>)
                    ,restrict_to = 'missed_call'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Call-ID">>
                                        ,<<"Call-Bridged">>
                                        ,<<"Message-Left">>
                                        ]
                    ,optional_headers = [<<"Caller-ID-Name">>
                                        ,<<"Caller-ID-Number">>
                                        ,<<"From-Realm">>
                                        ,<<"From-User">>
                                        ,<<"Notify">>
                                        ,<<"To">>
                                        ,<<"To-Realm">>
                                        ,<<"To-User">>
                                        ,<<"Timestamp">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"missed_call">>)
                    ,types = []
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Register API Notification definition.
%% @end
%%------------------------------------------------------------------------------
-spec register_definition() -> kapi_definition:api().
register_definition() ->
    #kapi_definition{name = <<"register">>
                    ,friendly_name = <<"Registration">>
                    ,description = <<"This event is triggered when a device registers but is not currently registered">>
                    ,build_fun = fun register/1
                    ,validate_fun = fun register_v/1
                    ,publish_fun = fun publish_register/1
                    ,binding = ?BINDING_STRING(<<"sip">>, <<"register">>)
                    ,restrict_to = 'register'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Username">>
                                        ,<<"Realm">>
                                        ]
                    ,optional_headers = [<<"Account-DB">>
                                        ,<<"Authorizing-ID">>
                                        ,<<"Authorizing-Type">>
                                        ,<<"Call-ID">>
                                        ,<<"Contact">>
                                        ,<<"Event-Timestamp">>
                                        ,<<"Expires">>
                                        ,<<"From-Host">>
                                        ,<<"From-User">>
                                        ,<<"Network-IP">>
                                        ,<<"Network-Port">>
                                        ,<<"Owner-ID">>
                                        ,<<"To-Host">>
                                        ,<<"To-User">>
                                        ,<<"Suppress-Unregister-Notify">>
                                        ,<<"User-Agent">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"register">>)
                    ,types = []
                    }.


%%%=============================================================================
%%% System Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get System Alert Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec system_alert_definition() -> kapi_definition:api().
system_alert_definition() ->
    #kapi_definition{name = <<"system_alert">>
                    ,friendly_name = <<"System Alert">>
                    ,description = <<"This event is triggered to alert the system administrators">>
                    ,build_fun = fun system_alert/1
                    ,validate_fun = fun system_alert_v/1
                    ,publish_fun = fun publish_system_alert/1
                    ,binding = ?BINDING_STRING(<<"system">>, <<"alert">>)
                    ,restrict_to = 'system_alert'
                    ,required_headers = [<<"Message">>
                                        ,<<"Subject">>
                                        ]
                    ,optional_headers = [<<"Details">>
                                        ,<<"Line">>
                                        ,<<"Module">>
                                        ,<<"Node">>
                                        ,<<"Pid">>
                                        ,<<"Request-ID">>
                                        ,<<"Section">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"system_alert">>)
                    ,types = []
                    }.


%%%=============================================================================
%%% User Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Customer Update Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec customer_update_definition() -> kapi_definition:api().
customer_update_definition() ->
    #kapi_definition{name = <<"customer_update">>
                    ,friendly_name = <<"Customer Update">>
                    ,description = <<"This event is triggered when the customer update API is used to deliver a message to the account">>
                    ,build_fun = fun customer_update/1
                    ,validate_fun = fun customer_update_v/1
                    ,publish_fun = fun publish_customer_update/1
                    ,binding = ?BINDING_STRING(<<"user">>, <<"customer_update">>)
                    ,restrict_to = 'customer_update'
                    ,required_headers = [<<"Account-ID">>
                                        ]
                    ,optional_headers = [<<"DataBag">>
                                        ,<<"Recipient-ID">>
                                        ,<<"Template-ID">>
                                        ,<<"User-Type">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"customer_update">>)
                    ,types = []
                    }.
%%------------------------------------------------------------------------------
%% @doc Get New User Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec new_user_definition() -> kapi_definition:api().
new_user_definition() ->
    #kapi_definition{name = <<"new_user">>
                    ,friendly_name = <<"New User">>
                    ,description = <<"This event is triggered when an end user creates a new user">>
                    ,build_fun = fun new_user/1
                    ,validate_fun = fun new_user_v/1
                    ,publish_fun = fun publish_new_user/1
                    ,binding = ?BINDING_STRING(<<"user">>, <<"new">>)
                    ,restrict_to = 'new_user'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"User-ID">>
                                        ]
                    ,optional_headers = [<<"Password">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"new_user">>)
                    ,types = []
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Password Recovery Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec password_recovery_definition() -> kapi_definition:api().
password_recovery_definition() ->
    #kapi_definition{name = <<"password_recovery">>
                    ,friendly_name = <<"Password Recovery">>
                    ,description = <<"This event is triggered when an end user requests a password recovery link">>
                    ,build_fun = fun password_recovery/1
                    ,validate_fun = fun password_recovery_v/1
                    ,publish_fun = fun publish_password_recovery/1
                    ,binding = ?BINDING_STRING(<<"user">>, <<"password_recovery">>)
                    ,restrict_to = 'password_recovery'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Email">>
                                        ,<<"Password-Reset-Link">>
                                        ]
                    ,optional_headers = [<<"Account-DB">>
                                        ,<<"First-Name">>
                                        ,<<"Last-Name">>
                                        ,<<"Timezone">>
                                        ,<<"User-ID">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"password_recovery">>)
                    ,types = []
                    }.


%%%=============================================================================
%%% Voicemail Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Voicemail full Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec voicemail_full_definition() -> kapi_definition:api().
voicemail_full_definition() ->
    #kapi_definition{name = <<"voicemail_full">>
                    ,friendly_name = <<"Voicemail Box Full">>
                    ,description = <<"This event is triggered any time an attempt to leave a voicemail message is blocked because the voicemail box is full">>
                    ,build_fun = fun voicemail_full/1
                    ,validate_fun = fun voicemail_full_v/1
                    ,publish_fun = fun publish_voicemail_full/1
                    ,binding = ?BINDING_STRING(<<"voicemail">>, <<"full">>)
                    ,restrict_to = 'voicemail_full'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Max-Message-Count">>
                                        ,<<"Message-Count">>
                                        ,<<"Voicemail-Box">>
                                        ]
                    ,optional_headers = ?DEFAULT_OPTIONAL_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"voicemail_full">>)
                    ,types = []
                    }.

-define(VOICEMAIL_NEW_HEADERS, [<<"Account-ID">>
                               ,<<"From-Realm">>
                               ,<<"From-User">>
                               ,<<"To-Realm">>
                               ,<<"To-User">>
                               ,<<"Voicemail-Box">>
                               ,<<"Voicemail-ID">>
                               ,<<"Voicemail-Timestamp">>
                               ]).
-define(OPTIONAL_VOICEMAIL_NEW_HEADERS, [<<"Call-ID">>
                                        ,<<"Caller-ID-Name">>
                                        ,<<"Caller-ID-Number">>
                                        ,<<"Voicemail-Length">>
                                        ,<<"Voicemail-Transcription">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]).
-define(VOICEMAIL_DELETED_HEADERS, [<<"Account-ID">>
                                   ,<<"From-Realm">>
                                   ,<<"From-User">>
                                   ,<<"To-Realm">>
                                   ,<<"To-User">>
                                   ,<<"Reason">>
                                   ,<<"Voicemail-Box">>
                                   ,<<"Voicemail-ID">>
                                   ,<<"Voicemail-Timestamp">>
                                   ]).
-define(OPTIONAL_VOICEMAIL_DELETED_HEADERS, [<<"Call-ID">>
                                            ,<<"Caller-ID-Name">>
                                            ,<<"Caller-ID-Number">>
                                            ,<<"Voicemail-Length">>
                                            ,<<"Voicemail-Transcription">>
                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                            ]).
%%------------------------------------------------------------------------------
%% @doc Get Voicemail New Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec voicemail_new_definition() -> kapi_definition:api().
voicemail_new_definition() ->
    #kapi_definition{name = <<"voicemail_new">>
                    ,friendly_name = <<"New Voicemail Message">>
                    ,description = <<"This event is triggered any time a voicemail message is left">>
                    ,build_fun = fun voicemail_new/1
                    ,validate_fun = fun voicemail_new_v/1
                    ,publish_fun = fun publish_voicemail_new/1
                    ,binding = ?BINDING_STRING(<<"voicemail">>, <<"new">>)
                    ,restrict_to = 'voicemail_new'
                    ,required_headers = ?VOICEMAIL_NEW_HEADERS
                    ,optional_headers = ?OPTIONAL_VOICEMAIL_NEW_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"voicemail_new">>)
                    ,types = []
                    }.
%%------------------------------------------------------------------------------
%% @doc Get Voicemail Saved Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec voicemail_saved_definition() -> kapi_definition:api().
voicemail_saved_definition() ->
    #kapi_definition{name = <<"voicemail_saved">>
                    ,friendly_name = <<"Voicemail Message Saved">>
                    ,description = <<"This event is triggered any time a voicemail message is saved in the voicemail box 'new' folder">>
                    ,build_fun = fun voicemail_saved/1
                    ,validate_fun = fun voicemail_saved_v/1
                    ,publish_fun = fun publish_voicemail_saved/1
                    ,binding = ?BINDING_STRING(<<"voicemail">>, <<"saved">>)
                    ,restrict_to = 'voicemail_saved'
                    ,required_headers = ?VOICEMAIL_NEW_HEADERS
                    ,optional_headers = ?OPTIONAL_VOICEMAIL_NEW_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"voicemail_saved">>)
                    ,types = []
                    }.

%%------------------------------------------------------------------------------
%% @doc Get Voicemail Deleted Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec voicemail_deleted_definition() -> kapi_definition:api().
voicemail_deleted_definition() ->
    #kapi_definition{name = <<"voicemail_deleted">>
                    ,friendly_name = <<"Voicemail Message Deleted">>
                    ,description = <<"This event is triggered any time a voicemail message is deleted in the voicemail box">>
                    ,build_fun = fun voicemail_deleted/1
                    ,validate_fun = fun voicemail_deleted_v/1
                    ,publish_fun = fun publish_voicemail_deleted/1
                    ,binding = ?BINDING_STRING(<<"voicemail">>, <<"deleted">>)
                    ,restrict_to = 'voicemail_deleted'
                    ,required_headers = ?VOICEMAIL_DELETED_HEADERS
                    ,optional_headers = ?OPTIONAL_VOICEMAIL_DELETED_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"voicemail_deleted">>)
                    ,types = []
                    }.

%%%=============================================================================
%%% Webhook Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Webhook Callflow Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec webhook_definition() -> kapi_definition:api().
webhook_definition() ->
    #kapi_definition{name = <<"webhook">>
                    ,friendly_name = <<"Callflow Webhook Triggered">>
                    ,description = <<"This event is triggered when a corresponding webhook action in a callflow is reached">>
                    ,build_fun = fun webhook/1
                    ,validate_fun = fun webhook_v/1
                    ,publish_fun = fun publish_webhook/1
                    ,binding = ?BINDING_STRING(<<"webhook">>, <<"callflow">>)
                    ,restrict_to = 'webhook'
                    ,required_headers = [<<"Data">>
                                        ,<<"Hook">>
                                        ]
                    ,optional_headers = [<<"Timestamp">>
                                             | ?DEFAULT_OPTIONAL_HEADERS
                                        ]
                    ,values = ?NOTIFY_VALUES(<<"webhook">>)
                    ,types = []
                    }.
%%------------------------------------------------------------------------------
%% @doc Notification Get Webhook Disabled API definition.
%% @end
%%------------------------------------------------------------------------------
-spec webhook_disabled_definition() -> kapi_definition:api().
webhook_disabled_definition() ->
    #kapi_definition{name = <<"webhook_disabled">>
                    ,friendly_name = <<"Webhook Disabled">>
                    ,description = <<"This event is triggered when a webhook is disabled">>
                    ,build_fun = fun webhook_disabled/1
                    ,validate_fun = fun webhook_disabled_v/1
                    ,publish_fun = fun publish_webhook_disabled/1
                    ,binding = ?BINDING_STRING(<<"webhook">>, <<"disabled">>)
                    ,restrict_to = 'webhook_disabled'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Hook-ID">>
                                        ]
                    ,optional_headers = ?DEFAULT_OPTIONAL_HEADERS
                    ,values = ?NOTIFY_VALUES(<<"webhook_disabled">>)
                    ,types = []
                    }.

%%%=============================================================================
%%% Phone Service Required Action Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Phone Service Required Action API definition.
%% @end
%%------------------------------------------------------------------------------
-spec number_feature_manual_action_definition() -> kapi_definition:api().
number_feature_manual_action_definition() ->
    EventName = <<"number_feature_manual_action">>,
    Category = <<"account">>,
    #kapi_definition{name = EventName
                    ,friendly_name = <<"Number Feature Manual Action Required">>
                    ,description = <<"This event is triggered when a number feature is activate/deactivated and a manual action is required">>
                    ,build_fun = fun number_feature_manual_action/1
                    ,validate_fun = fun number_feature_manual_action_v/1
                    ,publish_fun = fun publish_number_feature_manual_action/1
                    ,binding = ?BINDING_STRING(Category, EventName)
                    ,restrict_to = 'number_feature_manual_action_required'
                    ,required_headers = [<<"Account-ID">>
                                        ,<<"Number">>
                                        ,<<"Feature">>
                                        ]
                    ,optional_headers = ?DEFAULT_OPTIONAL_HEADERS
                    ,values = ?NOTIFY_VALUES(EventName)
                    ,types = [{<<"Account-ID">>, fun kz_term:is_ne_binary/1}
                             ,{<<"Number">>, fun kz_term:is_ne_binary/1}
                             ,{<<"Feature">>, fun kz_json:is_json_object/1}
                             ]
                    }.

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get all API definitions of this module.
%% @end
%%------------------------------------------------------------------------------
-spec api_definitions() -> kapi_definition:apis().
api_definitions() ->
    [notify_update_definition()
    ,skel_definition()
    ,bill_reminder_definition()
    ,low_balance_definition()
    ,new_account_definition()
    ,service_added_definition()
    ,topup_definition()
    ,transaction_definition()
    ,account_zone_change_definition()
    ,inbound_fax_definition()
    ,inbound_fax_error_definition()
    ,outbound_fax_definition()
    ,outbound_fax_error_definition()
    ,outbound_smtp_fax_error_definition()
    ,cnam_request_definition()
    ,port_request_definition()
    ,port_cancel_definition()
    ,port_comment_definition()
    ,port_pending_definition()
    ,port_rejected_definition()
    ,port_scheduled_definition()
    ,port_unconfirmed_definition()
    ,ported_definition()
    ,denied_emergency_bridge_definition()
    ,deregister_definition()
    ,first_occurrence_definition()
    ,missed_call_definition()
    ,register_definition()
    ,system_alert_definition()
    ,customer_update_definition()
    ,new_user_definition()
    ,password_recovery_definition()
    ,voicemail_full_definition()
    ,voicemail_new_definition()
    ,voicemail_saved_definition()
    ,webhook_definition()
    ,webhook_disabled_definition()
    ,number_feature_manual_action_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(atom() | kz_term:text() | kz_term:ne_binary()) -> kapi_definition:api().
api_definition(Name) when is_atom(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(Name) when is_list(Name) ->
    api_definition(kz_term:to_binary(Name));
api_definition(<<"notify_update">>) ->
    notify_update_definition();
api_definition(<<"skel">>) ->
    skel_definition();
api_definition(<<"account_zone_change">>) ->
    account_zone_change_definition();
api_definition(<<"bill_reminder">>) ->
    bill_reminder_definition();
api_definition(<<"low_balance">>) ->
    low_balance_definition();
api_definition(<<"new_account">>) ->
    new_account_definition();
api_definition(<<"service_added">>) ->
    service_added_definition();
api_definition(<<"topup">>) ->
    topup_definition();
api_definition(<<"transaction">>) ->
    transaction_definition();
api_definition(<<"inbound_fax">>) ->
    inbound_fax_definition();
api_definition(<<"inbound_fax_error">>) ->
    inbound_fax_error_definition();
api_definition(<<"outbound_fax">>) ->
    outbound_fax_definition();
api_definition(<<"outbound_fax_error">>) ->
    outbound_fax_error_definition();
api_definition(<<"outbound_smtp_fax_error">>) ->
    outbound_smtp_fax_error_definition();
api_definition(<<"cnam_request">>) ->
    cnam_request_definition();
api_definition(<<"port_cancel">>) ->
    port_cancel_definition();
api_definition(<<"port_comment">>) ->
    port_comment_definition();
api_definition(<<"port_pending">>) ->
    port_pending_definition();
api_definition(<<"port_rejected">>) ->
    port_rejected_definition();
api_definition(<<"port_request">>) ->
    port_request_definition();
api_definition(<<"port_scheduled">>) ->
    port_scheduled_definition();
api_definition(<<"port_unconfirmed">>) ->
    port_unconfirmed_definition();
api_definition(<<"ported">>) ->
    ported_definition();
api_definition(<<"denied_emergency_bridge">>) ->
    denied_emergency_bridge_definition();
api_definition(<<"deregister">>) ->
    deregister_definition();
api_definition(<<"first_occurrence">>) ->
    first_occurrence_definition();
api_definition(<<"missed_call">>) ->
    missed_call_definition();
api_definition(<<"register">>) ->
    register_definition();
api_definition(<<"system_alert">>) ->
    system_alert_definition();
api_definition(<<"customer_update">>) ->
    customer_update_definition();
api_definition(<<"new_user">>) ->
    new_user_definition();
api_definition(<<"password_recovery">>) ->
    password_recovery_definition();
api_definition(<<"voicemail_full">>) ->
    voicemail_full_definition();
api_definition(<<"voicemail_new">>) ->
    voicemail_new_definition();
api_definition(<<"voicemail_saved">>) ->
    voicemail_saved_definition();
api_definition(<<"webhook">>) ->
    webhook_definition();
api_definition(<<"webhook_disabled">>) ->
    webhook_disabled_definition();
api_definition(<<"number_feature_manual_action">>) ->
    number_feature_manual_action_definition().

%%------------------------------------------------------------------------------
%% @doc Bind to a queue to this API exchange and events.
%% @end
%%------------------------------------------------------------------------------
-spec bind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
bind_q(Queue, Props) ->
    bind_to_q(Queue, props:get_value('restrict_to', Props)).

-spec bind_to_q(kz_term:ne_binary(), kz_term:api_atoms()) -> 'ok'.
bind_to_q(Q, 'undefined') ->
    'ok' = kz_amqp_util:bind_q_to_notifications(Q, <<"notifications.*.*">>);
bind_to_q(Q, ['new_fax'|T]) ->
    InboundBinding = kapi_definition:binding(inbound_fax_definition()),
    OutboundBinding = kapi_definition:binding(outbound_fax_definition()),
    'ok' = kz_amqp_util:bind_q_to_notifications(Q, InboundBinding),
    'ok' = kz_amqp_util:bind_q_to_notifications(Q, OutboundBinding),
    bind_to_q(Q, T);
bind_to_q(Q, ['fax_error'|T]) ->
    InboundBinding = kapi_definition:binding(inbound_fax_error_definition()),
    OutboundBinding = kapi_definition:binding(outbound_fax_error_definition()),
    'ok' = kz_amqp_util:bind_q_to_notifications(Q, InboundBinding),
    'ok' = kz_amqp_util:bind_q_to_notifications(Q, OutboundBinding),
    bind_to_q(Q, T);
bind_to_q(Q, [RestrictTo|T]) ->
    try [kapi_definition:binding(Definition)
         || Definition <- api_definitions(),
            kapi_definition:restrict_to(Definition) =:= RestrictTo
        ]
    of
        [Binding] ->
            'ok' = kz_amqp_util:bind_q_to_notifications(Q, Binding),
            bind_to_q(Q, T);
        _Else ->
            bind_to_q(Q, T)
    catch
        error:undef ->
            bind_to_q(Q, T)
    end;
bind_to_q(_Q, []) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Unbind from a queue of this API exchange and events.
%% @end
%%------------------------------------------------------------------------------
-spec unbind_q(kz_term:ne_binary(), kz_term:proplist()) -> 'ok'.
unbind_q(Queue, Props) ->
    unbind_q_from(Queue, props:get_value('restrict_to', Props)).

-spec unbind_q_from(kz_term:ne_binary(), kz_term:api_atoms()) -> 'ok'.
unbind_q_from(Q, 'undefined') ->
    'ok' = kz_amqp_util:unbind_q_from_notifications(Q, <<"notifications.*.*">>);
unbind_q_from(Q, ['new_fax'|T]) ->
    InboundBinding = kapi_definition:binding(inbound_fax_definition()),
    OutboundBinding = kapi_definition:binding(outbound_fax_definition()),
    'ok' = kz_amqp_util:unbind_q_from_notifications(Q, InboundBinding),
    'ok' = kz_amqp_util:unbind_q_from_notifications(Q, OutboundBinding),
    unbind_q_from(Q, T);
unbind_q_from(Q, ['fax_error'|T]) ->
    InboundBinding = kapi_definition:binding(inbound_fax_error_definition()),
    OutboundBinding = kapi_definition:binding(outbound_fax_error_definition()),
    'ok' = kz_amqp_util:unbind_q_from_notifications(Q, InboundBinding),
    'ok' = kz_amqp_util:unbind_q_from_notifications(Q, OutboundBinding),
    unbind_q_from(Q, T);
unbind_q_from(Q, [RestrictTo|T]) ->
    try [kapi_definition:binding(Definition)
         || Definition <- api_definitions(),
            kapi_definition:restrict_to(Definition) =:= RestrictTo
        ]
    of
        [Binding] ->
            'ok' = kz_amqp_util:unbind_q_from_notifications(Q, Binding),
            unbind_q_from(Q, T);
        _Else -> unbind_q_from(Q, T)
    catch
        error:undef ->
            unbind_q_from(Q, T)
    end;
unbind_q_from(_Q, []) ->
    'ok'.

%%------------------------------------------------------------------------------
%% @doc Declare the exchanges used by this API.
%% @end
%%------------------------------------------------------------------------------
-spec declare_exchanges() -> 'ok'.
declare_exchanges() ->
    kz_amqp_util:notifications_exchange().

%%%=============================================================================
%%% Helpers
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Look up account ID in the given API term.
%% @end
%%------------------------------------------------------------------------------
-spec account_id(kz_term:api_terms()) -> kz_term:api_ne_binary().
account_id('undefined') -> 'undefined';
account_id(Req) when is_list(Req) -> find_account_id(Req, fun props:get_first_defined/2);
account_id(Req) -> find_account_id(Req, fun kz_json:get_first_defined/2).

-spec find_account_id(kz_term:api_terms(), function()) -> kz_term:api_ne_binary().
find_account_id(Req, GetFun) ->
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
    case GetFun(Paths, Req) of
        ?NE_BINARY=Id -> Id;
        _ -> 'undefined'
    end.

%%------------------------------------------------------------------------------
%% @doc Look up account DB in the given API term.
%% @end
%%------------------------------------------------------------------------------
-spec account_db(kz_term:api_terms(), boolean()) -> kz_term:api_ne_binary().
account_db('undefined', _) -> 'undefined';
account_db(Req, StrictMODB) when is_list(Req) -> find_account_db(Req, StrictMODB, fun props:get_first_defined/2);
account_db(Req, StrictMODB) -> find_account_db(Req, StrictMODB, fun kz_json:get_first_defined/2).

-spec find_account_db(kz_term:api_terms(), boolean(), function()) -> kz_term:api_ne_binary().
find_account_db(Req, StrictMODB, GetFun) ->
    Paths = [<<"account_db">>, <<"pvt_account_db">>, <<"Account-DB">>],
    case GetFun(Paths, Req) of
        'undefined' ->
            case find_account_id(Req, GetFun) of
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

%%------------------------------------------------------------------------------
%% @doc Get a list of required and optional headers of the given Notification API.
%% @end
%%------------------------------------------------------------------------------
-spec headers(kz_term:ne_binary()) -> kz_term:ne_binaries().
headers(<<"fax_inbound_to_email">>) ->
    headers(<<"inbound_fax">>);
headers(<<"fax_inbound_error_to_email">>) ->
    headers(<<"inbound_fax_error">>);
headers(<<"fax_outbound_to_email">>) ->
    headers(<<"outbound_fax">>);
headers(<<"fax_outbound_error_to_email">>) ->
    headers(<<"outbound_fax_error">>);
headers(<<"fax_outbound_smtp_error">>) ->
    headers(<<"outbound_smtp_fax_error">>);
headers(Name) ->
    try api_definition(Name) of
        Definition ->
            kapi_definition:required_headers(Definition) ++ kapi_definition:optional_headers(Definition)
    catch
        error:undef ->
            lager:warning("no notification headers for ~s", [Name]),
            []
    end.

%%------------------------------------------------------------------------------
%% @doc Generic function to build API payload.
%% @end
%%------------------------------------------------------------------------------
-spec build_message(kz_term:api_terms(), kapi_definition:api()) -> api_formatter_return().
build_message(Prop, #kapi_definition{required_headers = ReqH
                                    ,optional_headers = OptH
                                    ,validate_fun = Validate
                                    ,name = _Name
                                    }) when is_list(Prop) ->
    case Validate(Prop) of
        'true' -> kz_api:build_message(Prop, ReqH, OptH);
        'false' -> {'error', "Proplist failed validation for " ++ binary_to_list(_Name)}
    end;
build_message(JObj, Definition) ->
    build_message(kz_json:to_proplist(JObj), Definition).

%%------------------------------------------------------------------------------
%% @doc Generic function to validate API payload.
%% @end
%%------------------------------------------------------------------------------
validate(Prop, #kapi_definition{required_headers = ReqH
                               ,values = Values
                               ,types = Types
                               }) when is_list(Prop) ->
    kz_api:validate(Prop, ReqH, Values, Types);
validate(JObj, Definition) ->
    validate(kz_json:to_proplist(JObj), Definition).

%%%=============================================================================
%%% Internal Notifications Functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Notify Status notification.
%% Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec notify_update(kz_term:api_terms()) -> api_formatter_return().
notify_update(Prop) ->
    build_message(Prop, notify_update_definition()).

-spec notify_update_v(kz_term:api_terms()) -> boolean().
notify_update_v(Prop) ->
    validate(Prop, notify_update_definition()).

-spec publish_notify_update(kz_term:ne_binary(), kz_term:api_terms()) -> 'ok'.
publish_notify_update(RespQ, JObj) ->
    publish_notify_update(RespQ, JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_notify_update(kz_term:ne_binary(), kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_notify_update(RespQ, API, ContentType) ->
    #kapi_definition{values = Values} = notify_update_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun notify_update/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Skeleton notification
%% Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec skel(kz_term:api_terms()) -> api_formatter_return().
skel(Prop) ->
    build_message(Prop, skel_definition()).

-spec skel_v(kz_term:api_terms()) -> boolean().
skel_v(Prop) ->
    validate(Prop, skel_definition()).

-spec publish_skel(kz_term:api_terms()) -> 'ok'.
publish_skel(JObj) ->
    publish_skel(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_skel(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_skel(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = skel_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun skel/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).


%%%=============================================================================
%%% Account Notifications Functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec account_zone_change(kz_term:api_terms()) -> api_formatter_return().
account_zone_change(Prop) ->
    build_message(Prop, account_zone_change_definition()).

-spec account_zone_change_v(kz_term:api_terms()) -> boolean().
account_zone_change_v(Prop) ->
    validate(Prop, account_zone_change_definition()).

-spec publish_account_zone_change(kz_term:api_terms()) -> 'ok'.
publish_account_zone_change(JObj) ->
    publish_account_zone_change(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_account_zone_change(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_account_zone_change(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = account_zone_change_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun account_zone_change/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec bill_reminder(kz_term:api_terms()) -> api_formatter_return().
bill_reminder(Prop) ->
    build_message(Prop, bill_reminder_definition()).

-spec bill_reminder_v(kz_term:api_terms()) -> boolean().
bill_reminder_v(Prop) ->
    validate(Prop, bill_reminder_definition()).

-spec publish_bill_reminder(kz_term:api_terms()) -> 'ok'.
publish_bill_reminder(JObj) ->
    publish_bill_reminder(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_bill_reminder(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_bill_reminder(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = bill_reminder_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun bill_reminder/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec low_balance(kz_term:api_terms()) -> api_formatter_return().
low_balance(Prop) ->
    build_message(Prop, low_balance_definition()).

-spec low_balance_v(kz_term:api_terms()) -> boolean().
low_balance_v(Prop) ->
    validate(Prop, low_balance_definition()).

-spec publish_low_balance(kz_term:api_terms()) -> 'ok'.
publish_low_balance(JObj) ->
    publish_low_balance(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_low_balance(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_low_balance(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = low_balance_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun low_balance/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec new_account(kz_term:api_terms()) -> api_formatter_return().
new_account(Prop) ->
    build_message(Prop, new_account_definition()).

-spec new_account_v(kz_term:api_terms()) -> boolean().
new_account_v(Prop) ->
    validate(Prop, new_account_definition()).

-spec publish_new_account(kz_term:api_terms()) -> 'ok'.
publish_new_account(JObj) ->
    publish_new_account(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_new_account(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_new_account(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = new_account_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun new_account/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec service_added(kz_term:api_terms()) -> api_formatter_return().
service_added(Prop) ->
    build_message(Prop, service_added_definition()).

-spec service_added_v(kz_term:api_terms()) -> boolean().
service_added_v(Prop) ->
    validate(Prop, service_added_definition()).

-spec publish_service_added(kz_term:api_terms()) -> 'ok'.
publish_service_added(JObj) ->
    publish_service_added(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_service_added(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_service_added(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = service_added_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun service_added/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec topup(kz_term:api_terms()) -> api_formatter_return().
topup(Prop) ->
    build_message(Prop, topup_definition()).

-spec topup_v(kz_term:api_terms()) -> boolean().
topup_v(Prop) ->
    validate(Prop, topup_definition()).

-spec publish_topup(kz_term:api_terms()) -> 'ok'.
publish_topup(JObj) ->
    publish_topup(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_topup(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_topup(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = topup_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun topup/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec transaction(kz_term:api_terms()) -> api_formatter_return().
transaction(Prop) ->
    build_message(Prop, transaction_definition()).

-spec transaction_v(kz_term:api_terms()) -> boolean().
transaction_v(Prop) ->
    validate(Prop, transaction_definition()).

-spec publish_transaction(kz_term:api_terms()) -> 'ok'.
publish_transaction(JObj) ->
    publish_transaction(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_transaction(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_transaction(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = transaction_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun transaction/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).


%%%=============================================================================
%%% Fax Notifications Functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec fax_inbound(kz_term:api_terms()) -> api_formatter_return().
fax_inbound(Prop) ->
    build_message(Prop, inbound_fax_definition()).

-spec fax_inbound_v(kz_term:api_terms()) -> boolean().
fax_inbound_v(Prop) ->
    validate(Prop, inbound_fax_definition()).

-spec publish_fax_inbound(kz_term:api_terms()) -> 'ok'.
publish_fax_inbound(JObj) ->
    publish_fax_inbound(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_fax_inbound(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_fax_inbound(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = inbound_fax_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun fax_inbound/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec fax_inbound_error(kz_term:api_terms()) -> api_formatter_return().
fax_inbound_error(Prop) ->
    build_message(Prop, inbound_fax_error_definition()).

-spec fax_inbound_error_v(kz_term:api_terms()) -> boolean().
fax_inbound_error_v(Prop) ->
    validate(Prop, inbound_fax_error_definition()).

-spec publish_fax_inbound_error(kz_term:api_terms()) -> 'ok'.
publish_fax_inbound_error(JObj) ->
    publish_fax_inbound_error(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_fax_inbound_error(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_fax_inbound_error(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = inbound_fax_error_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun fax_inbound_error/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec fax_outbound(kz_term:api_terms()) -> api_formatter_return().
fax_outbound(Prop) ->
    build_message(Prop, outbound_fax_definition()).

-spec fax_outbound_v(kz_term:api_terms()) -> boolean().
fax_outbound_v(Prop) ->
    validate(Prop, outbound_fax_definition()).

-spec publish_fax_outbound(kz_term:api_terms()) -> 'ok'.
publish_fax_outbound(JObj) ->
    publish_fax_outbound(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_fax_outbound(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_fax_outbound(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = outbound_fax_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun fax_outbound/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec fax_outbound_error(kz_term:api_terms()) -> api_formatter_return().
fax_outbound_error(Prop) ->
    build_message(Prop, outbound_fax_error_definition()).

-spec fax_outbound_error_v(kz_term:api_terms()) -> boolean().
fax_outbound_error_v(Prop) ->
    validate(Prop, outbound_fax_error_definition()).

-spec publish_fax_outbound_error(kz_term:api_terms()) -> 'ok'.
publish_fax_outbound_error(JObj) ->
    publish_fax_outbound_error(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_fax_outbound_error(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_fax_outbound_error(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = outbound_fax_error_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun fax_outbound_error/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec fax_outbound_smtp_error(kz_term:api_terms()) -> api_formatter_return().
fax_outbound_smtp_error(Prop) ->
    build_message(Prop, outbound_smtp_fax_error_definition()).

-spec fax_outbound_smtp_error_v(kz_term:api_terms()) -> boolean().
fax_outbound_smtp_error_v(Prop) ->
    validate(Prop, outbound_smtp_fax_error_definition()).

-spec publish_fax_outbound_smtp_error(kz_term:api_terms()) -> 'ok'.
publish_fax_outbound_smtp_error(JObj) ->
    publish_fax_outbound_smtp_error(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_fax_outbound_smtp_error(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_fax_outbound_smtp_error(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = outbound_smtp_fax_error_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun fax_outbound_smtp_error/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).


%%%=============================================================================
%%% Number and Port Notifications Functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec cnam_request(kz_term:api_terms()) -> api_formatter_return().
cnam_request(Prop) ->
    build_message(Prop, cnam_request_definition()).

-spec cnam_request_v(kz_term:api_terms()) -> boolean().
cnam_request_v(Prop) ->
    validate(Prop, cnam_request_definition()).

-spec publish_cnam_request(kz_term:api_terms()) -> 'ok'.
publish_cnam_request(JObj) ->
    publish_cnam_request(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_cnam_request(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_cnam_request(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = cnam_request_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun cnam_request/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec port_cancel(kz_term:api_terms()) -> api_formatter_return().
port_cancel(Prop) ->
    build_message(Prop, port_cancel_definition()).

-spec port_cancel_v(kz_term:api_terms()) -> boolean().
port_cancel_v(Prop) ->
    validate(Prop, port_cancel_definition()).

-spec publish_port_cancel(kz_term:api_terms()) -> 'ok'.
publish_port_cancel(JObj) ->
    publish_port_cancel(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_port_cancel(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_port_cancel(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = port_cancel_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun port_cancel/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec port_comment(kz_term:api_terms()) -> api_formatter_return().
port_comment(Prop) ->
    build_message(Prop, port_comment_definition()).

-spec port_comment_v(kz_term:api_terms()) -> boolean().
port_comment_v(Prop) ->
    validate(Prop, port_comment_definition()).

-spec publish_port_comment(kz_term:api_terms()) -> 'ok'.
publish_port_comment(JObj) ->
    publish_port_comment(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_port_comment(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_port_comment(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = port_comment_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun port_comment/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec port_pending(kz_term:api_terms()) -> api_formatter_return().
port_pending(Prop) ->
    build_message(Prop, port_pending_definition()).

-spec port_pending_v(kz_term:api_terms()) -> boolean().
port_pending_v(Prop) ->
    validate(Prop, port_pending_definition()).

-spec publish_port_pending(kz_term:api_terms()) -> 'ok'.
publish_port_pending(JObj) ->
    publish_port_pending(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_port_pending(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_port_pending(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = port_pending_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun port_pending/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec port_rejected(kz_term:api_terms()) -> api_formatter_return().
port_rejected(Prop) ->
    build_message(Prop, port_rejected_definition()).

-spec port_rejected_v(kz_term:api_terms()) -> boolean().
port_rejected_v(Prop) ->
    validate(Prop, port_rejected_definition()).

-spec publish_port_rejected(kz_term:api_terms()) -> 'ok'.
publish_port_rejected(JObj) ->
    publish_port_rejected(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_port_rejected(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_port_rejected(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = port_rejected_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun port_rejected/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec port_request(kz_term:api_terms()) -> api_formatter_return().
port_request(Prop) ->
    build_message(Prop, port_request_definition()).

-spec port_request_v(kz_term:api_terms()) -> boolean().
port_request_v(Prop) ->
    validate(Prop, port_request_definition()).

-spec publish_port_request(kz_term:api_terms()) -> 'ok'.
publish_port_request(JObj) ->
    publish_port_request(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_port_request(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_port_request(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = port_request_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun port_request/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec port_scheduled(kz_term:api_terms()) -> api_formatter_return().
port_scheduled(Prop) ->
    build_message(Prop, port_scheduled_definition()).

-spec port_scheduled_v(kz_term:api_terms()) -> boolean().
port_scheduled_v(Prop) ->
    validate(Prop, port_scheduled_definition()).

-spec publish_port_scheduled(kz_term:api_terms()) -> 'ok'.
publish_port_scheduled(JObj) ->
    publish_port_scheduled(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_port_scheduled(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_port_scheduled(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = port_scheduled_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun port_scheduled/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec port_unconfirmed(kz_term:api_terms()) -> api_formatter_return().
port_unconfirmed(Prop) ->
    build_message(Prop, port_unconfirmed_definition()).

-spec port_unconfirmed_v(kz_term:api_terms()) -> boolean().
port_unconfirmed_v(Prop) ->
    validate(Prop, port_unconfirmed_definition()).

-spec publish_port_unconfirmed(kz_term:api_terms()) -> 'ok'.
publish_port_unconfirmed(JObj) ->
    publish_port_unconfirmed(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_port_unconfirmed(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_port_unconfirmed(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = port_unconfirmed_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun port_unconfirmed/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec ported(kz_term:api_terms()) -> api_formatter_return().
ported(Prop) ->
    build_message(Prop, ported_definition()).

-spec ported_v(kz_term:api_terms()) -> boolean().
ported_v(Prop) ->
    validate(Prop, ported_definition()).

-spec publish_ported(kz_term:api_terms()) -> 'ok'.
publish_ported(JObj) ->
    publish_ported(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_ported(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_ported(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = ported_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun ported/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).


%%%=============================================================================
%%% Register Notifications Functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec denied_emergency_bridge(kz_term:api_terms()) -> api_formatter_return().
denied_emergency_bridge(Prop) ->
    build_message(Prop, denied_emergency_bridge_definition()).

-spec denied_emergency_bridge_v(kz_term:api_terms()) -> boolean().
denied_emergency_bridge_v(Prop) ->
    validate(Prop, denied_emergency_bridge_definition()).

-spec publish_denied_emergency_bridge(kz_term:api_terms()) -> 'ok'.
publish_denied_emergency_bridge(JObj) ->
    publish_denied_emergency_bridge(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_denied_emergency_bridge(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_denied_emergency_bridge(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = denied_emergency_bridge_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun denied_emergency_bridge/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).


%%%=============================================================================
%%% Register Notifications Functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec deregister(kz_term:api_terms()) -> api_formatter_return().
deregister(Prop) ->
    build_message(Prop, deregister_definition()).

-spec deregister_v(kz_term:api_terms()) -> boolean().
deregister_v(Prop) ->
    validate(Prop, deregister_definition()).

-spec publish_deregister(kz_term:api_terms()) -> 'ok'.
publish_deregister(JObj) ->
    publish_deregister(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_deregister(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_deregister(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = deregister_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun deregister/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec first_occurrence(kz_term:api_terms()) -> api_formatter_return().
first_occurrence(Prop) ->
    build_message(Prop, first_occurrence_definition()).

-spec first_occurrence_v(kz_term:api_terms()) -> boolean().
first_occurrence_v(Prop) ->
    validate(Prop, first_occurrence_definition()).

-spec publish_first_occurrence(kz_term:api_terms()) -> 'ok'.
publish_first_occurrence(JObj) ->
    publish_first_occurrence(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_first_occurrence(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_first_occurrence(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = first_occurrence_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun first_occurrence/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec missed_call(kz_term:api_terms()) -> api_formatter_return().
missed_call(Prop) ->
    build_message(Prop, missed_call_definition()).

-spec missed_call_v(kz_term:api_terms()) -> boolean().
missed_call_v(Prop) ->
    validate(Prop, missed_call_definition()).

-spec publish_missed_call(kz_term:api_terms()) -> 'ok'.
publish_missed_call(JObj) ->
    publish_missed_call(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_missed_call(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_missed_call(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = missed_call_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun missed_call/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec register(kz_term:api_terms()) -> api_formatter_return().
register(Prop) ->
    build_message(Prop, register_definition()).

-spec register_v(kz_term:api_terms()) -> boolean().
register_v(Prop) ->
    validate(Prop, register_definition()).

-spec publish_register(kz_term:api_terms()) -> 'ok'.
publish_register(JObj) ->
    publish_register(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_register(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_register(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = register_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun register/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).


%%%=============================================================================
%%% System Notifications Functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec system_alert(kz_term:api_terms()) -> api_formatter_return().
system_alert(Prop) ->
    build_message(Prop, system_alert_definition()).

-spec system_alert_v(kz_term:api_terms()) -> boolean().
system_alert_v(Prop) ->
    validate(Prop, system_alert_definition()).

-spec publish_system_alert(kz_term:api_terms()) -> 'ok'.
publish_system_alert(JObj) ->
    publish_system_alert(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_system_alert(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_system_alert(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = system_alert_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun system_alert/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).


%%%=============================================================================
%%% User Notifications Functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec customer_update(kz_term:api_terms()) -> api_formatter_return().
customer_update(Prop) ->
    build_message(Prop, customer_update_definition()).

-spec customer_update_v(kz_term:api_terms()) -> boolean().
customer_update_v(Prop) ->
    validate(Prop, customer_update_definition()).

-spec publish_customer_update(kz_term:api_terms()) -> 'ok'.
publish_customer_update(JObj) ->
    publish_customer_update(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_customer_update(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_customer_update(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = customer_update_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun customer_update/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec new_user(kz_term:api_terms()) -> api_formatter_return().
new_user(Prop) ->
    build_message(Prop, new_user_definition()).

-spec new_user_v(kz_term:api_terms()) -> boolean().
new_user_v(Prop) ->
    validate(Prop, new_user_definition()).

-spec publish_new_user(kz_term:api_terms()) -> 'ok'.
publish_new_user(JObj) ->
    publish_new_user(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_new_user(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_new_user(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = new_user_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun new_user/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec password_recovery(kz_term:api_terms()) -> api_formatter_return().
password_recovery(Prop) ->
    build_message(Prop, password_recovery_definition()).

-spec password_recovery_v(kz_term:api_terms()) -> boolean().
password_recovery_v(Prop) ->
    validate(Prop, password_recovery_definition()).

-spec publish_password_recovery(kz_term:api_terms()) -> 'ok'.
publish_password_recovery(JObj) ->
    publish_password_recovery(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_password_recovery(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_password_recovery(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = password_recovery_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun password_recovery/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).


%%%=============================================================================
%%% Voicemail Notifications Functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec voicemail_full(kz_term:api_terms()) -> api_formatter_return().
voicemail_full(Prop) ->
    build_message(Prop, voicemail_full_definition()).

-spec voicemail_full_v(kz_term:api_terms()) -> boolean().
voicemail_full_v(Prop) ->
    validate(Prop, voicemail_full_definition()).

-spec publish_voicemail_full(kz_term:api_terms()) -> 'ok'.
publish_voicemail_full(JObj) ->
    publish_voicemail_full(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_voicemail_full(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_voicemail_full(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = voicemail_full_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun voicemail_full/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec voicemail_new(kz_term:api_terms()) -> api_formatter_return().
voicemail_new(Prop) ->
    build_message(Prop, voicemail_new_definition()).

-spec voicemail_new_v(kz_term:api_terms()) -> boolean().
voicemail_new_v(Prop) ->
    validate(Prop, voicemail_new_definition()).

-spec publish_voicemail_new(kz_term:api_terms()) -> 'ok'.
publish_voicemail_new(JObj) ->
    publish_voicemail_new(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_voicemail_new(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_voicemail_new(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = voicemail_new_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun voicemail_new/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec voicemail_saved(kz_term:api_terms()) -> api_formatter_return().
voicemail_saved(Prop) ->
    build_message(Prop, voicemail_saved_definition()).

-spec voicemail_saved_v(kz_term:api_terms()) -> boolean().
voicemail_saved_v(Prop) ->
    validate(Prop, voicemail_saved_definition()).

-spec publish_voicemail_saved(kz_term:api_terms()) -> 'ok'.
publish_voicemail_saved(JObj) ->
    publish_voicemail_saved(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_voicemail_saved(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_voicemail_saved(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = voicemail_saved_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun voicemail_saved/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec voicemail_deleted(kz_term:api_terms()) -> api_formatter_return().
voicemail_deleted(Prop) ->
    build_message(Prop, voicemail_deleted_definition()).

-spec voicemail_deleted_v(kz_term:api_terms()) -> boolean().
voicemail_deleted_v(Prop) ->
    validate(Prop, voicemail_deleted_definition()).

-spec publish_voicemail_deleted(kz_term:api_terms()) -> 'ok'.
publish_voicemail_deleted(JObj) ->
    publish_voicemail_deleted(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_voicemail_deleted(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_voicemail_deleted(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = voicemail_deleted_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun voicemail_deleted/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%%=============================================================================
%%% Webhook Notifications Functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec webhook(kz_term:api_terms()) -> api_formatter_return().
webhook(Prop) ->
    build_message(Prop, webhook_definition()).

-spec webhook_v(kz_term:api_terms()) -> boolean().
webhook_v(Prop) ->
    validate(Prop, webhook_definition()).

-spec publish_webhook(kz_term:api_terms()) -> 'ok'.
publish_webhook(JObj) ->
    publish_webhook(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_webhook(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_webhook(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = webhook_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun webhook/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes prop-list, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec webhook_disabled(kz_term:api_terms()) -> api_formatter_return().
webhook_disabled(Prop) ->
    build_message(Prop, webhook_disabled_definition()).

-spec webhook_disabled_v(kz_term:api_terms()) -> boolean().
webhook_disabled_v(Prop) ->
    validate(Prop, webhook_disabled_definition()).

-spec publish_webhook_disabled(kz_term:api_terms()) -> 'ok'.
publish_webhook_disabled(JObj) ->
    publish_webhook_disabled(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_webhook_disabled(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_webhook_disabled(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = webhook_disabled_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun webhook_disabled/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%%=============================================================================
%%% Phone Service Action Required Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec number_feature_manual_action(kz_term:api_terms()) -> api_formatter_return().
number_feature_manual_action(Prop) ->
    build_message(Prop, number_feature_manual_action_definition()).

-spec number_feature_manual_action_v(kz_term:api_terms()) -> boolean().
number_feature_manual_action_v(Prop) ->
    validate(Prop, number_feature_manual_action_definition()).

-spec publish_number_feature_manual_action(kz_term:api_terms()) -> 'ok'.
publish_number_feature_manual_action(JObj) ->
    publish_number_feature_manual_action(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_number_feature_manual_action(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_number_feature_manual_action(API, ContentType) ->
    #kapi_definition{binding = Binding
                    ,values = Values
                    } = number_feature_manual_action_definition(),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun number_feature_manual_action/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).
