%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2011-2020, 2600Hz
%%% @doc Notification messages, like voicemail left.
%%% @author James Aimonetti
%%% @author Karl Anderson
%%% @author Hesaam Farhang
%%% This Source Code Form is subject to the terms of the Mozilla Public
%%% License, v. 2.0. If a copy of the MPL was not distributed with this
%%% file, You can obtain one at https://mozilla.org/MPL/2.0/.
%%%
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

         %% Webhook notifications
        ,webhook/1, webhook_v/1
        ,webhook_disabled/1, webhook_disabled_v/1

         %% published on completion of notification
        ,notify_update/1, notify_update_v/1

         %% Customer defined notification
        ,cf_notification/1, cf_notification_v/1

         %% skeleton notification
        ,skel/1, skel_v/1
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

         %% Webhook notifications
        ,publish_webhook/1, publish_webhook/2
        ,publish_webhook_disabled/1, publish_webhook_disabled/2

         %% published on completion of notification
        ,publish_notify_update/2, publish_notify_update/3

         %% Customer defined notification
        ,publish_cf_notification/1, publish_cf_notification/2

         %% skeleton notification
        ,publish_skel/1, publish_skel/2
        ]).

-export_type([doc/0]).

-include_lib("kz_amqp_util.hrl").

-type doc() :: kz_json:object().

-define(DEFAULT_OPTIONAL_HEADERS, [<<"Account-DB">>
                                  ,<<"Account-ID">>
                                  ,<<"Attachment-URL">>
                                  ,<<"Bcc">>
                                  ,<<"Cc">>
                                  ,<<"From">>
                                  ,<<"HTML">>
                                  ,<<"Preview">>
                                  ,<<"Reply-To">>
                                  ,<<"Subject">>
                                  ,<<"Text">>
                                  ,<<"To">>
                                  ]).

-define(BINDING_STRING(Category, Name), <<"notifications.", (Category)/binary, ".", (Name)/binary>>).

-define(NOTIFY_VALUES(Name), kapi_definition:event_type_headers(<<"notification">>, Name)).

%%%=============================================================================
%%% Internal Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Notify Update Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec notify_update_definition() -> kapi_definition:api().
notify_update_definition() ->
    EventName = <<"notify_update">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Notify Status Update">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered by notification consumer, e.g. teletype application, to send status of notification being process to publisher">>
               }
              ,{fun kapi_definition:set_build_fun/2, fun notify_update/1}
              ,{fun kapi_definition:set_validate_fun/2, fun notify_update_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_notify_update/2}
              ,{fun kapi_definition:set_required_headers/2, [<<"Status">>]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Failure-Message">>
                                                            ,<<"Metadata">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, [{<<"Status">>, [<<"completed">>
                                                                  ,<<"disabled">>
                                                                  ,<<"failed">>
                                                                  ,<<"ignored">>
                                                                  ,<<"pending">>
                                                                  ]
                                                   }
                                                   | ?NOTIFY_VALUES(EventName)
                                                  ]}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Skeleton API Notification definition.
%% @end
%%------------------------------------------------------------------------------
-spec skel_definition() -> kapi_definition:api().
skel_definition() ->
    EventName = <<"skel">>,
    Category = <<"account">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Example Notification">>}
              ,{fun kapi_definition:set_description/2
               ,<<"An example notification, this event should never be triggered">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun skel/1}
              ,{fun kapi_definition:set_validate_fun/2, fun skel_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_skel/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"skel">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'skel'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"User-ID">>
                                                            ]
               }
              ,{fun kapi_definition:set_optional_headers/2, ?DEFAULT_OPTIONAL_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).


%%%=============================================================================
%%% Account Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Account Zone Notification Change API definition.
%% @end
%%------------------------------------------------------------------------------
-spec account_zone_change_definition() -> kapi_definition:api().
account_zone_change_definition() ->
    EventName = <<"account_zone_change">>,
    Category = <<"account">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Account Zone Change">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when an end user requests the home zone of an account is changed">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun account_zone_change/1}
              ,{fun kapi_definition:set_validate_fun/2, fun account_zone_change_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_account_zone_change/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"zone_change">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'account_zone_change'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Zones">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, ?DEFAULT_OPTIONAL_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Bill Reminder Notification (service invoices) API definition.
%% @end
%%------------------------------------------------------------------------------
-spec bill_reminder_definition() -> kapi_definition:api().
bill_reminder_definition() ->
    EventName = <<"bill_reminder">>,
    Category = <<"account">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Bill Reminder">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered before a few days before the end of the month to remind account's owners of estimated service plan charges">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun bill_reminder/1}
              ,{fun kapi_definition:set_validate_fun/2, fun bill_reminder_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_bill_reminder/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"reminder">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'bill_reminder'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Due-Date">>
                                                            ,<<"Items">>
                                                            ,<<"Timestamp">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Payment-Token">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, [{<<"Account-ID">>, fun kz_term:is_ne_binary/1}
                                                 ,{<<"Due-Date">>, fun kz_term:is_pos_integer/1}
                                                 ,{<<"Items">>, fun kz_json:are_json_objects/1}
                                                 ,{<<"Payment-Token">>, fun kz_json:is_json_object/1}
                                                 ,{<<"Timestamp">>, fun kz_term:is_pos_integer/1}
                                                 ]}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Low Balance Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec low_balance_definition() -> kapi_definition:api().
low_balance_definition() ->
    EventName = <<"low_balance">>,
    Category = <<"account">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Account Low Balance">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when an account is found with a balance below the notification threshold">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun low_balance/1}
              ,{fun kapi_definition:set_validate_fun/2, fun low_balance_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_low_balance/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"low_balance">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'low_balance'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Current-Balance">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, ?DEFAULT_OPTIONAL_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get New Account Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec new_account_definition() -> kapi_definition:api().
new_account_definition() ->
    EventName = <<"new_account">>,
    Category = <<"account">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"New Account">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when an end user creates a new account">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun new_account/1}
              ,{fun kapi_definition:set_validate_fun/2, fun new_account_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_new_account/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"new">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'new_account'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Account-API-Key">>
                                                            ,<<"Account-Name">>
                                                            ,<<"Account-Realm">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get New Service Notification Addition (from service audit log) API definition.
%% @end
%%------------------------------------------------------------------------------
-spec service_added_definition() -> kapi_definition:api().
service_added_definition() ->
    EventName = <<"service_added">>,
    Category = <<"account">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Service Added">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when an account's billable quantities change">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun service_added/1}
              ,{fun kapi_definition:set_validate_fun/2, fun service_added_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_service_added/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"service_added">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'service_added'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Audit-Log">>
                                                            ,<<"Items">>
                                                            ,<<"Timestamp">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, ?DEFAULT_OPTIONAL_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, [{<<"Items">>, fun kz_json:are_json_objects/1}]}
              ],
    kapi_definition:setters(Setters).

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
    EventName = <<"topup">>,
    Category = <<"account">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Automatic Account Top-up">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when an account automatic top-up is attempted">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun topup/1}
              ,{fun kapi_definition:set_validate_fun/2, fun topup_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_topup/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"topup">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'topup'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Amount">>
                                                            ,<<"Response">>
                                                            ,<<"Success">>
                                                            ,<<"Timestamp">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, ?COMMON_TRANSACTION_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%-----------------------------------------------------------------------------%% Transaction
%% @doc Get same headers Notification for top-up and transaction API definition.
%% @end
%%------------------------------------------------------------------------------
-spec transaction_definition() -> kapi_definition:api().
transaction_definition() ->
    EventName = <<"transaction">>,
    Category = <<"account">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Transaction Completed">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a transaction is attempted">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun transaction/1}
              ,{fun kapi_definition:set_validate_fun/2, fun transaction_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_transaction/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"transaction">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'transaction'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Amount">>
                                                            ,<<"Response">>
                                                            ,<<"Success">>
                                                            ,<<"Timestamp">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2
               ,[<<"Service-Plan">> | ?COMMON_TRANSACTION_HEADERS]
               }
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%%=============================================================================
%%% Fax Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Fax Inbound Notification (New Inbound Fax) API definition.
%% @end
%%------------------------------------------------------------------------------
-spec inbound_fax_definition() -> kapi_definition:api().
inbound_fax_definition() ->
    EventName = <<"inbound_fax">>,
    Category = <<"fax">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Successful Fax Reception">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a fax is successfully received">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun fax_inbound/1}
              ,{fun kapi_definition:set_validate_fun/2, fun fax_inbound_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_fax_inbound/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"inbound">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'inbound_fax'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Fax-ID">>
                                                            ,<<"From-Realm">>
                                                            ,<<"From-User">>
                                                            ,<<"To-Realm">>
                                                            ,<<"To-User">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Call-ID">>
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
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Fax Inbound Notification Error API definition.
%% @end
%%------------------------------------------------------------------------------
-spec inbound_fax_error_definition() -> kapi_definition:api().
inbound_fax_error_definition() ->
    EventName = <<"inbound_fax_error">>,
    Category = <<"fax">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Fax Reception Error">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when receiving a fax fails">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun fax_inbound_error/1}
              ,{fun kapi_definition:set_validate_fun/2, fun fax_inbound_error_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_fax_inbound_error/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"inbound_error">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'inbound_fax_error'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"From-Realm">>
                                                            ,<<"From-User">>
                                                            ,<<"To-Realm">>
                                                            ,<<"To-User">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Call-ID">>
                                                            ,<<"Callee-ID-Name">>
                                                            ,<<"Callee-ID-Number">>
                                                            ,<<"Caller-ID-Name">>
                                                            ,<<"Caller-ID-Number">>
                                                            ,<<"Fax-Error">>
                                                            ,<<"Fax-Info">>
                                                            ,<<"Fax-ID">>
                                                            ,<<"Fax-Notifications">>
                                                            ,<<"Fax-Timestamp">>
                                                            ,<<"FaxBox-ID">>
                                                            ,<<"Owner-ID">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Fax Outbound Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec outbound_fax_definition() -> kapi_definition:api().
outbound_fax_definition() ->
    EventName = <<"outbound_fax">>,
    Category = <<"fax">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Successful Fax Transmission">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a fax is successfully transmitted">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun fax_outbound/1}
              ,{fun kapi_definition:set_validate_fun/2, fun fax_outbound_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_fax_outbound/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"outbound">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'outbound_fax'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Callee-ID-Number">>
                                                            ,<<"Caller-ID-Number">>
                                                            ,<<"Fax-ID">>
                                                            ,<<"Fax-JobId">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Call-ID">>
                                                            ,<<"Callee-ID-Name">>
                                                            ,<<"Caller-ID-Name">>
                                                            ,<<"Fax-Info">>
                                                            ,<<"Fax-Notifications">>
                                                            ,<<"Fax-Timestamp">>
                                                            ,<<"FaxBox-ID">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Fax Outbound Notification Error API definition.
%% @end
%%------------------------------------------------------------------------------
-spec outbound_fax_error_definition() -> kapi_definition:api().
outbound_fax_error_definition() ->
    EventName = <<"outbound_fax_error">>,
    Category = <<"fax">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Fax Transmission Error">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when transmitting a fax fails">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun fax_outbound_error/1}
              ,{fun kapi_definition:set_validate_fun/2, fun fax_outbound_error_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_fax_outbound_error/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"outbound_error">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'outbound_fax_error'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Fax-ID">>
                                                            ,<<"Fax-JobId">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Call-ID">>
                                                            ,<<"Callee-ID-Name">>
                                                            ,<<"Callee-ID-Number">>
                                                            ,<<"Caller-ID-Name">>
                                                            ,<<"Caller-ID-Number">>
                                                            ,<<"Fax-Error">>
                                                            ,<<"Fax-Info">>
                                                            ,<<"Fax-Notifications">>
                                                            ,<<"Fax-Timestamp">>
                                                            ,<<"FaxBox-ID">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Fax Outbound Notification SMTP Error API definition.
%% @end
%%------------------------------------------------------------------------------
-spec outbound_smtp_fax_error_definition() -> kapi_definition:api().
outbound_smtp_fax_error_definition() ->
    EventName = <<"outbound_smtp_fax_error">>,
    Category = <<"fax">>,
    ErrorsFun = fun(L) when is_list(L) -> kz_term:is_not_empty(L);
                   (_) -> 'false'
                end,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Invalid Email-to-Fax Email">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when the received email-to-fax email is invalid">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun fax_outbound_smtp_error/1}
              ,{fun kapi_definition:set_validate_fun/2, fun fax_outbound_smtp_error_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_fax_outbound_smtp_error/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"outbound_smtp_error">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'outbound_smtp_fax_error'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Fax-From-Email">>
                                                            ,<<"Errors">>
                                                            ,<<"Timestamp">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Fax-To-Email">>
                                                            ,<<"FaxBox-ID">>
                                                            ,<<"FaxBox-Name">>
                                                            ,<<"FaxBox-Timezone">>
                                                            ,<<"Number">>
                                                            ,<<"Owner-ID">>
                                                            ,<<"Original-Number">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, [{<<"Errors">>, ErrorsFun}]}
              ],
    kapi_definition:setters(Setters).

%%%=============================================================================
%%% Number and Port Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Cnam Request Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec cnam_request_definition() -> kapi_definition:api().
cnam_request_definition() ->
    EventName = <<"cnam_request">>,
    Category = <<"number">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"CNAM Update">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when an end user would like the CNAM for a number changed">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun cnam_request/1}
              ,{fun kapi_definition:set_validate_fun/2, fun cnam_request_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_cnam_request/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"cnam_request">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'cnam_request'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Number">>
                                                            ,<<"Cnam">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Acquired-For">>
                                                            ,<<"Local-Number">>
                                                            ,<<"Number-State">>
                                                            ,<<"Request">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

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
    EventName = <<"port_cancel">>,
    Category = <<"number">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Port Cancel">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a port request is canceled">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun port_cancel/1}
              ,{fun kapi_definition:set_validate_fun/2, fun port_cancel_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_port_cancel/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"port_cancel">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'port_cancel'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Port-Request-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, ?PORT_OPTIONAL_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, [{<<"Reason">>, fun kz_json:is_json_object/1}]}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Port Comment Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec port_comment_definition() -> kapi_definition:api().
port_comment_definition() ->
    EventName = <<"port_comment">>,
    Category = <<"number">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Port Comment">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a comment is left on a port request">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun port_comment/1}
              ,{fun kapi_definition:set_validate_fun/2, fun port_comment_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_port_comment/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"port_comment">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'port_comment'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Port-Request-ID">>
                                                            ,<<"Comment">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, ?PORT_OPTIONAL_HEADERS -- [<<"Reason">>]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Port Pending Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec port_pending_definition() -> kapi_definition:api().
port_pending_definition() ->
    EventName = <<"port_pending">>,
    Category = <<"number">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Port Pending">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a port request is accepted and submitted to a carrier">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun port_pending/1}
              ,{fun kapi_definition:set_validate_fun/2, fun port_pending_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_port_pending/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"port_pending">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'port_pending'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Port-Request-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, ?PORT_OPTIONAL_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, [{<<"Reason">>, fun kz_json:is_json_object/1}]}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Port Rejected Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec port_rejected_definition() -> kapi_definition:api().
port_rejected_definition() ->
    EventName = <<"port_rejected">>,
    Category = <<"number">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Port Rejected">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a port request is rejected">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun port_rejected/1}
              ,{fun kapi_definition:set_validate_fun/2, fun port_rejected_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_port_rejected/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"port_rejected">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'port_rejected'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Port-Request-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, ?PORT_OPTIONAL_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, [{<<"Reason">>, fun kz_json:is_json_object/1}]}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Port Request Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec port_request_definition() -> kapi_definition:api().
port_request_definition() ->
    EventName = <<"port_request">>,
    Category = <<"number">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Port Request">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a port is submitted for processing">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun port_request/1}
              ,{fun kapi_definition:set_validate_fun/2, fun port_request_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_port_request/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"port_request">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'port_request'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Port-Request-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, ?PORT_OPTIONAL_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, [{<<"Reason">>, fun kz_json:is_json_object/1}]}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Port Scheduled Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec port_scheduled_definition() -> kapi_definition:api().
port_scheduled_definition() ->
    EventName = <<"port_scheduled">>,
    Category = <<"number">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Port Scheduled">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a port is accepted by a carrier and scheduled">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun port_scheduled/1}
              ,{fun kapi_definition:set_validate_fun/2, fun port_scheduled_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_port_scheduled/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"port_scheduled">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'port_scheduled'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Port-Request-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, ?PORT_OPTIONAL_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, [{<<"Reason">>, fun kz_json:is_json_object/1}]}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Port Unconfirmed Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec port_unconfirmed_definition() -> kapi_definition:api().
port_unconfirmed_definition() ->
    EventName = <<"port_unconfirmed">>,
    Category = <<"number">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Port Unconfirmed">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a port is created, prior to submitting">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun port_unconfirmed/1}
              ,{fun kapi_definition:set_validate_fun/2, fun port_unconfirmed_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_port_unconfirmed/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"port_unconfirmed">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'port_unconfirmed'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Port-Request-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, ?PORT_OPTIONAL_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, [{<<"Reason">>, fun kz_json:is_json_object/1}]}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Ported API Notification definition.
%% @end
%%------------------------------------------------------------------------------
-spec ported_definition() -> kapi_definition:api().
ported_definition() ->
    EventName = <<"ported">>,
    Category = <<"number">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Ported">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a port request for number is completed">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun ported/1}
              ,{fun kapi_definition:set_validate_fun/2, fun ported_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_ported/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"ported">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'ported'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Port-Request-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, ?PORT_OPTIONAL_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, [{<<"Reason">>, fun kz_json:is_json_object/1}]}
              ],
    kapi_definition:setters(Setters).

%%%=============================================================================
%%% Register Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Denied Emergency Notification Bridge API definition.
%% @end
%%------------------------------------------------------------------------------
-spec denied_emergency_bridge_definition() -> kapi_definition:api().
denied_emergency_bridge_definition() ->
    EventName = <<"denied_emergency_bridge">>,
    Category = <<"registration">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Emergency Call Failed">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a call to an number classified as emergency fails">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun denied_emergency_bridge/1}
              ,{fun kapi_definition:set_validate_fun/2, fun denied_emergency_bridge_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_denied_emergency_bridge/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"denied_emergency_bridge">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'denied_emergency_bridge'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Call-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Emergency-Caller-ID-Name">>
                                                            ,<<"Emergency-Caller-ID-Number">>
                                                            ,<<"Outbound-Caller-ID-Name">>
                                                            ,<<"Outbound-Caller-ID-Number">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%%=============================================================================
%%% SIP Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Deregister API Notification definition.
%% @end
%%------------------------------------------------------------------------------
-spec deregister_definition() -> kapi_definition:api().
deregister_definition() ->
    EventName = <<"deregister">>,
    Category = <<"sip">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"De-Registration">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a device fails to re-register and the contact expires">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun deregister/1}
              ,{fun kapi_definition:set_validate_fun/2, fun deregister_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_deregister/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"deregister">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'deregister'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Username">>
                                                            ,<<"Realm">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Account-DB">>
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
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get First Occurrence Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec first_occurrence_definition() -> kapi_definition:api().
first_occurrence_definition() ->
    EventName = <<"first_occurrence">>,
    Category = <<"sip">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Account First Occurrence">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when an end user registers the first device and/or places the first call on an account">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun first_occurrence/1}
              ,{fun kapi_definition:set_validate_fun/2, fun first_occurrence_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_first_occurrence/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"first_occurrence">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'first_occurrence'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Occurrence">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, ?DEFAULT_OPTIONAL_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Missed Call Notification Alert API definition.
%% @end
%%------------------------------------------------------------------------------
-spec missed_call_definition() -> kapi_definition:api().
missed_call_definition() ->
    EventName = <<"missed_call">>,
    Category = <<"sip">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Missed Call">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when an corresponding missed call action in a callflow is invoked">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun missed_call/1}
              ,{fun kapi_definition:set_validate_fun/2, fun missed_call_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_missed_call/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"missed_call">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'missed_call'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Call-Bridged">>
                                                            ,<<"Message-Left">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Caller-ID-Name">>
                                                            ,<<"Caller-ID-Number">>
                                                            ,<<"From-Realm">>
                                                            ,<<"From-User">>
                                                            ,<<"Notify">>
                                                            ,<<"To">>
                                                            ,<<"To-Realm">>
                                                            ,<<"To-User">>
                                                            ,<<"Timestamp">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Register API Notification definition.
%% @end
%%------------------------------------------------------------------------------
-spec register_definition() -> kapi_definition:api().
register_definition() ->
    EventName = <<"register">>,
    Category = <<"sip">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Registration">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a device registers but is not currently registered">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun register/1}
              ,{fun kapi_definition:set_validate_fun/2, fun register_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_register/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"register">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'register'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Username">>
                                                            ,<<"Realm">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Account-DB">>
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
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%%=============================================================================
%%% System Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get System Alert Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec system_alert_definition() -> kapi_definition:api().
system_alert_definition() ->
    EventName = <<"system_alert">>,
    Category = <<"system">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"System Alert">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered to alert the system administrators">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun system_alert/1}
              ,{fun kapi_definition:set_validate_fun/2, fun system_alert_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_system_alert/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"alert">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'system_alert'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Message">>
                                                            ,<<"Subject">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Details">>
                                                            ,<<"Line">>
                                                            ,<<"kapi_definitionule">>
                                                            ,<<"Node">>
                                                            ,<<"Pid">>
                                                            ,<<"Request-ID">>
                                                            ,<<"Section">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%%=============================================================================
%%% User Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Customer Update Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec customer_update_definition() -> kapi_definition:api().
customer_update_definition() ->
    EventName = <<"customer_update">>,
    Category = <<"user">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Customer Update">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when the customer update API is used to deliver a message to the account">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun customer_update/1}
              ,{fun kapi_definition:set_validate_fun/2, fun customer_update_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_customer_update/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"customer_update">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'customer_update'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"DataBag">>
                                                            ,<<"Recipient-ID">>
                                                            ,<<"Template-ID">>
                                                            ,<<"User-Type">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get New User Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec new_user_definition() -> kapi_definition:api().
new_user_definition() ->
    EventName = <<"new_user">>,
    Category = <<"user">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"New User">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when an end user creates a new user">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun new_user/1}
              ,{fun kapi_definition:set_validate_fun/2, fun new_user_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_new_user/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"new">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'new_user'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"User-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Password">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Password Recovery Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec password_recovery_definition() -> kapi_definition:api().
password_recovery_definition() ->
    EventName = <<"password_recovery">>,
    Category = <<"user">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Password Recovery">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when an end user requests a password recovery link">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun password_recovery/1}
              ,{fun kapi_definition:set_validate_fun/2, fun password_recovery_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_password_recovery/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"password_recovery">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'password_recovery'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Email">>
                                                            ,<<"Password-Reset-Link">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Account-DB">>
                                                            ,<<"First-Name">>
                                                            ,<<"Last-Name">>
                                                            ,<<"Timezone">>
                                                            ,<<"User-ID">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).


%%%=============================================================================
%%% Voicemail Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Voicemail full Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec voicemail_full_definition() -> kapi_definition:api().
voicemail_full_definition() ->
    EventName = <<"voicemail_full">>,
    Category = <<"voicemail">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Voicemail Box Full">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered any time an attempt to leave a voicemail message is blocked because the voicemail box is full">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun voicemail_full/1}
              ,{fun kapi_definition:set_validate_fun/2, fun voicemail_full_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_voicemail_full/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"full">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'voicemail_full'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Max-Message-Count">>
                                                            ,<<"Message-Count">>
                                                            ,<<"Voicemail-Box">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, ?DEFAULT_OPTIONAL_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

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
%%------------------------------------------------------------------------------
%% @doc Get Voicemail New Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec voicemail_new_definition() -> kapi_definition:api().
voicemail_new_definition() ->
    EventName = <<"voicemail_new">>,
    Category = <<"voicemail">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"New Voicemail Message">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered any time a voicemail message is left">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun voicemail_new/1}
              ,{fun kapi_definition:set_validate_fun/2, fun voicemail_new_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_voicemail_new/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"new">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'voicemail_new'}
              ,{fun kapi_definition:set_required_headers/2, ?VOICEMAIL_NEW_HEADERS}
              ,{fun kapi_definition:set_optional_headers/2, ?OPTIONAL_VOICEMAIL_NEW_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Get Voicemail Saved Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec voicemail_saved_definition() -> kapi_definition:api().
voicemail_saved_definition() ->
    EventName = <<"voicemail_saved">>,
    Category = <<"voicemail">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Voicemail Message Saved">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered any time a voicemail message is saved in the voicemail box 'new' folder">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun voicemail_saved/1}
              ,{fun kapi_definition:set_validate_fun/2, fun voicemail_saved_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_voicemail_saved/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"saved">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'voicemail_saved'}
              ,{fun kapi_definition:set_required_headers/2, ?VOICEMAIL_NEW_HEADERS}
              ,{fun kapi_definition:set_optional_headers/2, ?OPTIONAL_VOICEMAIL_NEW_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%%=============================================================================
%%% Webhook Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get Webhook Callflow Notification API definition.
%% @end
%%------------------------------------------------------------------------------
-spec webhook_definition() -> kapi_definition:api().
webhook_definition() ->
    EventName = <<"webhook">>,
    Category = <<"webhook">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Callflow Webhook Triggered">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a corresponding webhook action in a callflow is reached">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun webhook/1}
              ,{fun kapi_definition:set_validate_fun/2, fun webhook_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_webhook/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"callflow">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'webhook'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Data">>
                                                            ,<<"Hook">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Timestamp">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%------------------------------------------------------------------------------
%% @doc Notification Get Webhook Disabled API definition.
%% @end
%%------------------------------------------------------------------------------
-spec webhook_disabled_definition() -> kapi_definition:api().
webhook_disabled_definition() ->
    EventName = <<"webhook_disabled">>,
    Category = <<"webhook">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Webhook Disabled">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when a webhook is disabled">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun webhook_disabled/1}
              ,{fun kapi_definition:set_validate_fun/2, fun webhook_disabled_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_webhook_disabled/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"disabled">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'webhook_disabled'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Hook-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, ?DEFAULT_OPTIONAL_HEADERS}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

%%%=============================================================================
%%% Customer Defined Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get customer defined notification.
%% @end
%%------------------------------------------------------------------------------
-spec cf_notification_definition() -> kapi_definition:api().
cf_notification_definition() ->
    EventName = <<"cf_notification">>,
    Category = <<"account">>,
    Setters = [{fun kapi_definition:set_name/2, EventName}
              ,{fun kapi_definition:set_friendly_name/2, <<"Customer defined notification">>}
              ,{fun kapi_definition:set_description/2
               ,<<"This event is triggered when an customer want send own notification, as example from callflow">>
               }
              ,{fun kapi_definition:set_category/2, Category}
              ,{fun kapi_definition:set_build_fun/2, fun cf_notification/1}
              ,{fun kapi_definition:set_validate_fun/2, fun cf_notification_v/1}
              ,{fun kapi_definition:set_publish_fun/2, fun publish_cf_notification/1}
              ,{fun kapi_definition:set_binding/2, ?BINDING_STRING(Category, <<"cf_notification">>)}
              ,{fun kapi_definition:set_restrict_to/2, 'cf_notification'}
              ,{fun kapi_definition:set_required_headers/2, [<<"Account-ID">>
                                                            ,<<"Template-ID">>
                                                            ]}
              ,{fun kapi_definition:set_optional_headers/2, [<<"Caller-ID-Name">>
                                                            ,<<"Caller-ID-Number">>
                                                            ,<<"Call-ID">>
                                                            ,<<"Call-Bridged">>
                                                            ,<<"Message-Left">>
                                                            ,<<"From-Realm">>
                                                            ,<<"From-User">>
                                                            ,<<"Notify">>
                                                            ,<<"To">>
                                                            ,<<"To-Realm">>
                                                            ,<<"To-User">>
                                                            ,<<"Timestamp">>
                                                            ,<<"Comments">>
                                                            ,<<"Notification-Media">>
                                                                 | ?DEFAULT_OPTIONAL_HEADERS
                                                            ]}
              ,{fun kapi_definition:set_values/2, ?NOTIFY_VALUES(EventName)}
              ,{fun kapi_definition:set_types/2, []}
              ],
    kapi_definition:setters(Setters).

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
    ,cf_notification_definition()
    ].

%%------------------------------------------------------------------------------
%% @doc Get API definition of the given `Name'.
%% @see api_definitions/0
%% @end
%%------------------------------------------------------------------------------
-spec api_definition(kz_term:text()) -> kapi_definition:api().
api_definition(Name) when not is_binary(Name) ->
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
api_definition(<<"cf_notification">>) ->
    cf_notification_definition().

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
        'error':'undef' ->
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
        'error':'undef' ->
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
        <<Id/binary>> -> Id;
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
                AccountId -> kzs_util:format_account_db(AccountId)
            end;
        ?MATCH_MODB_SUFFIX_RAW(_, _, _)=Db -> maybe_strict_modb(Db, StrictMODB);
        ?MATCH_MODB_SUFFIX_UNENCODED(_, _, _)=Db -> maybe_strict_modb(Db, StrictMODB);
        ?MATCH_MODB_SUFFIX_ENCODED(_, _, _)=Db -> maybe_strict_modb(Db, StrictMODB);
        ?MATCH_ACCOUNT_RAW(_)=Db -> kzs_util:format_account_db(Db);
        ?MATCH_ACCOUNT_UNENCODED(_)=Db -> kzs_util:format_account_db(Db);
        ?MATCH_ACCOUNT_ENCODED(_)=Db -> kzs_util:format_account_db(Db);
        ?MATCH_ACCOUNT_encoded(_)=Db -> kzs_util:format_account_db(Db);
        OtherDb -> OtherDb
    end.

-spec maybe_strict_modb(kz_term:ne_binary(), boolean()) -> kz_term:ne_binary().
maybe_strict_modb(Db, 'true') ->
    kzs_util:format_account_modb(Db, 'encoded');
maybe_strict_modb(Db, 'false') ->
    kzs_util:format_account_db(Db).

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
        'error':'undef' ->
            lager:warning("no notification headers for ~s", [Name]),
            []
    end.

%%------------------------------------------------------------------------------
%% @doc Generic function to build API payload.
%% @end
%%------------------------------------------------------------------------------
-spec build_message(kz_term:api_terms(), kapi_definition:api()) -> api_formatter_return().
build_message(Prop, Definition) when is_list(Prop) ->
    ReqH = kapi_definition:required_headers(Definition),
    OptH = kapi_definition:optional_headers(Definition),
    Validate = kapi_definition:validate_fun(Definition),
    Binding = kapi_definition:binding(Definition),
    case Validate(Prop) of
        'true' -> kz_api:build_message(Prop, ReqH, OptH);
        'false' -> {'error', "Proplist failed validation for " ++ binary_to_list(Binding)}
    end;
build_message(JObj, Definition) ->
    build_message(kz_json:to_proplist(JObj), Definition).

%%------------------------------------------------------------------------------
%% @doc Generic function to validate API payload.
%% @end
%%------------------------------------------------------------------------------
validate(Prop, Definition) when is_list(Prop) ->
    ReqH = kapi_definition:required_headers(Definition),
    Values = kapi_definition:values(Definition),
    Types = kapi_definition:types(Definition),
    kz_api:validate(Prop, ReqH, Values, Types);
validate(JObj, Definition) ->
    validate(kz_json:to_proplist(JObj), Definition).

%%%=============================================================================
%%% Internal Notifications Functions
%%%=============================================================================


%%------------------------------------------------------------------------------
%% @doc Notify Status notification.
%% Takes proplist, creates JSON string and publish it on AMQP.
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
    Values = kapi_definition:values(notify_update_definition()),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun notify_update/1),
    kz_amqp_util:targeted_publish(RespQ, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Skeleton notification
%% Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = skel_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun skel/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%%=============================================================================
%%% Account Notifications Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = account_zone_change_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun account_zone_change/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = bill_reminder_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun bill_reminder/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = low_balance_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun low_balance/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = new_account_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun new_account/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = service_added_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun service_added/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = topup_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun topup/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = transaction_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun transaction/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%%=============================================================================
%%% Fax Notifications Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = inbound_fax_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun fax_inbound/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = inbound_fax_error_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun fax_inbound_error/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = outbound_fax_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun fax_outbound/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = outbound_fax_error_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun fax_outbound_error/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = outbound_smtp_fax_error_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun fax_outbound_smtp_error/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%%=============================================================================
%%% Number and Port Notifications Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = cnam_request_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun cnam_request/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = port_cancel_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun port_cancel/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = port_comment_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun port_comment/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = port_pending_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun port_pending/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = port_rejected_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun port_rejected/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = port_request_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun port_request/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = port_scheduled_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun port_scheduled/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = port_unconfirmed_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun port_unconfirmed/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = ported_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun ported/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%%=============================================================================
%%% Register Notifications Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = denied_emergency_bridge_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun denied_emergency_bridge/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%%=============================================================================
%%% Register Notifications Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = deregister_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun deregister/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = first_occurrence_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun first_occurrence/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = missed_call_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun missed_call/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = register_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun register/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%%=============================================================================
%%% System Notifications Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = system_alert_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun system_alert/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%%=============================================================================
%%% User Notifications Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = customer_update_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun customer_update/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = new_user_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun new_user/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = password_recovery_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun password_recovery/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%%=============================================================================
%%% Voicemail Notifications Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = voicemail_full_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun voicemail_full/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = voicemail_new_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun voicemail_new/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = voicemail_saved_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun voicemail_saved/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%%=============================================================================
%%% Webhook Notifications Functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = webhook_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun webhook/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
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
    Definition = webhook_disabled_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun webhook_disabled/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).

%%%=============================================================================
%%% Customer Defined Notifications Definitions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Takes proplist, creates JSON string and publish it on AMQP.
%% @end
%%------------------------------------------------------------------------------
-spec cf_notification(kz_term:api_terms()) -> api_formatter_return().
cf_notification(Prop) ->
    build_message(Prop, cf_notification_definition()).

-spec cf_notification_v(kz_term:api_terms()) -> boolean().
cf_notification_v(Prop) ->
    validate(Prop, cf_notification_definition()).

-spec publish_cf_notification(kz_term:api_terms()) -> 'ok'.
publish_cf_notification(JObj) ->
    publish_cf_notification(JObj, ?DEFAULT_CONTENT_TYPE).

-spec publish_cf_notification(kz_term:api_terms(), kz_term:ne_binary()) -> 'ok'.
publish_cf_notification(API, ContentType) ->
    Definition = cf_notification_definition(),
    Binding = kapi_definition:binding(Definition),
    Values = kapi_definition:values(Definition),
    {'ok', Payload} = kz_api:prepare_api_payload(API, Values, fun cf_notification/1),
    kz_amqp_util:notifications_publish(Binding, Payload, ContentType).
