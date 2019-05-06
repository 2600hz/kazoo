# Notification events

This events will fire when a notification event is triggered in Kazoo. These are the same events as Email notifications you get. They also proccessed by Teletype application to send Email and their Email templates are configurable within Branding application in UI.

These events usally _may_ have some common fields which are explained in the next section.

Please do note that most of the fields should be present on the payload, but they could also be missing based on the available information at the time of the event was occured.

## Info

* **Name:** notifications
* **Friendly Name:** Notifications Webhook

## Modifiers

* **type:** Any of teletype notifications, see below for a list of all notifications.

## Base Notifications Fields

### Required Fields

Notifications events are basically AMQP message which triggred by some application in Kazoo. So their payload always have the Kazoo standard AMQP fields:

* `app_name`: The application's name where this event was triggred
* `app_version`: The application's version where this event was triggred
* `event_category`: The event category, which is always set `notification` for these events
* `event_name`: The name of the triggred event
* `msg_id`: Message ID
* `node`: The node where this event is generated
* `server_id`: The server ID where this event is generated
* `system_log_id`: System log id if available

### Optional Fields

All these fields are can be present in the event. Most of them are indocating that this event is just to trigger Email template render to test the template or get a preview of them (fields like `preview`, `html`/`text`). Fields like `to`, `cc` are Email addresse(s) for sending the event to specific Emails (based on their Email template configuration).

* `account_db` (`string`): Account-DB related to the type of the event
* `attachment_url` (`string`): Attachment URL iff the event has an attachment
* `bcc` (`string` or a list of `string` or `object`): Usually if the event is preview, Branding application can set `bcc`, `cc` and `to` accordingly to send a preview of the notification templates
* `cc` (`string` or a list of `string` or `object`): Usually if the event is preview, Branding application can set `bcc`, `cc` and `to` accordingly to send a preview of the notification templates
* `from` (`string` or a list of `string` or `object`): Usually if the event is preview, Branding application can set `bcc`, `cc` and `to` accordingly to send a preview of the notification templates
* `html` (`string`): Usually if the event is preview, Branding application can set `html` and/or `text` of the template to use for rendering the email notification
* `preview` (`boolean`): Usually set by Branding application to indicate this event is for previewing of supplied HTML or Plain Text email template
* `reply_to` (`string` or a list of `string` or `object`): Usually if the event is preview, Branding application can set `bcc`, `cc` and `to` accordingly to send a preview of the notification templates
* `subject` (`string`): Usually set by Branding application to use when the event is for previewing of supplied HTML or Plain Text email template
* `text` (`string`): Usually set by Branding application to indicate this event is for previewing of supplied HTML or Plain Text email template
* `to` (`string` or a list of `string` or `object`): Usually if the event is preview, Branding application can set `bcc`, `cc` and `to` accordingly to send a preview of the notification templates


## Account Zone Change: account_zone_change

This event is triggered when an end user requests the home zone of an account is changed.

### Possible fields

* `account_id`
* `event_name`: `account_zone_change`
* `zones`

## Bill Reminder: bill_reminder

This event is triggered before a few days before the end of the month to remind account's owners of estimated service plan charges.

### Possible fields

* `account_id`
* `due_date`
* `event_name`: `bill_reminder`
* `items`
* `payment_token`
* `timestamp`

## Customer defined notification: cf_notification

This event is triggered when an customer want send own notification, as example from callflow.

### Possible fields

* `account_id`
* `call_bridged`
* `call_id`
* `caller_id_name`
* `caller_id_number`
* `comments`: Text message that need to include into notification
* `event_name`: `cf_notification`
* `from_realm`
* `from_user`
* `message_left`
* `notification_media`: What is media need to use for notification, possible values: `email` and `sms`
* `notify`: Arbitrary data related to the call
* `template_id`
* `timestamp`
* `to_realm`
* `to_user`
* `to`

## CNAM Update: cnam_request

This event is triggered when an end user would like the CNAM for a number changed.

### Possible fields

* `account_id`
* `acquired_for`
* `cnam`
* `event_name`: `cnam_request`
* `local_number`
* `number`
* `number_state`

## Customer Update: customer_update

This event is triggered when the customer update API is used to deliver a message to the account.

### Possible fields

* `account_id`
* `databag`: Optional arbitrary data sent when event was triggred
* `event_name`: `customer_update`
* `recipient_id`: Optional destination account id if set, otherwise Email will be sent to all account childrens of the `account_id` above
* `template_id`: Source of the template to render
* `user_type`: Optional, could be a single user ID, otherwise if set to `all_users` will sent to all user types (admin and regular). If missing will send to admin users only

## Emergency Call Failed: denied_emergency_bridge

This event is triggered when a call to an number classified as emergency fails.

### Possible fields

* `account_id`
* `call_id`
* `emergency_caller_id_name`
* `emergency_caller_id_number`
* `event_name`: `denied_emergency_bridge`
* `outbound_caller_id_name`
* `outbound_caller_id_number`

## De-Registration: deregister

This event is triggered when a device fails to re-register and the contact expires.

### Possible fields

* `account_db`
* `account_id`
* `authorizing_id`
* `call_id`
* `contact`
* `event_name`: `deregister`
* `event_timestamp`
* `expires`
* `freeswitch_hostname`
* `from_host`
* `from_user`
* `network_ip`
* `network_port`
* `presence_hosts`
* `profile_name`
* `rpid`
* `realm`
* `status`
* `suppress_unregister_notify`
* `to_host`
* `to_user`
* `user_agent`
* `username`

## Account First Occurrence: first_occurrence

This event is triggered when an end user registers the first device and/or places the first call on an account.

### Possible fields

* `account_id`
* `event_name`: `first_occurrence`
* `occurrence`: Type of occurrence, possible values: `registration`, `call`

## Successful Fax Reception: inbound_fax

This event is triggered when a fax is successfully received.

### Possible fields

* `account_id`
* `call_id`
* `callee_id_name`
* `callee_id_number`
* `caller_id_name`
* `caller_id_number`
* `event_name`: `inbound_fax`
* `fax_id`
* `fax_info`: Arbitrary internal data related to fax application
* `fax_notifications`
* `fax_timestamp`
* `faxbox_id`
* `from_realm`
* `from_user`
* `owner_id`
* `to_realm`
* `to_user`

## Fax Reception Error: inbound_fax_error

This event is triggered when receiving a fax fails.

### Possible fields

* `account_id`
* `call_id`
* `callee_id_name`
* `callee_id_number`
* `caller_id_name`
* `caller_id_number`
* `event_name`: `inbound_fax_error`
* `fax_error`
* `fax_id`
* `fax_info`: Arbitrary internal data related to fax application
* `fax_notifications`
* `fax_result_code`
* `fax_timestamp`
* `faxbox_id`
* `from_realm`
* `from_user`
* `owner_id`
* `to_realm`
* `to_user`

## Account Low Balance: low_balance

This event is triggered when an account is found with a balance below the notification threshold.

### Possible fields

* `account_id`
* `current_balance`
* `event_name`: `low_balance`

## Missed Call: missed_call

This event is triggered when an corresponding missed call action in a callflow is invoked.

### Possible fields

* `account_id`
* `call_bridged`
* `call_id`
* `caller_id_name`
* `caller_id_number`
* `event_name`: `missed_call`
* `from_realm`
* `from_user`
* `message_left`
* `notify`: Arbitrary data related to call
* `timestamp`
* `to_realm`
* `to_user`
* `to`

## New Account: new_account

This event is triggered when an end user creates a new account.

### Possible fields

* `account_api_key`
* `account_db`
* `account_id`
* `account_name`
* `account_realm`
* `event_name`: `new_account`

## New User: new_user

This event is triggered when an end user creates a new user.

### Possible fields

* `account_id`
* `event_name`: `new_user`
* `password`
* `user_id`

## Successful Fax Transmission: outbound_fax

This event is triggered when a fax is successfully transmitted.

### Possible fields

* `account_id`
* `call_id`
* `callee_id_name`
* `callee_id_number`
* `caller_id_name`
* `caller_id_number`
* `event_name`: `outbound_fax`
* `fax_id`
* `fax_info`: Arbitrary data related to Fax application
* `fax_jobid`
* `fax_notifications`
* `fax_timestamp`
* `faxbox_id`

## Fax Transmission Error: outbound_fax_error

This event is triggered when transmitting a fax fails.

### Possible fields

* `account_id`
* `call_id`
* `callee_id_name`
* `callee_id_number`
* `caller_id_name`
* `caller_id_number`
* `event_name`: `outbound_fax_error`
* `fax_error`
* `fax_id`
* `fax_info`: Arbitrary data related to Fax application
* `fax_jobid`
* `fax_notifications`
* `fax_timestamp`
* `faxbox_id`

## Invalid Email-to-Fax Email: outbound_smtp_fax_error

This event is triggered when the received email-to-fax email is invalid.

### Possible fields

* `account_id`
* `errors`: List of encountered errors during recieving the email
* `event_name`: `outbound_smtp_fax_error`
* `fax_from_email`
* `fax_to_email`
* `faxbox_id`
* `faxbox_name`
* `faxbox_timezone`
* `number`
* `original_number`
* `owner_id`
* `timestamp`

## Password Recovery: password_recovery

This event is triggered when an end user requests a password recovery link.

### Possible fields

* `account_db`
* `account_id`
* `email`
* `event_name`: `password_recovery`
* `first_name`
* `last_name`
* `password_reset_link`
* `timezone`
* `user_id`

## Port Cancel: port_cancel

This event is triggered when a port request is canceled.

### Possible fields

* `account_id`
* `authorized_by`
* `event_name`: `port_cancel`
* `local_number`
* `number`
* `number_state`
* `port`
* `port_request_id`
* `reason`

## Port Comment: port_comment

This event is triggered when a comment is left on a port request.

### Possible fields

* `account_id`
* `authorized_by`
* `comment`
* `event_name`: `port_comment`
* `local_number`
* `number`
* `number_state`
* `port`
* `port_request_id`

## Port Pending: port_pending

This event is triggered when a port request is accepted and submitted to a carrier.

### Possible fields

* `account_id`
* `authorized_by`
* `event_name`: `port_pending`
* `local_number`
* `number`
* `number_state`
* `port`
* `port_request_id`
* `reason`

## Port Rejected: port_rejected

This event is triggered when a port request is rejected.

### Possible fields

* `account_id`
* `authorized_by`
* `event_name`: `port_rejected`
* `local_number`
* `number`
* `number_state`
* `port`
* `port_request_id`
* `reason`

## Port Request: port_request

This event is triggered when a port is submitted for processing.

### Possible fields

* `account_id`
* `authorized_by`
* `event_name`: `port_request`
* `local_number`
* `number`
* `number_state`
* `port`
* `port_request_id`
* `reason`
* `version`

## Port Scheduled: port_scheduled

This event is triggered when a port is accepted by a carrier and scheduled.

### Possible fields

* `account_id`
* `authorized_by`
* `event_name`: `port_scheduled`
* `local_number`
* `number`
* `number_state`
* `port`
* `port_request_id`
* `reason`

## Port Unconfirmed: port_unconfirmed

This event is triggered when a port is created, prior to submitting.

### Possible fields

* `account_id`
* `authorized_by`
* `event_name`: `port_unconfirmed`
* `local_number`
* `number`
* `number_state`
* `port`
* `port_request_id`
* `reason`

## Ported: ported

This event is triggered when a port request for number is completed.

### Possible fields

* `account_id`
* `authorized_by`
* `event_name`: `ported`
* `local_number`
* `number`
* `number_state`
* `port`
* `port_request_id`
* `reason`

## Registration: register

This event is triggered when a device registers but is not currently registered.

### Possible fields

* `account_db`
* `account_id`
* `authorizing_id`
* `authorizing_type`
* `call_id`
* `contact`
* `event_name`: `register`
* `event_timestamp`
* `expires`
* `from_host`
* `from_user`
* `network_ip`
* `network_port`
* `owner_id`
* `realm`
* `suppress_unregister_notify`
* `to_host`
* `to_user`
* `user_agent`
* `username`

## Service Added: service_added

This event is triggered when an account's billable quantities change.

### Possible fields

* `account_id`
* `audit_log`
* `event_name`: `service_added`
* `items`
* `timestamp`

## System Alert: system_alert

This event is triggered to alert the system administrators.

### Possible fields

* `account_id`
* `details`
* `event_name`: `system_alert`
* `line`
* `message`
* `module`
* `node`
* `pid`
* `request_id`
* `section`
* `subject`

## Automatic Account Top-up: topup

This event is triggered when an account automatic top-up is attempted.

Please note since Kazoo version 4.3 underlying billing engine is re-written and this event is not sent anymore. Instead you would get a transaction event instead.

### Possible fields

* `account_id`
* `add_ons`
* `amount`
* `billing_address`
* `card_last_four`
* `currency_code`
* `discounts`
* `event_name`: `top_up`
* `id`
* `purchase_order`
* `response`
* `success`
* `tax_amount`
* `timestamp`

## Transaction Completed: transaction

This event is triggered when a transaction is attempted.

### Possible fields

* `account_id`
* `add_ons`
* `amount`
* `billing_address`
* `card_last_four`
* `currency_code`
* `discounts`
* `event_name`: `transaction`
* `id`
* `purchase_order`
* `response`
* `service_plan`
* `success`
* `tax_amount`
* `timestamp`

## Voicemail Box Full: voicemail_full

This event is triggered any time an attempt to leave a voicemail message is blocked because the voicemail box is full.

### Possible fields

* `account_id`
* `event_name`: `voicemail_full`
* `max_message_count`
* `message_count`
* `voicemail_box`

## New Voicemail Message: voicemail_new

This event is triggered any time a voicemail message is left.

### Possible fields

* `account_id`
* `call_id`
* `caller_id_name`
* `caller_id_number`
* `event_name`: `voicemail_new`
* `from_realm`
* `from_user`
* `to_realm`
* `to_user`
* `voicemail_box`
* `voicemail_id`
* `voicemail_length`
* `voicemail_timestamp`
* `voicemail_transcription`

## Voicemail Message Saved: voicemail_saved

This event is triggered any time a voicemail message is saved in the voicemail box 'new' folder.

### Possible fields

* `account_id`
* `call_id`
* `caller_id_name`
* `caller_id_number`
* `event_name`: `voicemail_saved`
* `from_realm`
* `from_user`
* `to_realm`
* `to_user`
* `voicemail_box`
* `voicemail_id`
* `voicemail_length`
* `voicemail_timestamp`
* `voicemail_transcription`

## Callflow Webhook Triggered: webhook

This event is triggered when a corresponding webhook action in a callflow is reached.

### Possible fields

* `account_id`
* `data`: Arbitrary data related to the call
* `event_name`: `webhook`
* `hook`: On the fly webhook to proccess this event
* `timestamp`

## Webhook Disabled: webhook_disabled

This event is triggered when a webhook is disabled.

### Possible fields

* `account_id`
* `event_name`: `webhook_disabled`
* `hook_id`
