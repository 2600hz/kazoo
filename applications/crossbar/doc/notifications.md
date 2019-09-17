# Email Notifications Template

Allow managing templates for notification emails.

## About Notifications

Flowing is a basic structure of a notification object (here it is a digest of `voicemail_to_email` notification).

```json
{
  "enabled": true,
  "macros": {
    "voicemail.vmbox_id": {
      "i18n_label": "voicemail_vmbox_id",
      "friendly_name": "Voicemail Box Id",
      "description": "Which voicemail box was the message left in"
    }
  },
  "subject": "New voicemail from {{caller_id.name}} ({{caller_id.number}})",
  "category": "voicemail",
  "friendly_name": "Voicemail To Email",
  "to": {
    "type": "original"
  },
  "from": "no_reply@localhost.me",
  "cc": {
    "type": "specified",
    "email_addresses": []
  },
  "bcc": {
    "type": "specified",
    "email_addresses": []
  },
  "id": "voicemail_to_email",
  "account_overridden": true,
  "templates": {
    "text/plain": {
      "length": 971
    },
    "text/html": {
      "length": 13350
    }
  }
}
```

By looking at above structure, there are some points of interest to say about notification parameters:

* The `account_overridden` parameter would be added if the notification is account-specific; lack of the key indicates it is the system default notification.
* The `enabled` parameter indicates should system send an E-mail notifying of the causing event. Lacking of this parameter would considered as the notification is enabled.
* The `macros` object is a per-template, system-defined set of macros you can use in your templates. You **cannot** configure this via the API.

In addition to the JSON data describing configuration of a notification, notification templates can be represented in various formats and can be modified by uploading the representation document (such as using a WYSIWYG tool). Currently supported formats are plain text and HTML documents.

### Template Formats

Creating the configuration documents is all well and good, but it is necessary to be able to attach the templates in their various forms as well. Currently supported formats are `text/html` and `text/plain`.

### Operation considerations

In Kazoo versions prior to 3.19, notification templates were managed and processed by the `notify` application. In the newer Kazoo versions this has been replace by a robust and more featureful `teletype` application.

All accounts will continue to be processed by the `notify` application until the Crossbar notification APIs are accessed for the first time (for instance, when using the Branding application in Monster). Once a client has accessed the APIs, a flag is set on the account telling the `notify` application to ignore processing and instructs the `teletype` application to process it instead. This allows administrators to run both `notify` and `teletype` concurrently without sending multiple copies of each notification.

## Notifications Schema

#### Schema

Notifications templates



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`bcc.email_addresses.[]` |   | `string()` |   | `false` |  
`bcc.email_addresses` | BCC Email Addresses | `array(string())` |   | `false` |  
`bcc.type` |   | `string('original' | 'specified' | 'admins')` |   | `false` |  
`bcc` | Bcc email field | `object()` |   | `false` |  
`category` | Category of the template, for grouping purposes | `string(1..)` |   | `false` |  
`cc.email_addresses.[]` |   | `string()` |   | `false` |  
`cc.email_addresses` | CC Email Addresses | `array(string())` |   | `false` |  
`cc.type` |   | `string('original' | 'specified' | 'admins')` |   | `false` |  
`cc` | CC email field | `object()` |   | `false` |  
`enabled` | Enable notification | `boolean()` | `true` | `false` |  
`friendly_name` | Friendly name of the template | `string(1..)` |   | `false` |  
`from` | From: email address | `string()` |   | `true` |  
`macros` |   | `object()` | `{}` | `false` |  
`reply_to` | Reply-To: email address | `string()` |   | `false` |  
`subject` | Email subject | `string(1..200)` |   | `true` |  
`template_charset` |   | `string(1..)` | `utf-8` | `false` |  
`to.email_addresses.[]` |   | `string()` |   | `false` |  
`to.email_addresses` |   | `array(string())` |   | `false` |  
`to.type` |   | `string('original' | 'specified' | 'admins')` |   | `false` |  
`to` | To email field | `object()` |   | `true` |  



## Summary of Available System Notifications

Request to see what templates exist on the system to override.

> GET /v2/notifications

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications
```

**Responses**

```json
{
  "auth_token": "{AUTH_TOKEN}",
  "data": [
    {
      "id": "voicemail_to_email",
      "macros": {
        "call_id": {
          "description": "Call ID of the caller",
          "friendly_name": "Call ID",
          "i18n_label": "call_id"
        },
        "caller_id.name": {
          "description": "Caller ID Name",
          "friendly_name": "Caller ID Name",
          "i18n_label": "caller_id_name"
        }
        "..."
      }
    }
    "..."
  ],
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Summary of Account Overridden Notifications

To see what notification templates an account overrides. The key `account_overridden` will exist on any templates that are account-specific.

> GET /v2/accounts/{ACCOUNT_ID}/notifications

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications
```

##### Response

```json
{
  "auth_token": "{AUTH_TOKEN}",
  "data": [
    {
      "id": "voicemail_to_email",
      "macros": {
        "call_id": {
          "description": "Call ID of the caller",
          "friendly_name": "Call ID",
          "i18n_label": "call_id"
        },
        "caller_id.name": {
          "description": "Caller ID Name",
          "friendly_name": "Caller ID Name",
          "i18n_label": "caller_id_name"
        }
        "..."
      },
      "account_overridden": true
    }
    "..."
  ],
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Create a New System Notification

Creates a new system notification template.

!!! note
    Only a super duper admin can create/modify/delete system notifications!

> PUT /v2/notifications

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{
    "data": {
      "id":"voicemail_to_email",
      "to": {
        "type": "users",
        "email_addresses": ["user@account.com"]
      },
      "from": "reseller@resellerdomain.com",
      "subject": "Hello {{user.first_name}}, you received a new voicemail!",
      "enabled": true,
      "template_charset": "utf-8"
    }}' \
    http://{SERVER}:8000/v2/notifications
```

**Responses**

```json
{
  "auth_token": "{AUTH_TOKEN}",
  "data": {
    "enabled": true,
    "id": "voicemail_to_email",
    "from": "reseller@resellerdomain.com",
    "macros": { "..." },
    "subject": "Hello {{user.first_name}}, you received a new voicemail!",
    "template_charset": "utf-8",
    "to": {
      "email_addresses": [
        "user@account.com"
      ],
      "type": "users"
    }
  },
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Create a Notification as Account Override

Now that you've fetched the system default template, modify the template and PUT it back to the account.

!!! note
    This request will fail if `id` does not already exist in the system defaults.

> PUT /v2/accounts/{ACCOUNT_ID}/notifications

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{
    "data": {
      "id":"voicemail_to_email",
      "to": {
        "type": "users",
        "email_addresses": ["user@account.com"]
      },
      "from": "reseller@resellerdomain.com",
      "subject": "Hello {{user.first_name}}, you received a new voicemail!",
      "enabled": true,
      "template_charset": "utf-8"
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications
```

**Responses**

Same as `PUT` for system level, but with the `"account_overridden": true` flag added.


#### Details of a System Notification

Using the ID from the system listing above, get the template object. This document allows you to set some "static" properties (things not derived from the event causing the notification, e.g. call data, system alert, etc).

> GET /v2/notifications/{NOTIFICATION_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/notifications/{NOTIFICATION_ID}
```

**Responses**

```json
{
  "auth_token": "{AUTH_TOKEN}",
  "data": [
    {
      "id": "{NOTIFICATION_ID}",
      "macros": { "..." },
      "templates": {
        "text/html": {
          "length": 600
        }
        ,"text/plain": {
          "length": 408
        }
      }
    }
    "..."
  ],
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Details of an Account Overridden Notification

Performing a GET with an account ID will return the notification object, again with the `account_overridden` flag added.

> GET /v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```

**Responses**

Same as above with the `account_overridden` flag added.

#### Update a Notification

Similar to the PUT, POST will update an existing configuration:

> POST /v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

!!! note
    Omit `/accounts/{ACCOUNT_ID}` to update the system's version. Only a super duper admin can create/modify/delete system notifications!

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{
    "data": {
        "id":"{NOTIFICATION_ID}",
        "to": {
            "type": "users",
            "email_addresses": ["user@account.com"]
        },
        "from": "reseller@resellerdomain.com",
        "subject": "Hello {{user.first_name}}, you received a new voicemail!",
        "enabled": true,
        "template_charset": "utf-8",
        "macros": {
            "user.first_name": {
                "i18n_label": "first_name",
                "friendly_name": "First Name",
                "description": "If the voicemail box has an owner id, this is the first name of that user.  Not always present"
            },
            "user.last_name": {
                "i18n_label": "last_name",
                "friendly_name": "Last Name",
                "description": "If the voicemail box has an owner id, this is the last name of that user.  Not always present"
            }
        }
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```

## Remove a Notification

> DELETE /v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

> **Note:** Omit the `/accounts/{ACCOUNT_ID}` to remove the system default. Only a super duper admin can create/modify/delete system notifications!

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```

## Get Notification Template:

> GET /v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

When you perform a `GET` request to fetch a notification configuration (using `Accept: application/json` header or not setting `Accept` header at all), there is a `Content-Type` HTTP header in the response headers. Use those content types to fetch a specific template by setting your request `Accept` header:

### Get as `text/html`

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Accept: text/html' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```

### Get as `text/plain`

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Accept: text/plain' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```

Note that the only difference is the `Accept` attribute. This will determine which attachment is returned in the payload. If you specify a nonexistent `Accept` MIME type, expect to receive a `406 Not Acceptable` error.

For clients that do not support setting the `Accept` header, a query string parameter can be included (e.g. `/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}?accept=text/html` to get the HTML template).

## Update Notification Template:

> POST /v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

### Update `text/plain`

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Content-Type: text/plain' \
    -d 'some plain text template code' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```

### Update `text/html`

An Example using Curl Upload File:

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Content-Type: text/html' \
    -F content=@file.html \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```

!!! note
    Omit `/accounts/{ACCOUNT_ID}` to update the system's version. Only a super duper admin can create/modify/delete system notifications!

## Preview a new template

It can be helpful to preview the resulting email when modifying templates, but before actually saving the template.

> POST /v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}/preview

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Content-Type: application/json' \
    -d '{"data": {
        "to": {"email_addresses": ["me@2600hz.com"]},
        "from": "kazoo@2600hz.com",
        "subject": "Testing NOTIFICATION",
        "html": "SSUyNTIwJTI1dTI2NjElMjUyMFVuaWNvZGUlMjUyMQ==",
        "plain": "You just received an email! It was sent to {{user.email}}",
        "enabled": true
        }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}/preview
```

* `html` is the base64 encoded HTML template
* `plain` is the plain-text template


## Remove All Account's Notification Customizations

To remove all notification customizations made on the account use a `DELETE` method with action `remove_customizations`.

> DELETE /v2/accounts/{ACCOUNT_ID}/notifications/

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"action": "remove_customizations", "data": {}}'
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications
```


##### Response

```json
{
  "auth_token": "{AUTH_TOKEN}",
  "data": {
    "account_zone_change": "deleted",
    "cnam_request": "deleted",
    "customer_update": "deleted",
    "denied_emergency_bridge": "deleted",
    "deregister": "deleted",
    "fax_inbound_error_to_email": "deleted",
    "fax_inbound_error_to_email_filtered": "deleted",
    "fax_inbound_to_email": "deleted",
    "fax_outbound_error_to_email": "deleted",
    "fax_outbound_smtp_error_to_email": "deleted",
    "fax_outbound_to_email": "deleted",
    "first_occurrence": "deleted",
    "low_balance": "deleted",
    "missed_call": "deleted",
    "new_account": "deleted",
    "new_user": "deleted",
    "password_recovery": "deleted",
    "port_cancel": "deleted",
    "port_comment": "deleted",
    "port_pending": "deleted",
    "port_rejected": "deleted",
    "port_request": "deleted",
    "port_request_admin": "deleted",
    "port_scheduled": "deleted",
    "port_unconfirmed": "deleted",
    "ported": "deleted",
    "service_added": "deleted",
    "skel": "deleted",
    "system_alert": "deleted",
    "topup": "deleted",
    "transaction": "deleted",
    "transaction_failed": "deleted",
    "voicemail_full": "deleted",
    "voicemail_to_email": "deleted",
    "webhook_disabled": "deleted"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Force All Account's Notifications to System Default

To remove all notification customizations made on the account and use the notification from system use a `PUT` method with action `force_system`.

> PUT /v2/accounts/{ACCOUNT_ID}/notifications/

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"action": "force_system", "data": {}}'
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications
```

**Responses**

```json
{
  "auth_token": "{AUTH_TOKEN}",
  "data": {
    "account_zone_change": "replaced",
    "cnam_request": "replaced",
    "customer_update": "replaced",
    "denied_emergency_bridge": "replaced",
    "deregister": "replaced",
    "fax_inbound_error_to_email": "replaced",
    "fax_inbound_error_to_email_filtered": "replaced",
    "fax_inbound_to_email": "replaced",
    "fax_outbound_error_to_email": "replaced",
    "fax_outbound_smtp_error_to_email": "replaced",
    "fax_outbound_to_email": "replaced",
    "first_occurrence": "replaced",
    "low_balance": "replaced",
    "missed_call": "replaced",
    "new_account": "replaced",
    "new_user": "replaced",
    "password_recovery": "replaced",
    "port_cancel": "replaced",
    "port_comment": "replaced",
    "port_pending": "replaced",
    "port_rejected": "replaced",
    "port_request": "replaced",
    "port_request_admin": "replaced",
    "port_scheduled": "replaced",
    "port_unconfirmed": "replaced",
    "ported": "replaced",
    "service_added": "replaced",
    "skel": "replaced",
    "system_alert": "replaced",
    "topup": "replaced",
    "transaction": "replaced",
    "transaction_failed": "replaced",
    "voicemail_full": "replaced",
    "voicemail_to_email": "replaced",
    "webhook_disabled": "replaced"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Get the Notifications SMTP Logs

> GET /v2/accounts/{ACCOUNT_ID}/notifications/smtplog

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Content-Type: text/plain' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/smtplog
```

## Get a notification's SMTP log

> GET /v2/accounts/{ACCOUNT_ID}/notifications/smtplog/{SMTP_LOG_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/smtplog/{SMTP_LOG_ID}
```

```json
{
  "data": {
    "rendered_templates": {
      "text/plain": "Expired registration in account \"teste\".\nNotifications are enabled for loss of registration on the device 995582142@teste.sip.90e9.com\n\nLast Registration:\nDevice ID: 4e411cf70ad352a222e24fbacf467c18\nAccount ID: 85ea6075c6c1e266f8512e2233541bdb\nUser Agent: Grandstream GXP2130 1.0.7.25\nContact: sip:995582142@192.168.26.13:5060;reg-id=1;+sip.instance=&quot;urn:uuid:00000000-0000-1000-8000-000B826C4283&quot;\n\nThis may be due to a network connectivity issue, power outage, or misconfiguration. Please check the device.",
      "text/html": "<h2>Expired registration in account \"teste\"</h2><p>Notifications are enabled for loss of registration on the device 995582142@teste.sip.90e9.com</p><h3>Last Registration</h3><table><tbody><tr><td>Device ID</td><td>4e411cf70ad352a222e24fbacf467c18</td></tr><tr><td>Account ID</td><td>85ea6075c6c1e266f8512e2233541bdb</td></tr><tr><td>User Agent</td><td>Grandstream GXP2130 1.0.7.25</td></tr><tr><td>Contact</td><td>sip:995582142@192.168.26.13:5060;reg-id=1;+sip.instance=&quot;urn:uuid:00000000-0000-1000-8000-000B826C4283&quot;</td></tr></tbody></table><p>This may be due to a network connectivity issue, power outage, or misconfiguration. Please check the device.</p>"
    },
    "subject": "Loss of Registration for 995582142@teste.sip.90e9.com",
    "emails": {
      "from": "no_reply@dev-01.90e9.com",
      "to": [
        "teste@factorlusitano.com"
      ]
    },
    "receipt": "2.0.0 Ok: queued as B60E22044B",
    "account_id": "{ACCOUNT_ID}",
    "account_db": "{ACCOUNT_DB}",
    "template_id": "deregister",
    "template_account_id": "5ba01ad7ad1611d436b1860d8c552897",
    "id": "{SMTP_LOG_ID}"
  },
  "revision": "{REVISION}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Customer update

You can use the special Customer Update notification to send a message to all reseller's children users or to a particular account's users.

### Send Message to All Reseller's Accounts:

> POST /v2/accounts/{ACCOUNT_ID}/notifications/customer_update/message

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Content-Type: text/html' \
    -F content=@file.html \
    http://{SERVER}:8000/v2/accounts/{RESELLER_ACCOUNT_ID}/notifications/customer_update/message
```

### Send Message to a Particular Account:

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Content-Type: text/html' \
    -d '{"data": {"recipient_id": "33ca3929ed585e0e423eb39e4ffe1452"}}' \
    http://{SERVER}:8000/v2/accounts/{PARTICULAR_ACCOUNT_ID}/notifications/customer_update/message
```

### Send Message to Particular Users

You can send a message to all users, administrators only or a particular user within an account. Just add a `user_type` field to your payload:

* **All users:**

```json
{
  "data": {"user_type": "all_users"}
}
```

* **Particular user:**

> **Note:** Applicable to send message to a particular account only.

```json
{
  "data": {"user_type": "{USER_ID}"}
}
```

* **Admin privileged users only**

> **Note:** This is the default behavior, so `user_type` could be omitted.

```json
{
  "data": {"user_type": "admins_only"}
}
```

### Specifying the Message

For specifying the actual message (and the email subject) you want to send, provide HTML and plain text templates by uploading document:

```json
{
  "data": {
    "recipient_id": "33ca3929ed585e0e423eb39e4ffe1452",
    "user_type": "3d9b564d5c95d52d81a2e49ea0c57941",
    "id": "customer_update",
    "account_overridden": true,
    "enabled": true,
    "category": "user",
    "friendly_name": "Customer update",
    "from": "info@localhost.me",
    "subject": "Test Reseller customer update",
    "bcc": {
      "email_addresses": [],
      "type": ""
    },
    "cc": {
      "email_addresses": [],
      "type": ""
    },
    "macros": {
      "user.email": {
        "description": "Email of the user",
        "friendly_name": "Email",
        "i18n_label": "user_email"
      },
      "user.first_name": {
        "description": "First Name",
        "friendly_name": "First Name",
        "i18n_label": "first_name"
      },
      "user.last_name": {
        "description": "Last Name",
        "friendly_name": "Last Name",
        "i18n_label": "last_name"
      },
      "user.timezone": {
        "description": "Timezone of the user",
        "friendly_name": "Timezone",
        "i18n_label": "user_timezone"
      },
      "user.username": {
        "description": "Username",
        "friendly_name": "Username",
        "i18n_label": "username"
      }
    },
    "template_charset": "utf-8",
    "html": "PHA+RGVhciB7e3VzZXIuZmlyc3RfbmFtZX19IHt7dXNlci5sYXN0X25hbWV9fS48L3A+CjxwPkhlcmUgYXJlIHNvbWUgbmV3cyB0aGF0IHdlIGhhdmUgc2VsZWN0ZWQgZm9yIHlvdTwvcD4KPHA+QmVzdCByZWdhcmRzLDwvcD4KPHA+T25OZXQgSW5ub3ZhdGlvbnMgTGltaXRlZC48L3A+",
    "plain": "Dear {{user.first_name}} {{user.last_name}}.\n\nHere are some more news that we have selected for you.\n\nBest regards,\nYour Imagination, Corp",
    "templates": {
      "text/html": {
        "length": 161
      },
      "text/plain": {
        "length": 136
      }
    },
  }
}
```

### Send Message from your Kazoo Application

To send an update to a customer from your Kazoo Application, you can build payload include your application data (`<<"DataBag">>` field) and send it over AMQP using predefined particular template (`<<"Template-ID">>` field) or your own hard coded templates (`<<"HTML">>` and `<<"Text">>` fields):

```erlang
-spec send_account_update(ne_binary()) -> 'ok'.
send_account_update(AccountId) ->
    case kz_amqp_worker:call(build_customer_update_payload(AccountId)
                            ,fun kapi_notifications:publish_customer_update/1
                            ,fun kapi_notifications:customer_update_v/1
                            )
    of
        {'ok', _Resp} ->
            lager:debug("published customer_update notification");
        {'error', _E} ->
            lager:debug("failed to publish_customer update notification: ~p", [_E])
    end.

-spec build_customer_update_payload(ne_binary()) -> kz_proplist().
build_customer_update_payload(AccountId) ->
    props:filter_empty(
      [{<<"Account-ID">>, kz_services_reseller:get_id(AccountId)}
      ,{<<"Recipient-ID">>, AccountId}
      %% DataBag is useful if you have a customized template and wants to pass some your info from your app
      ,{<<"DataBag">>, kz_json:from_list([{<<"field1">>,<<"value1">>},{<<"field2">>,{[{<<"subfield1">>,<<"subvalue1">>},{<<"subfield2">>,<<"subvalue2">>}]}}])}
      %% set below prop if you have a customized template with this ID in your account's DB
      ,{<<"Template-ID">>, <<"customer_update_billing_period">>}
      %% otherwise set your customized message as below:
      ,{<<"HTML">>, base64:encode(<<"Dear {{user.first_name}} {{user.last_name}}. <br /> DataBag test: {{databag.field2.subfield1}} <br /> Kind regards,">>)}
      ,{<<"Text">>, <<"Oh Dear {{user.first_name}} {{user.last_name}}.\n\nDataBag test: {{databag.field2.subfield2}}\n\nBest regards,">>}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ]).
```
