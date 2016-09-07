### Notifications

Allow managing templates for notification emails.

#### About Notifications

Example structure:

```json
{
    "enabled": true,
    "from": "peter@email.com",
    "macros": {
        "user.first_name": {
            "description": "If the voicemail box has an owner id, this is the first name of that user.  Not always present",
            "friendly_name": "First Name",
            "i18n_label": "first_name"
        },
        "user.last_name": {
            "description": "If the voicemail box has an owner id, this is the first name of that user.  Not always present",
            "friendly_name": "Last Name",
            "i18n_label": "last_name"
        }
    },
    "subject": "Hello {{user.first_name}}, you received a new voicemail!",
    "template_charset": "utf-8",
    "to": {
        "email_addresses": [
            "peter@email.com"
        ],
        "type": "admins"
    }
}
```

In addition to the JSON data, templates in various formats can be uploaded (such as from a WYSIWYG tool). Currently supported are plaintext and HTML documents.

The `macros` object is a per-template, system-defined set of macros you can use in your templates. You cannot configure this via the API.

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`bcc` | Bcc email field | `object` |   | `false`
`bcc.email_addresses` | BCC Email Addresses | `array()` |   | `false`
`bcc.type` |   | `string('original', 'specified', 'admins')` |   | `false`
`category` | Category of the template, for grouping purposes | `string(1..)` |   | `false`
`cc` | CC email field | `object` |   | `false`
`cc.email_addresses` | CC Email Addresses | `array(string)` |   | `false`
`cc.email_addresses.[]` |   | `string` |   | `false`
`cc.type` |   | `string('original', 'specified', 'admins')` |   | `false`
`enabled` | Enable notification | `boolean` | `true` | `false`
`friendly_name` | Friendly name of the template | `string(1..)` |   | `false`
`from` | From: email address | `string` |   | `true`
`macros` |   | `object` | `{}` | `false`
`reply_to` | Reply-To: email address | `string` |   | `false`
`subject` | Email subject | `string(1..200)` |   | `true`
`template_charset` |   | `string(1..)` | `utf-8` | `false`
`to` | To email field | `object` |   | `true`
`to.email_addresses` |   | `array(string)` |   | `false`
`to.email_addresses.[]` |   | `string` |   | `false`
`to.type` |   | `string('original', 'specified', 'admins')` |   | `false`


#### Crossbar

Using Crossbar to modify notifications is very simple:

* GET - Gets the current notification(s).
* PUT - Add a notification.
* POST - Updates a notification, or adds/updates a template.
* DELETE - Removes a notification.

To modify an account notification, the requester must be a reseller of that account or the master account.

##### Account Temporal Rules Sets URI

* `/v2/accounts/{ACCOUNT_ID}/notifications`: modify an account's template(s)
* `/v2/notifications`: Modify the system default templates

###### GET - Fetch available notification templates from the system

This is the first request to make to see what templates exist on the system to override

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/notifications
    {
        "auth_token": "{AUTH_TOKEN},
        "data": [
            {"id": "voicemail_to_email"
             ,"macros": {
                 "call_id": {
                     "description": "Call ID of the caller"
                     ,"friendly_name": "Call ID"
                     ,"i18n_label": "call_id"
                     }
                 ,"caller_id.name": {
                     "description": "Caller ID Name"
                     ,"friendly_name": "Caller ID Name"
                     ,"i18n_label": "caller_id_name"
                 }
                 ,...
             }
            }
            ,{...}
        ]
        "request_id": "{REQUEST_ID}",
        "revision": "undefined",
        "status": "success"
    }

To see what notification templates an account overrides, include the account ID in the URI:

> GET /v2/accounts/{ACCOUNT_ID}/notifications

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications
```

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
                },
                ...
            },
            "account_overridden": true
        }
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "undefined",
    "status": "success"
}
```

The key `account_overridden` will exist on any templates that are account-specific.

###### GET - Fetch a notification's configuration

Using the ID from the system listing above, get the template JSON. This document allows you to set some "static" properties (things not derived from the event causing the notification, e.g. call data, system alert, etc).

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/notifications/{NOTIFICATION_ID}
    {
        "auth_token": "{AUTH_TOKEN}",
        "data": {
            "id": "{NOTIFICATION_ID}"
            ,"macros":{...}
            ,"templates": {
                "text/html": {
                    "length": 600
                }
                ,"text/plain": {
                    "length": 408
                }
            }
        },
        "request_id": "{REQUEST_ID}",
        "revision": "1-ad99c4dc5353792aed7be6e77b2d9d9a",
        "status": "success"
    }

Performing a GET with an account ID will return the notification object, again with the `account_overridden` flag added if it is account-specific; lack of the key indicates it is the system default notification.

###### PUT - Create a notification template

Now that you've fetched the system default template, modify and PUT it back to the account.

> PUT /v2/accounts/{ACCOUNT_ID}/notifications

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{
    "data": {
        "id":"{NOTIFICATION_ID}",
        "to": {
            "type": "users",
            "email_addresses": ["user@account.com"]
        },
        "from": "reseller@resellerdomain.com",
        "subject": "Hello {{user.first_name}}, you recieved a new voicemail!",
        "enabled": true,
        "template_charset": "utf-8"
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "enabled": true,
        "from": "reseller@resellerdomain.com",
        "id": "{NOTIFICATION_ID}",
        "macros": {
            "user.first_name": {
                "description": "If the voicemail box has an owner id, this is the first name of that user. Not always present",
                "friendly_name": "First Name",
                "i18n_label": "first_name"
            },
            "user.last_name": {
                "description": "If the voicemail box has an owner id, this is the last name of that user. Not always present",
                "friendly_name": "Last Name",
                "i18n_label": "last_name"
            }
        },
        "subject": "Hello {{user.first_name}}, you recieved a new voicemail!",
        "template_charset": "utf-8",
        "to": {
            "email_addresses": [
                "user@account.com"
            ],
            "type": "users"
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "1-2ad8b8ca45e49df830a0ffbbc6d964d0",
    "status": "success"
}
```

This request will fail if `id` does not already exist in the system defaults. To create a new system notification template, a superduper admin can use the above PUT, but to `/v2/notifications` instead of a specific account.

###### GET - Fetch a specific notification

Now that you've created an account-specific notification, you can fetch it to feed into a WYSIWYG editor or for other purposes:

> GET /v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "id": "{NOTIFICATION_ID}",
        ...
    },
    "request_id": "{REQUEST_ID}",
    "revision": "1-ad99c4dc5353792aed7be6e77b2d9d9a",
    "status": "success"
}
```

###### POST - Update a notification's config

Similar to the PUT, POST will update an existing config:

> POST /v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

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
        "subject": "Hello {{user.first_name}}, you recieved a new voicemail!",
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

Omit `/accounts/{ACCOUNT_ID}` to update the system's version.

###### DELETE - remove a notification template

> DELETE /v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```

Omit the `/accounts/{ACCOUNT_ID}` to remove the system default.

##### Template Formats

Creating the configuration documents is all well and good, but it is necessary to be able to attach the templates in their various forms as well. Currently supported formats are `text/html` and `text/plain`.

###### GET - Get notification template:

When you GET a notification config (`Accept` of `application/json`), get a `templates` list of `Content-Type` atttributes. Use those to fetch a specific template by setting the `Accept` header:

> GET /v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Accept: text/html' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Accept: text/plain' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```

Note that the only difference is the `Accept` attribute. This will determine which attachment is returned in the payload. If you specify a non-existent Accept MIME type, expect to receive a `406 Not Acceptable` error.

For clients that do not support setting the `Accept` header, a querystring parameter can be included (eg `http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}?accept=text/html` to get the HTML template.

###### POST - Update notification template:

> POST /v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Content-Type: text/plain' \
    -d 'some plain text template code' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Content-Type: text/html' \
    -d '<div>
            <p>Some Html and {{macro.key}} replaced on render</p>
       </div>' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
```


###### _POST_ - Preview a new template

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
        "plain": "You just recieved an email! It was sent to {{user.email}}",
        "enabled": true
        }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}/preview
```

* `html` is the base64 encoded HTML template
* `plain` is the plain-text template

### Operations considerations

In versions Kazoo prior to 3.19, notification templates were managed and processed by the `notify` app.

All accounts will continue to be processed by the `notify` app until the Crossbar notification APIs are accessed for the first time (for instance, when using the Branding App in Monster). Once a client has accessed the APIs, a flag is set on the account telling the `notify` app to ignore processing and instructs the `teletype` app to process it instead. This allows admins to run both `notify` and `teletyple` concurrently without sending multiple copies of each notification.

#### Logs

* GET - Gets the notification(s) SMTP log.

> GET /v2/accounts/{ACCOUNT_ID}/notifications/smtplog

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Content-Type: text/plain' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/smtplog
```

## Customer update

Send a message to all reseller's children or to a particular account.

> POST /v2/accounts/{ACCOUNT_ID}/notifications/customer_update/message

Send message to all reseller's accounts:

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Content-Type: text/plain' \
    -d '{} \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/customer_update/message
```

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Content-Type: text/plain' \
    -d '{} \
    http://{SERVER}:8000/v2/accounts/{SENDER(RESELLER)_ACCOUNT_ID}/notifications/customer_update/message
```

Send message to a particular acount:


```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Content-Type: text/plain' \
    -d '{"data": {"recipient_id": "33ca3929ed585e0e423eb39e4ffe1452"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/notifications/customer_update/message
```

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H 'Content-Type: text/plain' \
    -d '{"data": {"recipient_id": "33ca3929ed585e0e423eb39e4ffe1452"}}' \
    http://{SERVER}:8000/v2/accounts/{SENDER(RESELLER)_ACCOUNT_ID}/notifications/customer_update/message
```

You can send a message to all users, admins only or a particular user within an account. Just add a `user_type` field to your payload:

All users:

```json
... -d '{"data":{"user_type": "all_users"}}'
````

Particular user:
```
... -d '{"data":{"user_type": "{ACCOUNT_ID}"}}'
````

Admin privileged users only. Default. Could be omitted:

```json
... -d '{"data":{"user_type": "admins_only"}}'
````

You can send a message with changed subject, html and plain text templates by providing full notification document payload:

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
        "from": "info@onnet.su",
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
        "plain": "Dear {{user.first_name}} {{user.last_name}}.\n\nHere are some more news that we have selected for you.\n\nBest regards,\nOnNet Innovations Limited.",
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
