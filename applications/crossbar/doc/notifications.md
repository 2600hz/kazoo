/*
Section: Crossbar
Title: Notifications
Language: en-US
Version: 3.19
*/


Allow managing templates for notification emails.

## Structure

The structure is really simple:

Example:

    {"to":{
         "type": "admins"
         ,"email_addresses": [
             "peter@email.com"
         ]
     }
     ,"from": "peter@email.com"
     ,"subject": "Hello {{user.first_name}}, you recieved a new voicemail!"
     ,"enabled": true
     ,"template_charset": "utf-8"
     ,"macros": {
         "user.first_name": {
             "i18n_label": "first_name"
              ,"friendly_name": "First Name"
              ,"description": "If the voicemail box has an owner id, this is the first name of that user.  Not always present"
          }
          ,"user.last_name": {
              "i18n_label": "last_name"
              ,"friendly_name": "Last Name"
              ,"description": "If the voicemail box has an owner id, this is the first name of that user.  Not always present"
          }
      }
    }

In addition to the JSON data, templates in various formats can be uploaded (such as from a WYSIWYG tool). Currently supported are plaintext and HTML documents.

The `marcos` object is a per-template, system-defined set of macros you can use in your templates. You cannot configure this via the API.

## Crossbar

Using Crossbar to modify notifications is very simple:

* GET - Gets the current notification(s).
* PUT - Add a notification.
* POST - Updates a notification, or adds/updates a template.
* DELETE - Removes a notification.

To modify an account notification, the requester must be a reseller of that account or the master account.

### Account Temporal Rules Sets URI

* `/v2/accounts/{ACCOUNT_ID}/notifications`: modify an account's template(s)
* `/v2/notifications`: Modify the system default templates

#### GET - Fetch available notification templates from the system

This is the first request to make to see what templates exist on the system to override

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/notifications
    {
        "auth_token": "{AUTH_TOKEN},
        "data": [
            "{NOTIFICATION_ID}",
            ...
        ],
        "request_id": "{REQUEST_ID}",
        "revision": "undefined",
        "status": "success"
    }

To see what notification templates an account over-rides, include the account ID in the URI:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications
    {
        "auth_token": "{AUTH_TOKEN},
        "data": [
        ],
        "request_id": "{REQUEST_ID}",
        "revision": "undefined",
        "status": "success"
    }

In this case, the account overrides none of the default system templates.

#### GET - Fetch a notification's configuration

Using the ID from the system listing above, get the template JSON. This document allows you to set some "static" properties (things not derived from the event causing the notification, e.g. call data, system alert, etc).

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/notifications/{NOTIFICATION_ID}
    {
        "auth_token": "{AUTH_TOKEN}",
        "data": {
            "id": "{NOTIFICATION_ID}",
            "templates",["text/html"]
            ...
        },
        "request_id": "{REQUEST_ID}",
        "revision": "1-ad99c4dc5353792aed7be6e77b2d9d9a",
        "status": "success"
    }

#### PUT - Create a notification template

Now that you've fetched the system default template, modify and PUT it back to the account.

    curl -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type:application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications -d '{
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
    }}'
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

This request will fail if `id` does not already exist in the system defaults. To create a new system notification template, a superduper admin can use the above PUT, but to `/v2/notifications` instead of a specific account.

#### GET - Fetch a specific notification

Now that you've created an account-specific notification, you can fetch it to feed into a WYSIWYG editor or for other purposes:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}
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

#### POST - Update a notification's config

Similar to the PUT, POST will update an existing config:

    curl -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type:application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID} -d '{
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
    }}'

Omit `/accounts/{ACCOUNT_ID}` to update the system's version.

#### Delete - remove a notification template

    curl -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type:application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

Omit the `/accounts/{ACCOUNT_ID}` to remove the system default.

### Template Formats

Creating the configuration documents is all well and good, but it is necessary to be able to attach the templates in their various forms as well. Currently supported formats are `text/html` and `text/plain`.

#### GET - Get notification template:

When you GET a notification config (`Accept` of `application/json`), get a `templates` list of `Content-Type` atttributes. Use those to fetch a specific template by setting the `Accept` header:

    curl -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Accept: text/html" http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

    curl -X GET -H "X-Auth-Token: {AUTH_TOKEN}" -H "Accept: text/plain" http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

Note that the only difference is the `Accept` attribute. This will determine which attachment is returned in the payload. If you specify a non-existent Accept MIME type, expect to receive a `406 Not Acceptable` error.

#### POST - Update notification template:

```
curl -X POST -H "X-Auth-Token:{AUTH_TOKEN}" -H "Content-Type:text/html" http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID} -d '
<div>
  <p>Some Html</p>
</div>'
```

```
curl -X POST -H "X-Auth-Token:{AUTH_TOKEN}" -H "Content-Type:text/plain"  http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}/text -d 'some plain text template code'
```

#### _POST_ - Preview a new template

It can be helpful to preview the resulting email when modifying templates, but before actually saving the template.

    curl -H "Content-Type:application/json" -H "X-Auth-Token: {AUTH_TOKEN}" 'http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}/preview' -d '{"data":{"to":{"email_addresses":["me@2600hz.com"]},"from":"kazoo@2600hz.com","subject":"Testing NOTIFICATION","html":"SSUyNTIwJTI1dTI2NjElMjUyMFVuaWNvZGUlMjUyMQ==","plain":"You just recieved an email! It was sent to {{user.email}}","enabled":true}}'

* `html` is the base64 encoded HTML template
* `plain` is the plain-text template
