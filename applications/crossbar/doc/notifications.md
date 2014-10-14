/*
Section: Crossbar
Title: Notifications
Language: en-US
*/


Allow managing templates for notification emails.

## Structure

The structure is really simple:

Ex:
```
 "to": {
     "type": "admins",
     "email_addresses": [
         "peter@email.com"
     ]
 },
 "from": "peter@email.com",
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
         "description": "If the voicemail box has an owner id, this is the first name of that user.  Not always present"
     }
 }
```

In addition to the JSON data, templates in various formats can be uploaded (such as from a WYSIWYG tool). Currently supported are plaintext and HTML documents.

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

#### GET - Fetch notification:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

#### PUT - Add notification:

```
curl -X PUT -H "X-Auth-Token:{AUTH_TOKEN}" -H "Content-Type:application/json" -d '{
    "data": {
        "to": {
            "type": "admins",
            "email_addresses": ["peter@email.com"]
        },
        "from": "peter@email.com",
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
                "description": "If the voicemail box has an owner id, this is the first name of that user.  Not always present"
            }
        }
    }
}' http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications
```

#### POST - Update notification:

```
curl -X POST -H "X-Auth-Token:{AUTH_TOKEN}" -H "Content-Type:application/json" -d '
{
    "data": {
        "to": {
            "type": "admins",
            "email_addresses": ["peter@email.com"]
        },
        "from": "peter@email.com",
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
                "description": "If the voicemail box has an owner id, this is the first name of that user.  Not always present"
            }
        }
    }
}' http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications
```

#### DELETE - Remove notification:

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}

#### GET - Get notification template:

    curl -X GET -H "X-Auth-Token:{AUTH_TOKEN}" -H "Content-Type:text/html" http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}/html

    curl -X GET -H "X-Auth-Token:{AUTH_TOKEN}" -H "Content-Type:text/plain" http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}/text

#### POST - Update notification template:

```
curl -X POST -H "X-Auth-Token:{AUTH_TOKEN}" -H "Content-Type:text/html" -d '
<div>
  <p>Some Html</p>
</div>' http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}/html
```

```
curl -X POST -H "X-Auth-Token:{AUTH_TOKEN}" -H "Content-Type:text/plain" -d '
some plain text' http://server:8000/v2/accounts/{ACCOUNT_ID}/notifications/{NOTIFICATION_ID}/text
```
