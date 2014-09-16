/*
Section: Crossbar
Title: Notifications
Language: en-US
*/

Allow to manage templates for notifications.

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

## Crossbar

Using Crossbar to modify notifications is very simple:

* GET - Gets the current notification(s).
* PUT - Add a notification.
* POST - Updates a notification.
* DELETE - Removes a notification.
* GET/(HTM/TXT) - Get HTML or TXT template.
* POST/(HTM/TXT) - Update HTML or TXT template.

To modify an account notification. The requester must be a reseller of that account or the master account.

### Account Temporal Rules Sets URI

`/v1/accounts/{ACCOUNT_ID}/notifications`
`/v1/notifications`


#### GET - Fetch notification:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/notification

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/notification/{id}

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
}' http://server:8000/v1/accounts/{ACCOUNT_ID}/notification
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
}' http://server:8000/v1/accounts/{ACCOUNT_ID}/notification
```

#### DELETE - Remove notification:

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/notification/{ID}


#### GET - Get notification template:

    curl -X GET -H "X-Auth-Token:{AUTH_TOKEN}" -H "Content-Type:text/html" http://server:8000/v1/accounts/{ACCOUNT_ID}/notifications/{ID}/html

    curl -X GET -H "X-Auth-Token:{AUTH_TOKEN}" -H "Content-Type:text/html" http://server:8000/v1/accounts/{ACCOUNT_ID}/notifications/{ID}/txt


#### POST - Update notification template:

```
curl -X POST -H "X-Auth-Token:{AUTH_TOKEN}" -H "Content-Type:text/html" -d '
<div>
  <p>Some Html</p>
</div>' http://server:8000/v1/accounts/{ACCOUNT_ID}/notifications/{ID}/html
```

```
curl -X POST -H "X-Auth-Token:{AUTH_TOKEN}" -H "Content-Type:text/html" -d '
some plain text' http://server:8000/v1/accounts/{ACCOUNT_ID}/notifications/{ID}/text
```





