/*
Section: Crossbar
Title: Apps Store
Language: en-US
*/

# Apps Store

Apps Store list apps allowed  by your service plan.

## Apps Structure

Cannot be modified, only accesible by GET requests.

Ex:
```
{
    "name": "numbers",
    "i18n": {
        "en-US": {
            "label": "Number Manager",
            "description": "Number Manager app allows you to easily buy, port and manage all numbers in your account.",
            "extended_description": "When you're managing a PBX or running a business there are more numbers in your hands than you can manage. Number Manager allows you to easily purchase and delete numbers, port numbers away from other carriers, and assign them to your own clients.",
            "features": ["Purchase new numbers", "Delete numbers", "Port numbers from other carriers", "Set a Caller ID for each number", "Set a Failover Number", "Associate e911 address to each number"]
        }
    },
    "tags": ["reseller", "developer"],
    "icon": "NumberManager_app.png",
    "api_url": "http://server:8000/v2/",
    "source_url": "http://server/monster-apps/numbers",
    "author": "2600Hz",
    "version": "1.0",
    "license": "-",
    "price": 0,
    "screenshots": ["numbermanager1.png", "numbermanager2.png", "numbermanager3.png"],
    "urls": {
        "documentation": "{documentation_url}",
        "howto": "{howto_video_url}"
    },
    "id": "d5c75363d3d188f08dfcf5f5b80f645f"
}
```

## App Permission

This is located on the account document.

```
{
    "apps": {
        "{{application_id}}": {
            "allowed_users": "specific",
            "users": []
        },
        "{{application_id}}": {
            "allowed_users": "specific",
            "users": [
                "{{user_id_1}}",
                "{{user_id_2}}"
            ]
        },
        "{{application_id}}": {
            "allowed_users": "admin",
            "users": []
        },
        "{{application_id}}": {
            "allowed_users": "all",
            "users": []
        }
    }
}
```

| Allowed Users  | To |
| ------------- | ------------- |
| Specific with **no user**  | No one  |
| Specific with **user(s)**  | Only listed users  |
| All  | Everyone in the account  |
| Admin | Only Admins  |

## Crossbar

Using Crossbar to modify Apps is very simple:

* GET - Gets the app(s).
* PUT - Install an app.
* POST - Updates an app.
* DELETE - Uninstall an app.

`/v2/accounts/{ACCOUNT_ID}/apps_store`

### Fetch App(s):

#### Request

`GET` request on:

`http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store`

or

`http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}`

```
curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store

curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}
```

#### Response

```
{
    "data": [{
        "id": "{APP_ID}",
        "name": "port",
        "i18n": {
            "en-US": {
                "label": "Port Manager",
                "description": "Manage your ports"
            }
        },
        "tags": [],
        "api_url": "http://server:8000/v2/",
        "source_url": "http://server/monster-apps/port"
    }, {
        "id": "{APP_ID}",
        "name": "callflows",
        "i18n": {
            "en-US": {
                "label": "Callflows",
                "description": "The callflow app does stuff",
                "extended_description": "...",
                "features": []
            }
        },
        "tags": [],
        "api_url": "http://server:8000/v2/",
        "source_url": "http://server/monster-apps/callflows"
    }, {
        "id": "{APP_ID}",
        "name": "branding",
        "i18n": {
            "en-US": {
                "label": "Branding",
                "description": "The Branding app allows you to customize the UI.",
                "extended_description": "...",
                "features": []
            }
        },
        "tags": [],
        "api_url": "http://server:8000/v2/",
        "source_url": "http://server/monster-apps/branding"
    }],
    "status": "success"
}
```

### PUT - Install App:

Install app on your account.

#### Request

`PUT` request on: `http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}`


```
{
    "data": {
        "name": "accounts",
        "allowed_users": "specific",
        "users": []
    }
}
```

```
curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID} -d '{"data": {"allowed_users": "specific","users": []}}'
```

#### Response

```
{
    "data": {
        "name": "accounts",
        "allowed_users": "specific",
        "users": []
    },
    "status": "success"
}
```



### POST - Update an App permission:

Update app permission on your account.

#### Request

`POST` request on: `http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}`


```
{
    "data": {
        "allowed_users": "all",
        "users": []
    }
}
```

```
curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID} -d '{"data": {"allowed_users": "all","users": []}}'
```

#### Response

```
{
    "data": {
        "allowed_users": "all",
        "users": []
    },
    "status": "success"
}
```

### DELETE - Uninstall an App:

Uninstall app on your account (remove permission for all users).

#### Request

`DELETE` request on: `http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}`


```
curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}
```

#### Response

```
{
    "data": {},
    "status": "success"
}
```

### Fetch App icon

#### Request

`GET` request on: `http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}/icon`


```
curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/apps_store/accounts/{ACCOUNT_ID}/{APP_ID}/icon
```

#### Response

Application Icon


### Fetch App screen shots

#### Request

`GET` request on: `http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}/screenshot/{NUMBER}`


```
curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/apps_store/accounts/{ACCOUNT_ID}/{APP_ID}/screenshot/{NUMBER}
```

#### Response

Application Screen shot

