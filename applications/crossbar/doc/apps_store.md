
### Apps Store

Apps Store list apps allowed  by your service plan.

#### Apps Structure

Cannot be modified, only accessible by GET requests.

Ex:

```json
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
    "api_url": "http://{{SERVER}}:8000/v2/",
    "source_url": "http://{{SERVER}}/monster-apps/numbers",
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

##### Install Master applications

Assuming you've installed your Monster applications to `/path/to/monster-ui/apps`, you can run the following SUP command on the {{SERVER}}:

    sup crossbar_maintenance init_apps '/path/to/monster-ui/apps' 'http://your.api.{{SERVER}}:8000/v2'

This will load the apps (and let you know which apps it couldn't automatically load) into the master account (including icons, if present). For any apps that failed to be loaded automatically, you can follow the manual instructions below.

If you want to install a single Monster application:

    sup crossbar_maintenance init_app '/path/to/monster-ui/apps/{{APP}}' 'http://{{SERVER}}:8000/v2'


#### App Permission

This is located on the account document.

```json
{
    "apps": {
        "{{application_id}}": {
            "allowed_users": "specific",
            "users": []
        },
        "{{application_id}}": {
            "allowed_users": "specific",
            "users": [{
                "id": {{user_id}}
            }]
        },
        "{{application_id}}": {
            "allowed_users": "admins"
        },
        "{{application_id}}": {
            "allowed_users": "all"
        }
    }
}
```

| Allowed Users  | To | key |
| ------------- | ------------- | ------------- |
| Specific with **no user**  | No one  | specific
| Specific with **user(s)**  | Only listed users  | specific
| All  | Everyone in the account  | all
| Admins | Only Admins  | admins

#### Fetch App(s):

> GET /v2/accounts/{ACCOUNT_ID}/apps_store

> GET /v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}
```

```json
{
    "data": [
        {APP}
    ],
    "status": "success"
}
```

#### Install App:

> PUT /v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}

Install app on your account.

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"allowed_users": "specific", "users": []}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}
```

```json
{
    "data": {
        "name": "{APP_ID}",
        "allowed_users": "specific",
        "users": []
    }
}
```


#### Update an App permission:

> POST /v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}

Update app permission on your account.

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"allowed_users": "all"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}
```

```json
{
    "data": {
        "allowed_users": "all"
    },
    "status": "success"
}
```

#### Uninstall an App:

> DELETE /v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}

Uninstall app on your account (remove permission for all users).

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}
```

```json
{
    "data": {},
    "status": "success"
}
```

#### Fetch App icon

> GET /v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}/icon

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}/icon
```

Streams application icon back.


#### Fetch App screen shots

> GET /v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}/screenshot/{APP_SCREENSHOT_INDEX}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}/screenshot/{APP_SCREENSHOT_INDEX}
```

Streams application screenshot number `{APP_SCREENSHOT_INDEX}` back.

#### Get Blacklist

> GET /v2/accounts/{ACCOUNT_ID}/apps_store/blacklist

Need to be reseller.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/blacklist
```

```json
{
    "data": {
        "blacklist": [
            "{APP_1}",
            "{APP_2}"
        ]
    },
    "status": "success"
}
```

#### Update Blacklist

> POST /v2/accounts/{ACCOUNT_ID}/apps_store/blacklist

Need to be reseller.

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"blacklist": [{APP_3}]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/apps_store/blacklist
```

```json
{
    "data": {
        "blacklist": [
            "{APP_1}",
            "{APP_2}",
            "{APP_3}"
        ]
    },
    "status": "success"
}
```
