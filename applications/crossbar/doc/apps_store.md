
### Apps Store

Apps Store list apps allowed  by your service plan.

#### Apps Structure

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

```
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

#### Crossbar

Using Crossbar to modify Apps is very simple:

* GET - Gets the app(s).
* PUT - Install an app.
* POST - Updates an app.
* DELETE - Uninstall an app.

`/v2/accounts/{{ACCOUNT_ID}}/apps_store`

##### Fetch App(s):

###### Request

`GET` request on:

`http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/apps_store`

or

`http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/apps_store/{{APP_ID}}`

```
curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/apps_store

curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/apps_store/{{APP_ID}}
```

###### Response

```
{
    "data": [{{APPS}}],
    "status": "success"
}
```

##### PUT - Install App:

Install app on your account.

###### Request

`PUT` request on: `http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/apps_store/{{APP_ID}}`


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
curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/apps_store/{{APP_ID}} -d '{"data": {"allowed_users": "specific","users": []}}'
```

###### Response

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



##### POST - Update an App permission:

Update app permission on your account.

###### Request

`POST` request on: `http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/apps_store/{{APP_ID}}`


```
{
    "data": {
        "allowed_users": "all"
    }
}
```

```
curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/apps_store/{{APP_ID}} -d '{"data": {"allowed_users": "all"}}'
```

###### Response

```
{
    "data": {
        "allowed_users": "all"
    },
    "status": "success"
}
```

##### DELETE - Uninstall an App:

Uninstall app on your account (remove permission for all users).

###### Request

`DELETE` request on: `http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/apps_store/{{APP_ID}}`


```
curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/apps_store/{{APP_ID}}
```

###### Response

```
{
    "data": {},
    "status": "success"
}
```

##### Fetch App icon

###### Request

`GET` request on: `http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/apps_store/{{APP_ID}}/icon`


```
curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://{{SERVER}}:8000/v2/apps_store/accounts/{{ACCOUNT_ID}}/{{APP_ID}}/icon
```

###### Response

Application Icon


##### Fetch App screen shots

###### Request

`GET` request on: `http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/apps_store/{{APP_ID}}/screenshot/{{NUMBER}}`


```
curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://{{SERVER}}:8000/v2/apps_store/accounts/{{ACCOUNT_ID}}/{{APP_ID}}/screenshot/{{NUMBER}}
```

###### Response

Application Screen shot

##### Get Blacklist

Need to be reseller

###### Request

`GET` request on: `http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/apps_store/blacklist`

###### Response

```json
{
    "data": {
        "blacklist": ["{{APP_1}}", "{{APP_2}}"]
    },
    "status": "success"
}
```

##### Update Blacklist

Need to be reseller

###### Request

`POST` request on: `http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/apps_store/blacklist`

```json
{
    "data": {
        "blacklist": ["{{APP_1}}", "{{APP_2}}", "{{APP_3}}"]
    },
    "status": "success"
}
```

###### Response

```json
{
    "data": {
        "blacklist": ["{{APP_1}}", "{{APP_2}}", "{{APP_3}}"]
    },
    "status": "success"
}
```
