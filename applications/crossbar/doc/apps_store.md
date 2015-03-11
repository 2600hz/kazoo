/*
Section: Crossbar
Title: Apps Store
Language: en-US
Version: 3.19
*/

Apps Store allow to list the "master apps", get the icons/screenshots.
Apps Store local is an api that allow to install/uninstall apps on an Account but also to modify which user can access these apps.

## Local Structure

The structure is really simple:

* `all`: Apply to all users on the account (`true`/`false`).
* `users`: A list of user to allow the app to (`all` needs to be set to `false`).

Ex:
```
"all": false,
"users": [
    "452d5706f66377970996b2ec1c0fc04a",
    "b771eb3eee6ea48f4321e3cc31c050ab"
]
```

## Master apps Structure

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

### Install Master applications

Assuming you've installed your Monster applications to `/path/to/monster-ui/apps`, you can run the following SUP command on the server:

    sup crossbar_maintenance init_apps '/path/to/monster-ui/apps' 'http://your.api.server:8000/v2'

This will load the apps (and let you know which apps it couldn't automatically load) into the master account (including icons, if present). For any apps that failed to be loaded automatically, you can follow the manual instructions below.

If you want to install a single Monster application:

    sup crossbar_maintenance init_apps '/path/to/monster-ui/apps/monster-ui-{APP}' 'http://your.api.server:8000/v2'

## Crossbar (Local)

Using Crossbar to modify Apps is very simple:

* GET - Gets the app(s).
* PUT - Install an app.
* POST - Updates an app.
* DELETE - Uninstall an app.

### Apps Store Local URIs

`/v2/accounts/{ACCOUNT_ID}/apps_store`

#### GET - Fetch App(s):

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}

#### PUT - Install App:

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID} -d '{"data": {"all": false,"users": ["452d5706f66377970996b2ec1c0fc04a","b771eb3eee6ea48f4321e3cc31c050ab"]}}'

#### POST - Update App:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID} -d '{"data": {"all": true,"users": []}}'

#### DELETE - Uninstall App:

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/accounts/{ACCOUNT_ID}/apps_store/{APP_ID}


## Crossbar (Master apps)

Using Crossbar to modify Apps is very simple:

* GET - Gets the app(s)/icon/screenshots.

### Apps Store Master apps URIs

`/v2/apps_store`

#### GET - Fetch App(s):

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/apps_store

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/apps_store/{APP_ID}

#### GET - Fetch Icon:

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/apps_store/{APP_ID}/icon

#### GET - Fetch screenshots(s):

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v2/apps_store/{APP_ID}/screenshot/{NUMBER}
