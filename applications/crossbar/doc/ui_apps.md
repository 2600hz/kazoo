/*
Section: Crossbar
Title: UI Apps
Language: en-US
*/

UI Apps is an api that allow to install/uninstall apps on an Account but also to modify which user can access these apps.
## Structure

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

## Crossbar

Using Crossbar to modify UI Apps is very simple:

* GET - Gets the app(s).
* PUT - Install an app.
* POST - Updates an app.
* DELETE - Uninstall an app.

### UI Apps URI

`/v1/accounts/{ACCOUNT_ID}/ui_apps`

#### GET - Fetch App(s):

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/ui_apps

    curl -v -X GET -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/ui_apps/{APP_ID}

#### PUT - Install App:

    curl -v -X PUT -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v1/accounts/{ACCOUNT_ID}/ui_apps/{APP_ID} -d '{"data": {"all": false,"users": ["452d5706f66377970996b2ec1c0fc04a","b771eb3eee6ea48f4321e3cc31c050ab"]}}'

#### POST - Update App:

    curl -v -X POST -H "X-Auth-Token: {AUTH_TOKEN}" -H "Content-Type: application/json" http://server:8000/v1/accounts/{ACCOUNT_ID}/ui_apps/{APP_ID} -d '{"data": {"all": true,"users": []}}'

#### DELETE - Uninstall App:

    curl -v -X DELETE -H "X-Auth-Token: {AUTH_TOKEN}" http://server:8000/v1/accounts/{ACCOUNT_ID}/ui_apps/{APP_ID}
