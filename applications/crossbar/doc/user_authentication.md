### User Authentication

#### About User Authentication

Using your username and password, along with an account identifier, will instruct Crossbar to create an authentication token to be used on subsequent requests requiring authentication.

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`account_name` | The account name of the user | `string(1..128)` |   | `false`
`account_realm` | The account realm of the user | `string(1..64)` |   | `false`
`credentials` | A hash of the uses credentials | `string(1..64)` |   | `true`
`method` | The hash method | `string('md5', 'sha')` | `md5` | `false`
`phone_number` | A phone number assigned to the users account | `string(1..64)` |   | `false`

#### Create a new auth token

Easy as 1, 2, 3:

1. First, select the hashing method and create your credentials hash:
    * MD5: `echo -n "{USERNAME}:{PASSWORD}" | md5sum`
    * SHA1: `echo -n "{USERNAME}:{PASSWORD}" | sha1sum`
2. Select an account identifier (any one of the three will suffice):
    * Account Name (`"account_name"`)
    * SIP Realm (`"realm"`)
    * A Phone Number assigned to the account (`"phone_number"`)
3. Send the HTTP PUT

The response will contain, among other things:

* `{AUTH_TOKEN}`: this is your authentication token to include in future requests
* `{ACCOUNT_ID}`: your account's ID, useful for constructing URIs
* `{OWNER_ID}`: The user's ID of the owner of the credentials used to generate this token
* `{RESELLER_ID}`: The account's reseller account ID, if any
* `{REQUEST_ID}`: Useful for debugging requests on your installation

> PUT /v2/user_auth

```shell
curl -v -X PUT \
    -H "Content-Type: application/json" \
    -d '{"data":{"credentials":"{CREDENTIALS_HASH}", "account_name":"{ACCOUNT_NAME"}, "method":{MD5_OR_SHA1}}' \
    http://{SERVER}:8000/v2/user_auth
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "account_id": "{ACCOUNT_ID}",
        "apps": [],
        "is_reseller": true,
        "language": "en-US",
        "owner_id": "{OWNER_ID}",
        "reseller_id": "{RESELLER_ID}"
    }
    ,"request_id": "{REQUEST_ID}
    ,"revision": "automatic"
    ,"status": "success"
}
```

#### Fetch Token auth information

> GET /v2/accounts/{ACCOUNT_ID}/user_auth/{AUTH_TOKEN}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/user_auth/{AUTH_TOKEN}
{
    "data": {
        "account_id": "{ACCOUNT_ID}",
        "owner_id": "{USER_ID}",
        "method": "cb_user_auth",
        "id": "{AUTH_TOKEN}",
        "reseller_id": "{RESELLER_ID}",
        "is_reseller": false,
        "account_name": "{ACCOUNT_NAME}",
        "language": "en-us",
        "apps": [{
            "id": "8bda62bf7ccf8f8acc219d5d2c515376",
            "name": "accounts",
            "api_url": "http://192.168.0.2:8000/v2/",
            "label": "Accounts Manager"
        }, {
            "id": "99d5f033f0a4176640f9bf1c4e81abed",
            "name": "numbers",
            "api_url": "http://192.168.0.2:8000/v2/",
            "label": "Number Manager"
        }, {
            "id": "0306d5162bad2c7a951b6842483f73cd",
            "name": "voip",
            "api_url": "http://192.168.0.2:8000/v2/",
            "label": "Smart PBX"
        }]
    },
    "auth_token": "{AUTH_TOKEN}",
    "status": "success"
}

```

#### Password Recovery

##### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`account_name` | The account name of the user | `string(1..64)` |   | `false`
`account_realm` | The account realm of the user | `string(1..64)` |   | `false`
`phone_number` | A phone number assigned to the user's account | `string(1..64)` |   | `false`
`username` | The user's API username | `string(1..254)` |   | `true`

Sometimes it is necessary to recover a password.
Similar to user authentication, you can supply the account realm, the account name, or a phone number associated with the account to send a password reset to the user's email.
This email will contain a link that one then click to verify identity & proceed with recovery.

> PUT /v2/user_auth/recovery

```shell
curl -v -X PUT \
    -H "content-type: application/json" \
    -d '{"data":{"username":"API_USERNAME", "account_realm":"ACCOUNT_REALM", "ui_url": "{UI_URL}"}}' \
    http://{SERVER}:8000/v2/user_auth/recovery
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {},
    "request_id": "7ab1daee08211c782a6cdc5425886be4",
    "revision": undefined,
    "status": "success"
}
```


#### Execute link from email account recovery

Send the `{RESET_ID}` collected in the recovery-email.

> POST /v2/user_auth/recovery

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"reset_id": "{RESET_ID}"}}'
    http://{SERVER}:8000/v2/user_auth/recovery
```

##### Responses

###### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {},
    "request_id": "7ab1daee08211c782a6cdc5425886be4",
    "revision": undefined,
    "status": "success"
}
```

###### Unknown `{RESET_ID}`

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "user": {
            "not_found": {
                "cause": "{RESET_ID}",
                "message": "The provided reset_id did not resolve to any user"
            }
        }
    },
    "error": "500",
    "message": "invalid request",
    "request_id": "dbafc07860f521430d176943742d5f7e",
    "status": "error"
}
```
