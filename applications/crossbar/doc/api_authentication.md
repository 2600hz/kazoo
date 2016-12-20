### Api_auth

Generating an auth token from your API token

Use your account's API token to instruct Crossbar to create an authentication token to be used on subsequent requests requiring authentication.

#### About

Get your API key for your account:

* It can be obtained by users on an account via the accounts API endpoint `api_key`.
* It can also be accessed by system administrators directly from the database by using `curl` to request the account doc from CouchDB:

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://localhost:15984//accounts/{ACCOUNT_ID}
```

```json
{...
    "pvt_api_key": "dfdb4869092fcaa210077109e42bdbac255dda8b9fe6eeb962b880bea7f9f372",
    ...
}
```

#### Schema

Provides an auth-token via an Account API key

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`api_key` | The Accounts API key | `string(64)` |   | `true`




#### The Authentication Process

> PUT /v2/api_auth

Note:

* `{AUTH_TOKEN}`: this is your authentication token to include in future requests
* `{ACCOUNT_ID}`: your account's ID, useful for constructing URIs
* `{OWNER_ID}`: the user's ID of the owner of the credentials used to generate this token
* `{RESELLER_ID}`: this account's reseller account ID, if any.
* `{REQUEST_ID}`: useful for debugging requests on your installation

```shell
curl -v -X PUT \
    -d '{"data": {"api_key":"{API_KEY}"} }' \
    http://{SERVER}:8000/v2/api_auth
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "account_id": "{ACCOUNT_ID}",
        "apps": [...],
        "is_reseller": true,
        "language": "en-US",
        "owner_id": "{OWNER_ID}",
        "reseller_id": "{RESELLER_ID}",
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
