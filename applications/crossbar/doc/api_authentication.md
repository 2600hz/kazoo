# Account API Authentication

Use your account's API token to instruct Crossbar to create an authentication token to be used on subsequent requests requiring authentication.

## Getting your API key from the API:

Use [Account API](accounts.md) to get API key for your account:

!!! note
    Must already authenticated as a user.


```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/api_key
```

```json
{
   "auth_token":"{AUTH_TOKEN}",
   "data":{
      "api_key":"{API_KEY}"
   },
   "revision":"{REQUEST_ID}",
   "request_id":"{REQUEST_ID}",
   "status":"success"
}
```

#### Schema

Provides an auth-token via an Account API key



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`api_key` | The Accounts API key | `string(64)` |   | `true` |  



#### Create

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
