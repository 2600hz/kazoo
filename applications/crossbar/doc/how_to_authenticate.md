# Authenticating REST request

Almost every request to Crossbar must be supplying with authentication credentials.

Failing to do so will result in in `401 Unauthorized` HTTP response status with response payload like this:

```json
{
  "data": {
    "message": "invalid credentials"
  },
  "error": "401",
  "message": "invalid_credentials",
  "status": "error",
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{API_NODE}",
  "request_id": "{REQUEST_ID}",
  "auth_token": "{AUTH_TOKEN}"
}
```

Crossbar provide a number of ways of authenticating request. Most common way is authenticated as user and received a token usable on subsequent requests.

## Basic Authentication

This is very simple and quick way of authentication. In this method you have to add `Authorization` HTTP header to request. The authorization method is `Basic` and the value is concatenation of your account ID and `md5` hash of username and password. Refer to [Basic Authentication](basics.md) for more information on how to generate the value for `Authorization` header.

Basic authentication has the disadvantage you have to provide the username and password in unencrypted text in every request.

## Token based Authentication

This is the prefer and more secure way to do authentication with Crossbar API.

In Token based authentication you have to login first to get a token which you can then use in other requests. The token should be set in `X-Auth-Token` HTTP header in subsequent requests.

The are different ways to do login authentication.

### Authenticate as a User

The best way to get authenticated and get the token for UI applications and making manual requests.

In this method, you provide the credentials of your user just for login and crossbar will generate an authentication token in response.

User credentials is MD5 hash of `USERNAME:PASSWORD`. So for example if your the username is `john@example.com` (usernames are always in lower case) and the password is `m32c6NfqYEt` MD5 hash of `john@example.com:m32c6NfqYEt` (note the colon `:` which separates the username and password) is `82a2dc91686ec828a67152d45a5c5ef7`.

For generating MD5 of a text in terminal you can use `md5sum` (in Linux) or `md5` (in macOS) as follow:

```shell
$ echo -n 'john@example.com:m32c6NfqYEt' | md5sum
82a2dc91686ec828a67152d45a5c5ef7  -
```

> **NOTE:** You can also use more secure SHA1 as your hash function. For generating SHA1 hash in terminal use `shasum` command.

If you are using a programming language, refer to its documentation on how to generate MD5 hash.

You also need another field of data to identify the user: account's name or account's realm or phone number assigned to this user.

After the required data is ready, you can use `user_auth` API to get an authentication token:

```shell
$ curl -v -X PUT \
    -H "Content-Type: application/json" \
    -d '{"data":{"credentials":"82a2dc91686ec828a67152d45a5c5ef7", "account_name":"{ACCOUNT_NAME"}, "method":[md5|sha1]}' \
    https://{SERVER}:8000/v2/user_auth
```

And a successful response:

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
    },
    "node": "{API_NODE}",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "{VERSION}",
}
```

Here `{AUTH_TOKEN}`, the authentication token, is a long list of characters which you can use in future requests.

```shell
curl -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    https://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}
```

See [User Authentication](user_authentication.md) to learn more about this API resource.

### Account API Authentication

Uses your account's API token to generate authentication token. If you're building a server applications, this is the best way to authenticate your application.

This is the same as authenticating as user, except you supplying the API key as data.

```shell
curl -X PUT \
    -d '{"data": {"api_key":"{API_KEY}"} }' \
    http://{SERVER}:8000/v2/api_auth
```

You can find the API key in Authentication application in UI, or you can get it using [Accounts API](./accounts.md#fetch-the-accounts-api-key).

## Next Steps

There are more way to authenticate your request, learn more about them in Authentication APIs section.

* For adding more security to your system see Multi-Factor Authentication.
* Use [Security API](security.md) for configuring authentication method.
