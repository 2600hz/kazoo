### Authentication

Crossbar module for operations on JWT Token, SSO/OAuth tokens.

#### About Authentication

#### Schema



#### Patch

> *Caution:* Reseting system identity secret will invalidate *all* issued token! In other words all logined users will be logout from the system and can't make any further request until login again. Use this API if you feel the system secret is compromised only.

> *Note:* Only super duper admin can reset system secert!

> PATCH /v2/auth/identity_secret

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/identity_secret
```

##### Response

Returns an empty success response.

```json
{
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Resetting a User Identity Secret

System Identity secret and the subject identity is being used to sign the identity in the JWT token that Kazoo is issuing.

If you feel that an account or a user secret is compropised, use this API to reset it.

* To reset an account's identity secret, simply make request to this API with the account's id in the paths
* To reset a user's identity secret, make a request as above with setting payload as: `{"data": { "owner_id": "{USER_ID}" } }`

> *Caution:* Reseting identity secret will invalidate user's issued token! In other words if the is already logined, the user will be logout from the system and can't make any further request until login again.

> *Note:* Only and account admin can a user's secert!

> PATCH /v2/accounts/{ACCOUNT_ID}/auth/identity_secret

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/290ac723eb6e73dd4a0adcd77785e04e/auth/identity_secret
```

##### Response

Returns an empty success response.

```json
{
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Get a List of Registered SSO App

This list all registered Single Sign On applications for the account's reseller. Account ID is determined in order by first from the query string `account_id`, or from request path (`/accounts/{ACCOUNT_ID}`) or from authenticated user's account id.

> GET /v2/auth/apps

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/apps
```

##### Response

```json
{
  "page_size": 1,
  "data": [
    {
      "id": "iamatestclientid.apps.googleusercontent.com",
      "provider": "google"
    }
  ],
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### List SSO Provider

Get a list of all Single Sign On provider.

> GET /v2/auth/providers

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers
```

##### Response

```json
{
  "page_size": 3,
  "data": [
    {
      "id": "salesforce"
    },
    {
      "id": "office365"
    },
    {
      "id": "google"
    }
  ],
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### List User's Linked SSO Applications

Get a list of the linked Single Sign On applications for the authenticated user.

> GET /v2/auth/links

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links
```

##### Response

```json
{
  "page_size": 1,
  "data": [
    {
      "id": "{LINKED_USER_ID}",
      "app": "{LINKED_OAUTH_APP_ID}",
      "provider": "{LINKED_OAUTH_PROVIDER}",
      "scopes": [
        "https://www.googleapis.com/auth/plus.me",
        "https://www.googleapis.com/auth/userinfo.profile",
        "https://www.googleapis.com/auth/userinfo.email"
      ],
      "email": "{USER_EMAIL}"
    }
  ]
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Create a Token from SSO Response

After a user authenticate with Single Sign On provider, use this API to send the provider response to Crossbar to login and create a token.

> PUT /v2/auth/callback

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{ "data": { "redirect_uri": "{MONSTER_UI_IP}", "client_id": "{OAUTH_CLIENT_ID}", "provider": "{OAUTH_PROVIDER_NAME}", "code": "{OAUTH_RESP_CODE}" } }'
    http://{SERVER}:8000/v2/auth/callback
```

##### Response when no User is Linked yet

If this is the first that the user is authenticating using this SSO provider, Kazoo returns and empty response indeicating that user should first login using it's own Kazoo credentials first to link the SSO application with its user. After login the user is linked with the app and it no need to maunual login again.

```json
{
  "page_size": 1,
  "data": {},
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

##### Response when the User is Linked

```json
{
  "page_size": 1,
  "data": {
    "owner_id": "{OWNER_ID}",
    "account_id": "{ACCOUNT_ID}",
    "reseller_id": "{RESELLER_ID}",
    "account_name": "{ACCOUNT_NAME}",
    "language": "{LANG}",
    "apps": [{APPS}]
  },
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Validate and Authorize a Foreign SSO Token/App

> PUT /v2/auth/authorize

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/authorize
```

#### Get Token Information (Query String version)

Returns the information encoded in the sets inside the specified authentication token (from query string `token` parameter).

> GET /v2/auth/tokeninfo

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/tokeninfo?token={AN_AUTH_TOKEN}
```

##### Response

```json
{
  "page_size": 1,
  "data": {
    "owner_id": "{OWNER_ID}",
    "account_id": "{ACCOUNT_ID}",
    "reseller_id": "{RESELLER_ID}",
    "account_name": "{ACCOUNT_NAME}",
    "language": "{LANG}",
    "apps": [{APPS}]
  },
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Get Token Information (Request Body version)

Returns the information encoded in the sets inside the specified authentication token (from request body `token` parameter).

> POST /v2/auth/tokeninfo

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{ "data": { "token": "{AN_AUTH_TOKEN}" } }'
    http://{SERVER}:8000/v2/auth/tokeninfo
```

##### Response

```json
{
  "page_size": 1,
  "data": {
    "owner_id": "{OWNER_ID}",
    "account_id": "{ACCOUNT_ID}",
    "reseller_id": "{RESELLER_ID}",
    "account_name": "{ACCOUNT_NAME}",
    "language": "{LANG}",
    "apps": [{APPS}]
  },
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}

#### Get a SSO Application

Get a Single Sign On application by it's ID.

> GET /v2/auth/apps/{APP_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/apps/{APP_ID}
```

##### Response

```json
{
  "page_size": 1,
  "data": {
    "id": "{APP_ID}"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}

#### Change a SSO Application

> POST /v2/auth/apps/{APP_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/apps/{APP_ID}
```

#### Remove a SSO Application

> DELETE /v2/auth/apps/{APP_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/apps/{APP_ID}
```

#### Get System Public Key

Get the System public key used for signing the JWT tokens issued by system.

*Note:* To get the public key in form of a PEM file set `Accept` header as `application/x-pem-file`.

> GET /v2/auth/keys/public

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/keys/public
```

##### Response

```json
{
  "page_size": 1,
  "data": {
     "public_key_pem": "-----BEGIN RSA PUBLIC KEY-----\n{SYSTEM_PUBLIC_KEY}\n-----END RSA PUBLIC KEY-----\n\n"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}

#### Reset System Private Key

Reset the private key of the system used to signing and verifing issued JWT tokens. If you feel that you the private key is compromised, use this API to generate a new private and public key.

> *Caution:* Reseting system private will invalidate *all* issued token! In other words all logined users will be logout from the system and can't make any further request until login again. Use this API if you feel the system private key is compromised only.

> *Note:* Only super duper admin can reset system private key!

> PATCH /v2/auth/keys/private

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/keys/private
```

#### Fetch a SSO Provider Informations

> GET /v2/auth/providers/{PROVIDER_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers/google
```

##### Response

```json
{
  "data": {
    "access_code_url": "https://accounts.google.com/o/oauth2/token",
    "auth_module": "oauth",
    "auth_url": "https://accounts.google.com/o/oauth2/token",
    "discovery": "https://accounts.google.com/.well-known/openid-configuration",
    "issuer_domains": [
      "accounts.google.com"
    ],
    // rest of the provider information...
    "id": "google"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Make changes to SSO Provider

> *Note:* Only super duper admin can make changes to SSO provider!

> POST /v2/auth/providers/{PROVIDER_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers/{PROVIDER_ID}
```

#### Remove a SSO Provider

> *Note:* Only super duper admin can delete a SSO provider!

> DELETE /v2/auth/providers/{PROVIDER_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers/{PROVIDER_ID}
```

#### Get an User's Linked SSO Applications Information

> GET /v2/auth/links/{LINK_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links/d6faaff6b054393f28356ab7b38ad1bf-116254222860442180295
```

##### Response

```json
{
  "data": {
    "email": "{USER_EMAIL}",
    "verified_email": true,
    "scope": "{PROVIDER_SCOPE}",
    "scopes": [{SCOPES}],
    "profile": { //user's profile },
    "display_name": "{Test Person}",
    "id": "d6faaff6b054393f28356ab7b38ad1bf-116254222860442180295"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Link an User to a SSO Application

When the user is signing on with A Single Sign On provider for the first time, it should login with its own Kazoo credentials only for this time, and then make a request to this API to link its Kazoo's user to the SSO. After this the user can sign in with SSO regularly without the need to use Kazoo credentials again.

> PUT /v2/auth/links/{LINK_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links/d6faaff6b054393f28356ab7b38ad1bf-116254222860442180295
```

##### Response

```json
{
  "data": {
    "email": "{USER_EMAIL}",
    "verified_email": true,
    "scope": "{PROVIDER_SCOPE}",
    "scopes": [{SCOPES}],
    "profile": { //user's profile },
    "display_name": "{Test Person}",
    "id": "d6faaff6b054393f28356ab7b38ad1bf-116254222860442180295"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Unlink a user from SSO Application

> DELETE /v2/auth/links/{LINK_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links/d6faaff6b054393f28356ab7b38ad1bf-116254222860442180295
```

##### Response

```json
{
  "data": {
    "email": "{USER_EMAIL}",
    "verified_email": true,
    "scope": "{PROVIDER_SCOPE}",
    "scopes": [{SCOPES}],
    "profile": { //user's profile },
    "display_name": "{Test Person}",
    "id": "d6faaff6b054393f28356ab7b38ad1bf-116254222860442180295"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```
