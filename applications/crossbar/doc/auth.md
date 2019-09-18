# Authentication Token Operations

Crossbar module for operations on JWT Token, SSO/OAuth tokens.

## About Authentication

### Schema for auth.callback

#### Schema

callback for authentication providers



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`client_id` | client id, usually application id for OAuth providers | `string()` |   | `true` |
`code` | access code emitted by provider | `string()` |   | `true` |
`provider` | provider | `string()` |   | `true` |
`redirect_uri` | redirect URI | `string()` |   | `true` |
`state` | state | `string()` |   | `false` |



### Schema for auth.provider

#### Schema

provider settings for authentication



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`id` |   | `string()` |   | `true` |



### Schema for auth.app

#### Schema

application description for authentication providers



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`client_id` | client_id, usually application id for OAuth providers | `string()` |   | `true` |
`email` | email for application | `string()` |   | `false` |
`provider` | provider | `string()` |   | `true` |
`secret` | secret for application | `string()` |   | `true` |



## Resetting System (Kazoo) Signature Secret

System signature secret and the subject signature is being used to sign the signature in the JWT token that Kazoo is issuing.

If you feel that this system secret is compromised, use this API to reset it.

!!! danger
    Resetting system signature secret will invalidate *all* issued token! In other words all login users will be logout from the system and can't make any further request until login again. Use this API if you feel the system secret is compromised only.

!!! note
    Only super duper admin can reset system secret!

> PUT /v2/auth

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{ "action": "reset_signature_secret", "data": {} }' \
    http://{SERVER}:8000/v2/auth
```

**Responses**

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

## Resetting an Account or a User Signature Secret

System signature secret and the subject signature is being used to sign the signature in the JWT token that Kazoo is issuing.

If you feel that an account or a user secret is compromised, use this API to reset it.

!!! danger
    Resetting signature secret will invalidate user's issued token! In other words if the user is already login, it will be logout from the system and can't make any further request until login again.

!!! note
    Only the user or an account admin can reset the user's secret.

### To Reset an Account Signature Secret

> PUT /v2/accounts/{ACCOUNT_ID}/auth

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{ "action": "reset_signature_secret", "data": {} }' \
    http://{SERVER}:8000/v2/accounts/290ac723eb6e73dd4a0adcd77785e04e/auth
```

### To Reset a User Signature Secret

> PUT /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/auth

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{ "action": "reset_signature_secret", "data": {} }' \
    http://{SERVER}:8000/v2/accounts/290ac723eb6e73dd4a0adcd77785e04e/users/3bedb94b3adfc4873a548b41d28778b5/auth
```

#### Response

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

## Request a new token while current token is still valid

!!! note
    This will fail if your password or User Signature Secret was reset.

> PUT /v2/auth

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{ "action": "refresh_token", "data": {} }'
    http://{SERVER}:8000/v2/auth
```

## Get a List of Registered SSO App

This list all registered Single Sign On applications for the account's reseller. Account ID is determined in order by first from the query string `account_id`, or from request path (`/accounts/{ACCOUNT_ID}`) or from authenticated user's account id.

> GET /v2/auth/apps

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/apps
```

**Responses**

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

## List SSO Provider

Get a list of all Single Sign On provider.

> GET /v2/auth/providers

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers
```

**Responses**

```json
{
  "page_size": 3,
  "start_key": [
    "oauth"
  ],
  "data": [
    {
      "id": "salesforce",
      "provider_type": "oauth"
    },
    {
      "id": "office365",
      "provider_type": "oauth"
    },
    {
      "id": "google",
      "provider_type": "oauth"
    },
    {
      "id": "duo",
      "enabled": false,
      "name": "System Default Provider",
      "provider_name": "duo",
      "provider_type": "multi_factor"
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

## List User's Linked SSO Applications

Get a list of the linked Single Sign On applications for the authenticated user.

> GET /v2/auth/links

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links
```

**Responses**

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
  ],
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## List Public Key Cryptography

Lists a list of keys which is being used to sign and validate JWT tokens.

> GET /v2/auth/keys

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/keys
```

**Responses**

```json
{
  "page_size": 1,
  "data": [
    "96247ed90b4bec8294c09ae6ece923a2"
  ],
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Authenticate a User with a SSO (Create a Token from SSO Response)

After a user authenticate with Single Sign On provider, use this API to send the provider response to Crossbar to login and create a token.

> PUT /v2/auth/callback

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{ "data": { "redirect_uri": "{MONSTER_UI_IP}", "client_id": "{OAUTH_CLIENT_ID}", "provider": "{OAUTH_PROVIDER_NAME}", "code": "{OAUTH_RESP_CODE}" } }'
    http://{SERVER}:8000/v2/auth/callback
```

### Response when no User is Linked yet

If this is the first time that the user is authenticating using this SSO provider, Kazoo returns and empty response indicating that user should first login using it's own Kazoo credentials first to link the SSO application with its user. After login the user is linked with the application and it no need to manual login again.

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

### Response when the User is Linked

```json
{
  "page_size": 1,
  "data": {
    "owner_id": "{OWNER_ID}",
    "account_id": "{ACCOUNT_ID}",
    "reseller_id": "{RESELLER_ID}",
    "account_name": "{ACCOUNT_NAME}",
    "language": "{LANG}",
    "apps": []
  },
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Validate and Authorize a Foreign SSO Token/App

> PUT /v2/auth/authorize

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/authorize
```

## Get Token Information (Query String version)

Returns the information encoded in the specified authentication token (from query string `token` parameter).

> GET /v2/auth/tokeninfo

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/tokeninfo?token={AN_AUTH_TOKEN}
```

**Responses**

```json
{
  "page_size": 1,
  "data": {
    "owner_id": "{OWNER_ID}",
    "account_id": "{ACCOUNT_ID}",
    "reseller_id": "{RESELLER_ID}",
    "account_name": "{ACCOUNT_NAME}",
    "language": "{LANG}",
    "apps": []
  },
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Get Token Information (Request Body version)

Returns the information encoded in the specified authentication token (from request body `token` parameter).

> POST /v2/auth/tokeninfo

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{ "data": { "token": "{AN_AUTH_TOKEN}" } }'
    http://{SERVER}:8000/v2/auth/tokeninfo
```

**Responses**

```json
{
  "page_size": 1,
  "data": {
    "owner_id": "{OWNER_ID}",
    "account_id": "{ACCOUNT_ID}",
    "reseller_id": "{RESELLER_ID}",
    "account_name": "{ACCOUNT_NAME}",
    "language": "{LANG}",
    "apps": []
  },
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Get a SSO Application

Get a Single Sign On application by it's ID.

> GET /v2/auth/apps/{APP_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/apps/{APP_ID}
```

**Responses**

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
```

## Change a SSO Application

> POST /v2/auth/apps/{APP_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/apps/{APP_ID}
```

## Remove a SSO Application

> DELETE /v2/auth/apps/{APP_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/apps/{APP_ID}
```

## Get a Public Key

Get a public key used for signing the JWT tokens issued by system.

!!! note
    To get the public key in form of a PEM file set `Accept` header as `application/x-pem-file`.

> GET /v2/auth/keys/{KEY_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/keys/96247ed90b4bec8294c09ae6ece923a2
```

**Responses**

```json
{
  "page_size": 1,
  "data": {
     "public_key_pem": "-----BEGIN RSA PUBLIC KEY-----\n{A_PUBLIC_KEY}\n-----END RSA PUBLIC KEY-----\n\n"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Reset a Private Key

Reset the private key of the system used to signing and verifying issued JWT tokens. If you feel that the private key is compromised, use this API to generate a new private and public key.

!!! danger
    Resetting system private will invalidate *all* issued token! In other words all login users will be logout from the system and can't make any further request until login again. Use this API if you feel the system private key is compromised only.

!!! note
    Only super duper admin can reset system private key!

> PUT /v2/auth/keys/{KEY_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{ "action": "reset_private_key", "data": {} }'
    http://{SERVER}:8000/v2/auth/keys/96247ed90b4bec8294c09ae6ece923a2
```

**Responses**

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

## Fetch a SSO Provider Information

> GET /v2/auth/providers/{PROVIDER_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers/google
```

**Responses**

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

## Make changes to SSO Provider

!!! note
    Only super duper admin can make changes to SSO provider!

> POST /v2/auth/providers/{PROVIDER_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers/{PROVIDER_ID}
```

## Remove a SSO Provider

!!! note
    Only super duper admin can delete a SSO provider!

> DELETE /v2/auth/providers/{PROVIDER_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers/{PROVIDER_ID}
```

## Get an User's Linked SSO Applications Information

> GET /v2/auth/links/{LINK_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links/d6faaff6b054393f28356ab7b38ad1bf-116254222860442180295
```

**Responses**

```json
{
  "data": {
    "email": "{USER_EMAIL}",
    "verified_email": true,
    "scope": "{PROVIDER_SCOPE}",
    "scopes": ["{SCOPES}"],
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

## Link an User to a SSO Application

When the user is signing on with A Single Sign On provider for the first time, it should login with its own Kazoo credentials one more time, and then make a request to this API to link its Kazoo's user to the SSO. After that the user can sign in with SSO regularly and no need to use Kazoo credentials again.

> PUT /v2/auth/links/{LINK_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links/d6faaff6b054393f28356ab7b38ad1bf-116254222860442180295
```

**Responses**

```json
{
  "data": {
    "email": "{USER_EMAIL}",
    "verified_email": true,
    "scope": "{PROVIDER_SCOPE}",
    "scopes": ["{SCOPES}"],
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

## Unlink a user from SSO Application

> DELETE /v2/auth/links/{LINK_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links/d6faaff6b054393f28356ab7b38ad1bf-116254222860442180295
```

**Responses**

```json
{
  "data": {
    "email": "{USER_EMAIL}",
    "verified_email": true,
    "scope": "{PROVIDER_SCOPE}",
    "scopes": ["{SCOPES}"],
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
