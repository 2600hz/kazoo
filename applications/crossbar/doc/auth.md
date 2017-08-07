### Authentication

#### About Authentication

#### Schema



#### Patch

> *Caution:* Reseting system identity secret will invalidate *all* issued token! In other words all logined users will be logout from the system and can't make any further request until loging again. Use this API if you feel the system secret is compromised only.

> *Note:* Only super duper admin can reset system secert!

> PATCH /v2/auth/identity_secret

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/identity_secret
```

##### Response

Empty response with normal Crossbar response envlope.

```json
{
  "timestamp": "{TIMESTAMP}",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{REQUEST_ID}"
}
```

#### Get a List of Registered OAuth App

This list all registered OAuth applications for the account. Account is the a reseller or a master account. `account_id` is determined by the request paths (`/accounts/{ACCOUNT_ID}`) or from authenticated user's account id.

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
  "auth_token": "{REQUEST_ID}"
}
```

#### List OAuth Provider

Get a list of all OAuth provider.

> GET /v2/auth/providers

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers
```

##### Response

```json
{
  "page_size": 4,
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
  "auth_token": "{REQUEST_ID}"
}
```

#### Fetch

> GET /v2/auth/links

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links
```

#### Create

> PUT /v2/auth/callback

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/callback
```

#### Create

> PUT /v2/auth/authorize

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/authorize
```

#### Fetch

> GET /v2/auth/tokeninfo

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/tokeninfo
```

#### Change

> POST /v2/auth/tokeninfo

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/tokeninfo
```

#### Fetch

> GET /v2/auth/keys/public

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/keys/public
```

#### Patch

> PATCH /v2/auth/keys/private

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/keys/private
```

#### Fetch

> GET /v2/auth/apps/{APP_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/apps/{APP_ID}
```

#### Change

> POST /v2/auth/apps/{APP_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/apps/{APP_ID}
```

#### Remove

> DELETE /v2/auth/apps/{APP_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/apps/{APP_ID}
```

#### Fetch

> GET /v2/auth/providers/{PROVIDER_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers/{PROVIDER_ID}
```

#### Change

> POST /v2/auth/providers/{PROVIDER_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers/{PROVIDER_ID}
```

#### Remove

> DELETE /v2/auth/providers/{PROVIDER_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/providers/{PROVIDER_ID}
```

#### Fetch

> GET /v2/auth/links/{LINK_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links/{LINK_ID}
```

#### Create

> PUT /v2/auth/links/{LINK_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links/{LINK_ID}
```

#### Remove

> DELETE /v2/auth/links/{LINK_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links/{LINK_ID}
```

