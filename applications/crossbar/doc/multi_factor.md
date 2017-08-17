### Multi Factor Authentication

#### About Multi Factor

Allow to configure a Multi Factor Authentication (MFA) provider for an account.

See [Kazoo Auth multi factor documentation](../../../core/kazoo_auth/doc/multi_factor.md) to learn more about available providers and their required settings.

##### Enable MFA for a Crossbar auth module

If you want to use multi factor authentication for a module, set the `multi_factor.enabled` to `true` for that authentication module. You can control if the multi factor settings can be applied to the account's children by `multi_factor.include_subaccounts`.

> **Note:** You can specify the `id` of multi factor provider settings. If you miss this value, system's default MFA provider will be used!

#### Multi Factor Authentication (MFA) flow summary

The MFA process in Kazoo is straight forward. You configured the Kazoo integrated MFA service provider, and enabling the multi factor for an authentication endpoint. User will authenticate as usual by its own Kazoo credential. If the first factor authentication passed, second-factor provider information (usually a signed token) would be returned to client with HTTP `401 Unauthorized` status.
User's client performs the second-factor authentication with the provider and sends provider response to Kazoo. If the provider validates user will be authenticated successful and a Kazoo token will be generated as usual otherwise if the second-factor provider response is not validated a HTTP `401 Unauthorized` will be returned.

#### Provider Configuration Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`enabled` | whether or not this configuration is enabled or not | `boolean` |  | `true`
`name` | A friendly name for the configuration | `string` |  | `true`
`provider_name` | multi factor provider name | `string` |  | `true`
`settings` | provider configuration | `object` |   | `false`


#### List Configured Multi Factor Providers

List configured multi factor providers and available system multi factor provider.

> GET /v2/accounts/{ACCOUNT_ID}/multi_factor

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/multi_factor
```

##### Response

```json
{
  "data": {
    "configured": [
      {
        "id": "c757665dca55edba2395df3ca6423f4f",
        "enabled": true,
        "name": "a nice day",
        "provider_name": "duo",
        "provider_type": "multi_factor"
      }
    ],
    "multi_factor_providers": [
      {
        "id": "duo",
        "enabled": false,
        "provider_name": "duo",
        "provider_type": "multi_factor"
      }
    ]
  },
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Configure a provider

Create configuration for a MFA provider. Provider type is `"multi_factor"`. Provider config should be in `"settings"`. See [Kazoo Auth Multi-Factor](../../../core/kazoo_auth/doc/multi_factor.md) to find out required configuration for each provider.

> PUT /v2/accounts/{ACCOUNT_ID}/multi_factor

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    --data '{"data": {"name": "another nice day", "enabled": true, "provider_name": "duo", "settings": {"integration_key": "{DUO_IKEY}", "secret_key": "{DUO_SKEY}", "application_secret_key": "{DUO_AKEY}", "api_hostname": "{DUO_HOST_NAME}", "duo_expire": 300,"app_expire": 3600}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/multi_factor
```

##### Response

```json
{
  "data": {
    "settings": {
      "secret_key": "{DUO_SKEY}",
      "integration_key": "{DUO_IKEY}",
      "duo_expire": 300,
      "application_secret_key": "{DUO_AKEY}",
      "app_expire": 3600,
      "api_hostname": "{DUO_HOST_NAME}"
    },
    "provider_name": "duo",
    "name": "another nice day",
    "enabled": true,
    "id": "c757665dca55edba2395df3ca6423f4f"
  },
  "revision": "{REVERSION}",
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Fetch a provider configuration

Get account's configuration of a provider.

> GET /v2/accounts/{ACCOUNT_ID}/multi_factor/{CONFIG_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/multi_factor/c757665dca55edba2395df3ca6423f4f
```

##### Response

```json
{
  "data": {
    "settings": {
      "secret_key": "{DUO_SKEY}",
      "integration_key": "{DUO_IKEY}",
      "duo_expire": 300,
      "application_secret_key": "{DUO_AKEY}",
      "app_expire": 3600,
      "api_hostname": "{DUO_HOST_NAME}"
    },
    "provider_name": "duo",
    "name": "another nice day",
    "enabled": true,
    "id": "c757665dca55edba2395df3ca6423f4f"
  },
  "revision": "{REVERSION}",
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Change a provider configuration

> POST /v2/accounts/{ACCOUNT_ID}/multi_factor/{CONFIG_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    --data '{"data": {"name": "another nice day with a change", "enabled": true, "provider_name": "duo", "settings": {"integration_key": "{DUO_IKEY}", "secret_key": "{DUO_SKEY}", "application_secret_key": "{DUO_AKEY}", "api_hostname": "{DUO_HOST_NAME}", "duo_expire": 300,"app_expire": 3600}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/multi_factor/c757665dca55edba2395df3ca6423f4f
```

##### Response

```json
{
  "data": {
    "settings": {
      "secret_key": "{DUO_SKEY}",
      "integration_key": "{DUO_IKEY}",
      "duo_expire": 300,
      "application_secret_key": "{DUO_AKEY}",
      "app_expire": 3600,
      "api_hostname": "{DUO_HOST_NAME}"
    },
    "provider_name": "duo",
    "name": "another nice day with a change",
    "enabled": true,
    "id": "c757665dca55edba2395df3ca6423f4f"
  },
  "revision": "{REVERSION}",
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Patch fields in a provider configuration

> PATCH /v2/accounts/{ACCOUNT_ID}/multi_factor/{CONFIG_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    --data '{"data": {"enabled": false}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/multi_factor/c757665dca55edba2395df3ca6423f4f
```

##### Response

```json
{
  "data": {
    "settings": {
      "secret_key": "{DUO_SKEY}",
      "integration_key": "{DUO_IKEY}",
      "duo_expire": 300,
      "application_secret_key": "{DUO_AKEY}",
      "app_expire": 3600,
      "api_hostname": "{DUO_HOST_NAME}"
    },
    "provider_name": "duo",
    "name": "another nice day with a change",
    "enabled": false,
    "id": "c757665dca55edba2395df3ca6423f4f"
  },
  "revision": "{REVERSION}",
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Remove a provider

> DELETE /v2/accounts/{ACCOUNT_ID}/multi_factor/{CONFIG_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/multi_factor/c757665dca55edba2395df3ca6423f4f
```

#### Get a summary of multi-factor login attempts

> GET /v2/accounts/{ACCOUNT_ID}/multi_factor/attempts

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/multi_factor/attempts
```

##### Response

```json
{
  "data": [
    {
      "id": "201702-09a979346eff06746e445a8cc1e574c4",
      "auth_type": "multi_factor",
      "auth_module": "cb_user_auth",
      "status": "failed",
      "message": "no multi factor authentication provider is configured",
      "timestamp": 63655033238,
      "client_ip": "10.1.0.2:8000",
    }
  ],
  "revision": "{REVERSION}",
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Fetch details of a multi-factor login attempts

> GET /v2/accounts/{ACCOUNT_ID}/multi_factor/attempts/{ATTEMPT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/multi_factor/attempts/201702-09a979346eff06746e445a8cc1e574c4
```

##### Response

```json
{
  "data": {
    "auth_type": "multi_factor",
    "status": "failed",
    "auth_module": "cb_user_auth",
    "message": "no multi factor authentication provider is configured",
    "client_headers": {
      "host": "10.1.0.2:8000",
      "connection": "keep-alive",
      "content-length": "83",
      "accept": "application/json, text/javascript, */*; q=0.01",
      "x-auth-token": "undefined",
      "user-agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/61.0.3163.39 Safari/537.36",
      "origin": "http://127.0.0.1:3000",
      "content-type": "application/json",
      "dnt": "1",
      "referer": "http://127.0.0.1:3000/",
      "accept-encoding": "gzip, deflate",
      "accept-language": "en-US,en;q=0.8"
    },
    "client_ip": "10.1.0.2:8000",
    "crossbar_request_id": "5dd9a7b69f74b3c09ca065316096b83e",
    "timestamp": 63655033238,
    "metadata": {
        "owner_id": "b6205d9a4a62d8e971c2d8f177676130",
        "account_id": "a391d64a083b99232f6d2633c47432e3"
    },
    "id": "201702-09a979346eff06746e445a8cc1e574c4"
  },
  "revision": "{REVERSION}",
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```
