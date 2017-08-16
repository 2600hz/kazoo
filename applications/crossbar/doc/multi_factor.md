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

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/multi_factor

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/multi_factor
```

```json
{
  "data": {
    "configured": [
      {
        "id": "c757665dca55edba2395df3ca6423f4f",
        "provider_name": "duo",
        "provider_type": "multi_factor",
        "enabled": true
      }
    ],
    "available_system_provider": [
      "duo"
    ]
  },
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Configure a provider

Creates configuration for a MFA provider. Provider type is `"multi_factor"`. Provider config should be in `"settings"`. See [Kazoo Auth Multi-Factor](../../../core/kazoo_auth/doc/multi_factor.md) to find out required configuration for each provider.

> PUT /v2/accounts/{ACCOUNT_ID}/multi_factor

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/multi_factor
    --data '{"data": {"provider_name": "duo", "provider_type": "multi_factor", "enabled": true, "settings": {"integration_key": "{DUO_IKEY}", "secret_key": "{DUO_SKEY}", "application_secret_key": "{DUO_AKEY}", "api_hostname": "{DUO_HOST_NAME}", "duo_expire": 300,"app_expire": 3600}}}'
```

```json
{
  "data": {
    "provider_name": "duo",
    "provider_type": "multi_factor",
    "enabled": true,
    "settings": {
      "integration_key": "{DUO_IKEY}",
      "secret_key": "{DUO_SKEY}",
      "application_secret_key": "{DUO_AKEY}",
      "api_hostname": "{DUO_HOST_NAME}",
      "duo_expire": 300,
      "app_expire": 3600
    },
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

```json
{
  "data": {
    "provider_name": "duo",
    "provider_type": "multi_factor",
    "enabled": true,
    "settings": {
      "integration_key": "{DUO_IKEY}",
      "secret_key": "{DUO_SKEY}",
      "application_secret_key": "{DUO_AKEY}",
      "api_hostname": "{DUO_HOST_NAME}",
      "duo_expire": 300,
      "app_expire": 3600
    },
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
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/multi_factor/c757665dca55edba2395df3ca6423f4f
    --data '{"data": {"provider_name": "duo", "provider_type": "multi_factor", "enabled": true, "settings": {"integration_key": "{DUO_IKEY}", "secret_key": "{DUO_SKEY}", "application_secret_key": "{DUO_AKEY}", "api_hostname": "{DUO_HOST_NAME}", "duo_expire": 500,"app_expire": 3600}}}'
```

```json
{
  "data": {
    "provider_name": "duo",
    "provider_type": "multi_factor",
    "enabled": true,
    "settings": {
      "integration_key": "{DUO_IKEY}",
      "secret_key": "{DUO_SKEY}",
      "application_secret_key": "{DUO_AKEY}",
      "api_hostname": "{DUO_HOST_NAME}",
      "duo_expire": 400,
      "app_expire": 3600
    },
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

#### Patch a field in a provider configuration

> PATCH /v2/accounts/{ACCOUNT_ID}/multi_factor/{CONFIG_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/multi_factor/c757665dca55edba2395df3ca6423f4f
    --data '{"data": {"enabled": false}}'
```

```json
{
  "data": {
    "provider_name": "duo",
    "provider_type": "multi_factor",
    "enabled": false,
    "settings": {
      "integration_key": "{DUO_IKEY}",
      "secret_key": "{DUO_SKEY}",
      "application_secret_key": "{DUO_AKEY}",
      "api_hostname": "{DUO_HOST_NAME}",
      "duo_expire": 400,
      "app_expire": 3600
    },
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

```json
{
  "data": [
    {
      "id": "201702-09a979346eff06746e445a8cc1e574c4",
      "auth_type": "multi_factor",
      "debug_type": "failed",
      "message": "no multi factor authentication provider is configured",
      "timestamp": 63655033238
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

> GET /v2/accounts/{ACCOUNT_ID}/multi_factor/attempts/201702-09a979346eff06746e445a8cc1e574c4

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/multi_factor/attempts/{ATTEMPT_ID}
```

```json
{
  "data": {
    "auth_type": "multi_factor",
    "debug_type": "failed",
    "message": "no multi factor authentication provider is configured",
    "timestamp": 63655033238,
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
