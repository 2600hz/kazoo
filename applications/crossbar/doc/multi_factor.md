### Multi Factor Authentication

#### About Multi Factor

After enabling multi-factor for your desired authentication method (see [Crossbar Auth Configs](./authentication_configuration.md)), you have to configure your MFA provider using this API endpoint.

See [Kazoo Auth multi factor documentation](../../../core/kazoo_auth/doc/multi_factor.md) to learn more about available providers and how to setting them up.

> **Note:** This only provides an API to create/change/get MFA settings. For more information about how MFA works and what settings are required please see Kazoo Auth MFA documentation and Crossbar Auth Configuration.

#### Schema


Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`enabled` | whether or not this authentication provider is enabled | `boolean` | `true` | `false`
`provider_name` | authentication provider name | `string` |   | `true`
`provider_type` | the type of authentication provider | `multi_factor` |   | `true`
`settings` | provider configuration | `object` |   | `false`


#### Get a summary of configured providers

Lists configured authentication providers for `{ACCOUNT_ID}`. Also provides all of the available providers which you can configure.

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

Create configuration for a provider by issuing below request. You have to provide the name of provider and and the type of the provider
which for MFA is always `"multi_factor"`. `"settings"` contains the configs necessary for this provider.

See [Kazoo Auth multi-factor documentation](../../../core/kazoo_auth/doc/multi_factor.md) to find out required configuration for each provider.

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

Gets account's configuration of a provider in `{CONFIG_ID}`.

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

#### Patch

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

#### Get a summary of login attempts

If the crossbar authentication modules is configured to save a debug log of login attempts, use this to get summary of the logs.

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

#### Fetch details of a login attempts

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
    "auth_config_origin": "{ACCOUNT_ID}",
    "mfa_config_origin": "{ACCOUNT_ID}",
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
