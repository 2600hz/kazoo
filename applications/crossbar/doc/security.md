### Security

Crossbar API to configure authentication for an account.

#### About Security

Crossbar authenticator modules can have their own account's version configuration to control some aspect of them like enabling/disabling the module or use multi factor authenticator for that specific module.

> **Note:** This API endpoint is _only_ configuring the authentication for a account, for configuring the system, you should use [System Configuration](./system_configs.md) instead. System config category is `crossbar.auth`.

##### How Crossbar is looking for authentication configuration

Configuration is the merged result of the account's configuration and all its parent's account up to the first reseller, then the system config itself. So the account inherits parent and reseller account and system config.

#### Enable Multi Factor Authentication for a Crossbar auth module

If you want to use multi factor authentication for a module, set the `multi_factor.enabled` to `true`. You can control if the multi factor settings can be applied to the account's children by `multi_factor.include_subaccounts`.

When setting `configuration_id` of the multi-factor, you have to set the Account ID which contains the that configuration too.

Only a parent Account or the same Account can set `configuration_id` and `account_id`.

#### Account Auth Configuration Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`cb_api_auth` |   | [#/definitions/auth_module_config](#auth.module.config) |   | `false`
`cb_auth` |   | [#/definitions/auth_module_config](#auth.module.config) |   | `false`
`cb_ip_auth` |   | [#/definitions/auth_module_config](#auth.module.config) |   | `false`
`cb_ubiquiti_auth` |   | [#/definitions/auth_module_config](#auth.module.config) |   | `false`
`cb_user_auth` |   | [#/definitions/auth_module_config](#auth.module.config) |   | `false`

#### Auth Module Configuration Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`enabled` | whether or not this authentication module is enabled | `boolean` |  | `true`
`log_failed_attempts` | should log failed logging attempts | `boolean` | `true` | `false`
`log_successful_attempts` | should log successful logging attempts | `boolean` | `false` | `false`
`multi_factor` | control multi factor authentications for this module | `object` |   | `false`
`multi_factor.account_id` | ID of the account that contains the multi factor configuration | `string` |  | `false`
`multi_factor.configuration_id` | document ID contains the multi factor configuration | `string` |  | `false`
`multi_factor.enabled` | turn on/off multi factor authentications for this module | `boolean` |  | `true`
`multi_factor.include_subaccounts` | should this multi factor authentication settings be applied when used by sub-accounts | `boolean` | `false` | `false`
`token_auth_expiry_s` | expiration period of the JWT token (seconds) | `integer` |  | `false`

#### Get a List of Available Auth Module

List of all available auth module to be configured.

> GET /v2/security

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/security
```

##### Response

```json
{
  "data": {
    "available_auth_modules": [
      "cb_api_auth",
      "cb_auth",
      "cb_ip_auth",
      "cb_ubiquiti_auth",
      "cb_user_auth"
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

#### Fetch All Configurations

Get all configured authentocator module on the account alongside the default settings of merged result of account itself and its parents, reseller and system.

> GET /v2/accounts/{ACCOUNT_ID}/security

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/security
```

##### Response

```json
{
  "data": {
    "account": {},
    "inherited_config": {
      "cb_user_auth": {
        "enabled": true,
        "token_auth_expiry_s": 3600,
        "log_failed_attempts": true,
        "log_successful_attempts": true
      },
      "cb_api_auth": {
        "enabled": true,
        "token_auth_expiry_s": 3600,
        "log_failed_attempts": true,
        "log_successful_attempts": true
      },
      "cb_auth": {
        "enabled": true,
        "token_auth_expiry_s": 3600,
        "log_failed_attempts": true,
        "log_successful_attempts": true
      },
      "cb_ip_auth": {
        "enabled": true,
        "token_auth_expiry_s": 3600,
        "log_failed_attempts": true,
        "log_successful_attempts": true
      },
      "cb_ubiquiti_auth": {
        "enabled": true,
        "token_auth_expiry_s": 3600,
        "log_failed_attempts": true,
        "log_successful_attempts": true
      }
    }
  },
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Change

Customize modules config for the account. Set what settings you want here, crossbar always get the merged config from account and hierarchy.

> POST /v2/accounts/{ACCOUNT_ID}/security

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{ "data": { "auth_modules" :{ "cb_user_auth": { "enabled": true, "token_auth_expiry_s": 604800}, "cb_api_auth": { "enabled": true } } } }'
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/security
```

##### Response

```json
{
  "data": {
    "auth_modules": {
      "cb_user_auth": {
        "token_auth_expiry_s": 604800,
        "enabled": true,
        "log_failed_attempts": true,
        "log_successful_attempts": true
      },
      "cb_api_auth": {
        "token_auth_expiry_s": 604800,
        "log_successful_attempts": true,
        "log_failed_attempts": true,
        "enabled": true
      }
    },
    "id": "configs_crossbar.auth"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Patch

Patch fields of config for the account customization.

> PATCH /v2/accounts/{ACCOUNT_ID}/security


```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{ "data": { "auth_modules" :{ "cb_api_auth": { "enabled": false } } } }'
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/security
```

##### Response

```json
{
  "data": {
    "auth_modules": {
      "cb_user_auth": {
        "token_auth_expiry_s": 604800,
        "enabled": false,
        "log_failed_attempts": true,
        "log_successful_attempts": true
      },
      "cb_api_auth": {
        "token_auth_expiry_s": 604800,
        "log_successful_attempts": true,
        "log_failed_attempts": true,
        "enabled": false
      }
    },
    "id": "configs_crossbar.auth"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Remove

Delete account's auth config.

> DELETE /v2/accounts/{ACCOUNT_ID}/security

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/security
```

##### Response

```json
{
  "data": {
    "auth_modules": {
      "cb_user_auth": {
        "token_auth_expiry_s": 604800,
        "enabled": false,
        "log_failed_attempts": true,
        "log_successful_attempts": true
      },
      "cb_api_auth": {
        "token_auth_expiry_s": 604800,
        "log_successful_attempts": true,
        "log_failed_attempts": true,
        "enabled": false
      }
    },
    "id": "configs_crossbar.auth"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Get a List of All Login Attempts

> GET /v2/accounts/{ACCOUNT_ID}/security/attempts

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/security/attempts
```

##### Response

```json
{
  "page_size": 1,
  "data": [
    {
      "id": "201707-5e9c6dc29efb34d87c0a06e8f613b1fd",
      "auth_type": "jwt_auth_token",
      "auth_module": "cb_user_auth",
      "status": "success",
      "message": "authentiaction resulted in token creation",
      "timestamp": 63667032239,
      "client_ip": "10.1.0.2"
    }
  ],
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

#### Get a Login Attempt Details

> GET /v2/accounts/{ACCOUNT_ID}/security/attempts/{ATTEMPT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/security/attempts/201707-5e9c6dc29efb34d87c0a06e8f613b1fd
```

##### Response

```json
{
  "data": {
    "auth_type": "jwt_auth_token",
    "status": "success",
    "auth_module": "cb_user_auth",
    "message": "authentiaction resulted in token creation",
    "auth_config_origin": "system",
    "multi_factor_config_origin": "system",
    "client_headers": {
      "host": "10.1.0.2:8000",
      "connection": "keep-alive",
      "content-length": "82",
      "accept": "application/json, text/javascript, */*; q=0.01",
      "x-auth-token": "undefined",
      "user-agent": "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.61 Safari/537.36",
      "origin": "http://127.0.0.1:3000",
      "content-type": "application/json",
      "dnt": "1",
      "referer": "http://127.0.0.1:3000/",
      "accept-encoding": "gzip, deflate",
      "accept-language": "en-US,en;q=0.8"
    },
    "client_ip": "10.1.0.2",
    "crossbar_request_id": "a6edc00018ebd9c7c991fbddf3677fcb",
    "timestamp": 63667032239,
    "metadata": {
      "owner_id": "0528dc7bbbf94bcc5df7d74d808a4ec0",
      "account_id": "6134cc9aa43ffaee3e3f0c9a84113d6e"
    },
    "id": "201707-5e9c6dc29efb34d87c0a06e8f613b1fd"
  },
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```
