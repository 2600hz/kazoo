# System Configs

## About System Configs

!!! note
    You must be `superduper_admin` to access this resource. Also make sure that the `system_configs` crossbar module has been enabled. `sup crossbar_maintenance start_module cb_system_configs`.

Manipulate documents in the `system_config` database via Crossbar.

A system configuration document is a JSON document of certain syntactic type (each node value must be a JSON object of type `system_config.$config_name`). REST API allows to address both documents and document nodes. Default node of the configuration document is treated specially, see below.

In order to avoid confusion configuration document nodes we call document sections, each such section is a JSON object representing a configuration object for Kazoo node or zone.

General idea is to return (with `with_defaults=true` URI option) the effective values for configuration, therefore a complex merge down is performed: document values are merged with document default section, and then merged with schema defaults.

Therefore on save actions (`PUT`/`POST`/`PATCH`) a reverse operation is performed: a difference from defaults is calculated and stored to the document (if any). Thus by setting a value to schema default will effectively remove it from the document on save action.

### Documents

`GET` always returns either an empty document or stored document. The document always has a default section included, populated with default values deduced from schema.

`POST` expects to receive a complete configuration document. The document must be valid, each section of the document must pass the validation against `system_config.$config_name` schema. Before saving the difference with provided default is calculated, and then the difference with schema defaults is calculated, and then the result of this is stored, meaning if section value is equal to default section value it will not be stored to the document. The actual (stored) values are then returned as POST results.

`PATCH` will merge provided changes with stored document, and then the resulting document is valid then the same store procedure is applied as in POST. The actual stored values are then returned as PATCH result.

`DELETE` deletes the document (or wipes a section). Attempt to delete non-existing configuration object will generate an HTTP `404` error.

### Sections

`GET` always returns either an empty document or stored values.

`POST` expects to receive a valid configuration document (against schema `system_config.$config_name`). If the document is valid, then a difference is calculated with configuration document default section, and then the difference is calculated with schema defaults, and then the result is stored. For default section only the difference from schema defaults are calculated. The actual (stored) values are returned as result of `POST` request.

`PATCH` will merge provided changes with stored section, and then the result is validated against schema. If result is valid, then the same procedure is applied as in `POST`. The actual (stored) values are returned as `PATCH` result.

`DELETE` deletes the section specified. If configuration document doesn't exist (or section in the document), then the request will fail with HTTP 404.

Defaults
--------

In order to have effective configuration values one must provide `with_defaults=true` URI parameter for GET requests (either to get document or document section). With this parameter provided a configuration merge down as described above is performed.

#### Schema

Schema for system_config documents



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`(?!id\b)(?!default\b)^.+@.+$` | Node-specific settings - these take highest precedence | `object()` |   | `false` |  
`(?!id\b)(?!default\b)^[a-zA-Z0-9.]+$` | Zone-specific settings - these are checked if a node-specific setting is not defined | `object()` |   | `false` |  
`default` | default settings that apply to all nodes/zones if not defined | `object()` | `{}` | `true` |  



## Fetch List of Configured System Configs Categories

> GET /v2/system_configs

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs
```

**Response**

```json
{
  "next_start_key": "notification.ported",
  "page_size": 50,
  "data": [
    "notification.port_unconfirmed",
    "notification.port_scheduled",
    "notification.port_request_admin",
    "notification.port_request",
    "notification.port_rejected",
    "notification.port_pending",
    "notification.port_comment",
    "notification.port_cancel",
    "notification.password_recovery",
    "notification.new_user",
    "notification.new_account",
    "notification.low_balance",
    "notification.first_occurrence",
    "notification.fax_outbound_to_email",
    "notification.fax_outbound_error_to_email",
    "notification.fax_inbound_to_email",
    "notification.fax_inbound_error_to_email_filtered",
    "notification.fax_inbound_error_to_email",
    "notification.deregister",
    "notification.denied_emergency_bridge",
    "notification.customer_update",
    "notification.cnam_request",
    "notification.account_zone_change",
    "modb",
    "milliwatt",
    "media",
    "kazoo_endpoint",
    "kazoo_couch",
    "kapps_controller",
    "hangups",
    "fax",
    "ecallmgr",
    "datamgr",
    "crossbar.resources",
    "crossbar.media",
    "crossbar.callflows",
    "crossbar.auth",
    "crossbar.accounts",
    "crossbar",
    "conferences",
    "cdr",
    "callflow.park",
    "callflow",
    "call_command",
    "braintree",
    "blackhole",
    "auth",
    "alerts",
    "accounts"
  ],
  "timestamp": "2017-03-29T23:51:00",
  "version": "4.0.0",
  "node": "{NODE}",
  "request_id": "4b9bb77a555a0130d5d862964ba3c834",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Fetch Specific Config Category Default Values

> GET /v2/system_configs/{SYSTEM_CONFIG_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}
```

**Response**

```json
{
    "data": {
        "default": {
            "acl_request_timeout_fudge_ms": 100,
            "acl_request_timeout_ms": 2000
        },
        "id": "sysconf"
    },
    "timestamp": "2017-03-29T23:15:46",
    "version": "4.0.0",
    "node": "{NODE}",
    "request_id": "aa044445a32ff469970873e49992efb7",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

## Create System Config Category

> PUT /v2/system_configs/{SYSTEM_CONFIG_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}
```

See [Update System Config Category](#update-system-config-category).

## Update System Config Category

> POST /v2/system_configs/{SYSTEM_CONFIG_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}
```

**Example Request Body**

```json
{
    "data": {
        "default": {
            "acl_request_timeout_fudge_ms": 100,
            "acl_request_timeout_ms": 2000
        },
        "id": "sysconf",
        "node": {
            "acl_request_timeout_fudge_ms": 101
        }
    }
}
```

**Response**

```json
{
    "data": {
        "node": {
            "acl_request_timeout_fudge_ms": 101
        },
        "id": "sysconf"
    },
    "revision": "{REVISION}",
    "timestamp": "2017-03-29T23:15:46",
    "version": "4.0.0",
    "node": "{NODE}",
    "request_id": "34496796b71921c4908d54ffcfba815e",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

## Patch

> PATCH /v2/system_configs/{SYSTEM_CONFIG_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}
```

**Example Request Body**

```json
{
    "data": {
        "node": {
            "acl_request_timeout_fudge_ms": 102
        }
    }
}
```

**Response**

```json
{
    "data": {
        "node": {
            "acl_request_timeout_fudge_ms": 102
        },
        "id": "sysconf"
    },
    "revision": "{REVISION}",
    "timestamp": "2017-03-29T23:15:46",
    "version": "4.0.0",
    "node": "{NODE}",
    "request_id": "8c8b81cc498d7cb12af17c6e501a3bd6",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

## Fetch Config Category with All Node Values and Default Values

> GET /v2/system_configs/{SYSTEM_CONFIG_ID}?with_defaults=true

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}
```

**Response**

```json
{
    "data": {
        "node": {
            "acl_request_timeout_fudge_ms": 102,
            "acl_request_timeout_ms": 2000
        },
        "default": {
            "acl_request_timeout_fudge_ms": 100,
            "acl_request_timeout_ms": 2000
        },
        "id": "sysconf"
    },
    "timestamp": "2017-03-29T23:15:46",
    "version": "4.0.0",
    "node": "{NODE}",
    "request_id": "9dc532fbc603ff2c89a8b28b8d4b9bf5",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

## Remove

> DELETE /v2/system_configs/{SYSTEM_CONFIG_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}
```

**Response**

```json
{
    "data": {
        "node": {
            "acl_request_timeout_fudge_ms": 102
        },
        "id": "sysconf"
    },
    "revision": "{REVISION}",
    "timestamp": "2017-03-29T23:15:47",
    "version": "4.0.0",
    "node": "{NODE}",
    "request_id": "69d9cce540246c6d733aab7806a85fab",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

## Fetch Specific Config Category For A Node

> GET /v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}
```

**Response**

```json
{
    "data": {
        "id": "sysconf\/node"
    },
    "timestamp": "2017-03-29T23:15:47",
    "version": "4.0.0",
    "node": "{NODE}",
    "request_id": "796768266b9bae69dbd2379f4dcd02fe",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

## Change System Config Value For A NODE

> POST /v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}
```

**Example Request Body**

```json
{
    "data": {
        "id": "sysconf\/node",
        "acl_request_timeout_fudge_ms": 101
    }
}
```

**Response**

```json
{
    "data": {
        "acl_request_timeout_fudge_ms": 101,
        "id": "sysconf\/node"
    },
    "revision": "{REVISION}",
    "timestamp": "2017-03-29T23:15:47",
    "version": "4.0.0",
    "node": "{NODE}",
    "request_id": "1e37f7cd29533fca6232552e4ba2324e",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

## Patch System Config Value For A NODE

> PATCH /v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}
```

**Example Request Body**

```json
{
    "data": {
        "acl_request_timeout_fudge_ms": 102
    }
}
```

**Response**

```json
{
    "data": {
        "acl_request_timeout_fudge_ms": 102,
        "id": "sysconf\/node"
    },
    "revision": "{REVISION}",
    "timestamp": "2017-03-29T23:15:47",
    "version": "4.0.0",
    "node": "{NODE}",
    "request_id": "67862fe74310955f4a53db3fa8473e35",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

## Fetch with defaults

> GET /v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}?with_defaults=true
```

**Response**

```json
{
    "data": {
        "acl_request_timeout_fudge_ms": 102,
        "acl_request_timeout_ms": 2000,
        "id": "sysconf\/node"
    },
    "timestamp": "2017-03-29T23:15:47",
    "version": "4.0.0",
    "node": "{NODE}",
    "request_id": "a0e221dc821d5d49a32555155344ea10",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```

## Remove

> DELETE /v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}
```

**Response**

```json
{
    "data": {
        "id": "sysconf\/node"
    },
    "revision": "{REVISION}",
    "timestamp": "2017-03-29T23:15:47",
    "version": "4.0.0",
    "node": "{NODE}",
    "request_id": "2dfd98b331aa180d8f07f9ee7488dbe3",
    "status": "success",
    "auth_token": "{AUTH_TOKEN}"
}
```
