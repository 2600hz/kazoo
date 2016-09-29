### System_configs

Manipulate documents in the `system_config` database via Crossbar.

#### About System_configs

You must be `super_duper_admin` to access this resource.

#### Schema



#### List all known configs

> GET /v2/system_configs

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
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
        "notification.fax_inbound_error_to_email",
        "notification.deregister",
        "notification.denied_emergency_bridge",
        "notification.customer_update",
        "notification.cnam_request",
        "modb",
        "metaflows",
        "media",
        "konami",
        "kazoo.pdf",
        "kazoo_couch",
        "kapps_maintenance",
        "kapps_controller",
        "hangups",
        "fax",
        "ecallmgr",
        "doodle",
        "datamgr",
        "crossbar.token_restrictions",
        "crossbar.resources",
        "crossbar.port_requests",
        "crossbar.phone_numbers",
        "crossbar.notifications",
        "crossbar.media",
        "crossbar.contact_list",
        "crossbar.cdrs",
        "crossbar.callflows",
        "crossbar.accounts",
        "crossbar",
        "callflow",
        "braintree",
        "blip",
        "bli",
        "blackhole",
        "accounts",
        "8b23cfb3383612cda8383047c20001a7"
    ],
    "next_start_key": "notification.port_scheduled",
    "page_size": 50,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Create a new config

> PUT /v2/system_configs

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"id": "candle_jack", "name": ""}}' \
    http://{SERVER}:8000/v2/system_configs
```

##### Successful creation

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "name": ""
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

##### Error: old soft-deleted document still exists

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "message": "conflicting documents"
    },
    "error": "409",
    "message": "datastore_conflict",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```


#### Delete the whole config

> DELETE /v2/system_configs/{SYSTEM_CONFIG_ID}

Note: use `?hard=true` to permanently delete the document.

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/bli
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Get default config

> GET /v2/system_configs/{SYSTEM_CONFIG_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/blip
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "default": {
            "blop": ""
            "k": [
                "value1"
            ]
        },
        "kazoo_apps@termina.tor": {
            "T": 42
        },
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Update default config

> POST /v2/system_configs/{SYSTEM_CONFIG_ID}

Note: returns previous default fields.

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"key": "my string", "blop": null}}' \
    http://{SERVER}:8000/v2/system_configs/blip
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "k": [
            "value1"
        ],
        "key": "my string"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Delete node specific config

> DELETE /v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/blip/kazoo_apps@termina.tor
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Get node-specific config

> GET /v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/system_configs/blip/kazoo_apps@termina.tor
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "T": 42
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Update node-specific config

> POST /v2/system_configs/{SYSTEM_CONFIG_ID}/{NODE}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"T": 42}}' \
    http://{SERVER}:8000/v2/system_configs/blip/kazoo_apps@termina.tor
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "T": 42
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
