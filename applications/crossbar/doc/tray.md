# Tray

## About Tray

Handle CRUD operations for Tray.io solutions

## Configuring Tray

In order to communicate with Tray.io there is an initial configuration step required. The configuration document is located at `system_config/crossbar.tray` and the required fields that do not have preset defaults are `master_token` and `partner_name`.

Here is an example of how the configuration document may look:

```json
{
    "_id": "crossbar.tray",
    "default": {
        "app_url": "https://app.tray.io",
        "gql_endpoint": "https://tray.io/graphql",
        "master_token": "19e7458a-f925-4eae-8fe7-9e787b0124de",
        "partner_name": "example"
    },
    "pvt_account_id": "system_config",
    "pvt_account_db": "system_config",
    "pvt_type": "config",
    "pvt_node": "kazoo_apps@fqdn.com"
}
```

## List all available solutions

> GET /v2/tray

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/tray
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "node": {
                "id": "7e52063f-19ef-4c48-be83-6a8acadca72f",
                "title": "Example Solution",
                "description": "Example description for solution.",
                "tags": [
                    "example"
                ],
                "customFields": [
                    {
                        "key": "example",
                        "value": "example"
                    }
                ]
            }
        }
    ],
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

## List all solution instances for account

> GET /v2/accounts/{ACCOUNT_ID}/tray

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tray
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "node": {
                "id": "7d47b403-67b8-4102-a9a4-3e91f0169655",
                "name": "Example Solution Instance",
                "enabled": true,
                "workflows": {
                    "edges": [
                        {
                            "node": {
                                "triggerUrl": "https://8d4298a8-66f5-4251-804e-7d3904d31b03.trayapp.io"
                            }
                        }
                    ]
                },
                "created": "2019-05-05T03:00:00.000Z"
            }
        }
    ],
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

## Create solution instance for account



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`id` | ID of the solution  | `string()` |   | `true` |
`name` | Name of the solution instance | `string()` |   | `true` |



> POST /v2/accounts/{ACCOUNT_ID}/tray

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {
        "id": "7e52063f-19ef-4c48-be83-6a8acadca72f",
        "name": "Example Solution Instance"
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tray
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "popupUrl": "https://app.tray.io/external/solutions/example/configure/7d47b403-67b8-4102-a9a4-3e91f0169655?code=9ba9381555840ebc5e86e6abf5ee21aad7eaf7a8"
    },
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

## Create one-time popup URL to allow editing solution instance for account

> GET /v2/accounts/{ACCOUNT_ID}/tray/{SOLUTION_INSTANCE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tray/{SOLUTION_INSTANCE_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "popupUrl": "https://app.tray.io/external/solutions/example/configure/7d47b403-67b8-4102-a9a4-3e91f0169655?code=06ad0d19d5f9afa52d1f5e317909d04d0103b130"
    },
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

## Update enabled state of solution instance for account



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`enabled` | Enabled state of the solution instance | `boolean()` |   | `true` |



> PATCH /v2/accounts/{ACCOUNT_ID}/tray/{SOLUTION_INSTANCE_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {
        "enabled": false
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tray/{SOLUTION_INSTANCE_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

## Delete solution instance for account

> DELETE /v2/accounts/{ACCOUNT_ID}/tray/{SOLUTION_INSTANCE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tray/{SOLUTION_INSTANCE_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```
