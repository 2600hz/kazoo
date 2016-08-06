### Service_plans

#### About Service_plans

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`bookkeepers` |   | `object` |   | `false`
`description` | Describes the service plan offering | `string` |   | `false`
`name` | A friendly name for the service plan | `string(1..128)` |   | `true`
`plan` | Outlines the service plan for various services | `object` |   | `true`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/service_plans

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/service_plans

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/service_plans/{ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/service_plans/{ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/service_plans/{ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/service_plans/override

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/override
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/service_plans/current

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/current
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/service_plans/reconciliation

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/reconciliation
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/service_plans/synchronization

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/synchronization
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/service_plans/available/{ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/available/{ID}
```

