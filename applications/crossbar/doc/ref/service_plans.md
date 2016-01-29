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

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/service_plans

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/service_plans/{ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/service_plans/{ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/service_plans/{ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/service_plans/override

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/override
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/service_plans/current

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/current
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/service_plans/reconciliation

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/reconciliation
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/service_plans/synchronization

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/synchronization
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/service_plans/available/{ID}

```curl
curl -v http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/service_plans/available/{ID}
```

