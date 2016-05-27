### Tasks

#### About Tasks

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`arguments` | Ordered list of arguments for the function | `array(string)` |   | `true`
`arguments.[]` |   | `string` |   | `false`
`function` | Function name of the module | `string(1..64)` |   | `true`
`module` | Erlang module of the task | `string(1..64)` |   | `true`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/tasks

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/tasks

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/tasks/help

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/help
```

