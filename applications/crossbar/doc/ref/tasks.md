### Tasks

#### About Tasks

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`task` | A task to be executed in the background | `object` |   | `true`
`task.action` | The action to be executed | `string` |   | `true`
`task.category` | Class of the task | `string` |   | `true`


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

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/csv

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/csv
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/csv/{ATTACHMENT_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/csv/{ATTACHMENT_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/csv/{ATTACHMENT_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/csv/{ATTACHMENT_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/csv/{ATTACHMENT_ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/csv/{ATTACHMENT_ID}
```

