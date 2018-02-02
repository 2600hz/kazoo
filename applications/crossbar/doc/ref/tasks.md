# Tasks

## About Tasks

#### Schema

Input data to go through as part of a background task



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`file_name` | Human-readable name of a task's input file | `string()` |   | `false` |  
`records` | List the rows of input data | `array(object())` |   | `false` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/tasks

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/tasks

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/output

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/output
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/input

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/input
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/stop

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/stop
```

