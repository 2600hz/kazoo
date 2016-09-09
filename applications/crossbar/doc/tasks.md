### Backgroud Jobs

Kazoo Tasks enables listing, adding, starting & removing generic background tasks.

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`records` | List the rows of input data | `array(object)` |   | `false`



#### List available tasks

> GET /v2/tasks

```shell
curl -v -X GET \
    http://{SERVER}:8000/v2/tasks
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "tasks": {
            "number_management": {
                "add": {
                    "description": "Bulk-create numbers",
                    "expected_content": "text/csv",
                    "mandatory": [
                        "number",
                        "account_id"
                    ],
                    "optional": [
                        "auth_by",
                        "module_name"
                    ]
                },
                "assign_to": {
                    "description": "Bulk-assign numbers to the provided account",
                    "expected_content": "text/csv",
                    "mandatory": [
                        "number",
                        "account_id"
                    ],
                    "optional": [
                        "auth_by"
                    ]
                }
            }
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### List all tasks

> GET /v2/accounts/{ACCOUNT_ID}/tasks

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "account_id": "{ACCOUNT_ID}",
            "auth_account_id": "{AUTH_ACCOUNT_ID}",
            "action": "add",
            "category": "number_management",
            "created": 63632526992,
            "file_name": "my_input_for_add.csv",
            "id": "e5c92c4b50bcec520d5d7e1ce1b869",
            "status": "pending",
            "total_count": 1
        },
        {
            "account_id": "{ACCOUNT_ID}",
            "auth_account_id": "{AUTH_ACCOUNT_ID}",
            "action": "add",
            "category": "number_management",
            "created": 63632526924,
            "end_timestamp": 63632526969,
            "id": "7c17c051d6553f0329d9f8c47b253c",
            "node": "whistle_apps@qwd",
            "start_timestamp": 63632526968,
            "status": "success",
            "success_count": 1,
            "total_count": 1
        }
    ],
    "page_size": 2,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Add a new task

> PUT /v2/accounts/{ACCOUNT_ID}/tasks

With CSV input data:

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: text/csv" \
    --data-binary @path/to/your/file.csv \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks?category={CATEGORY}&action={ACTION}
```

With JSON input data:

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"records": [{RECORDS}]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks?category={CATEGORY}&action={ACTION}
```

Without input data:

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks?category={CATEGORY}&action={ACTION}
```

##### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "_read_only": {
            "account_id": "{ACCOUNT_ID}",
            "auth_account_id": "{AUTH_ACCOUNT_ID}",
            "action": "{ACTION}",
            "category": "{CATEGORY}",
            "id": "edfb48ea9617fa6832e43ce676c53f",
            "submit_timestamp": 63632025993
            "total_count": {RECORDS_COUNT}
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

##### Unknown category

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        "{CATEGORY}"
    ],
    "error": "404",
    "message": "bad identifier",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```

##### Unknown action

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        "{ACTION}"
    ],
    "error": "404",
    "message": "bad identifier",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```

##### Bad CSV format

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "csv": {
            "format": {
                "message": "Empty CSV or some row(s) longer than others or header missing"
            }
        }
    },
    "error": "500",
    "message": "invalid request",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```

##### Bad input field name

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "attachment": {
            "type": {
                "unknown_fields": [
                    "wef"
                ]
            }
        }
    },
    "error": "500",
    "message": "invalid request",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```

##### Missing mandatory fields

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "attachment": {
            "type": {
                "missing_mandatory_fields": [
                    "number",
                    "account_id"
                ]
            }
        }
    },
    "error": "500",
    "message": "invalid request",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```

##### Rows or records missing values for mandatory fields

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "attachment": {
            "type": {
                "missing_mandatory_values": [
                    ",+14157215234",
                    "009afc511c97b2ae693c6cc4920988e8,"
                ]
            }
        }
    },
    "error": "500",
    "message": "invalid request",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```


#### Remove a completed task

> DELETE /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}
```

##### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "_read_only": {
            "account_id": "{ACCOUNT_ID}",
            "auth_account_id": "{AUTH_ACCOUNT_ID}",
            "action": "add",
            "category": "number_management",
            "end_timestamp": 63632524230,
            "failure_count": 2,
            "id": "{TASK_ID}",
            "node": "whistle_apps@qwd",
            "start_timestamp": 63632524230,
            "status": "failure",
            "submit_timestamp": 63632524207,
            "success_count": 0,
            "total_count": 2
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

##### Task not found

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        "{TASK_ID}"
    ],
    "error": "404",
    "message": "bad identifier",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```

##### Task is running

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "cause": "{TASK_ID}",
        "message": "bad identifier"
    },
    "error": "404",
    "message": "bad_identifier",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```


#### Get a specific task's details

> GET /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}
```

##### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "_read_only": {
            "account_id": "{ACCOUNT_ID}",
            "auth_account_id": "{AUTH_ACCOUNT_ID}",
            "action": "list",
            "category": "number_management",
            "created": 63633924886,
            "failure_count": 0,
            "id": "{TASK_ID}",
            "node": "whistle_apps@qwd",
            "start_timestamp": 63633924909,
            "status": "executing",
            "success_count": 50
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

##### Task does not exist

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        "{TASK_ID}"
    ],
    "error": "404",
    "message": "bad identifier",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```


#### Start a task

> PATCH /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}
```

##### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "_read_only": {
            "account_id": "{ACCOUNT_ID}",
            "auth_account_id": "{AUTH_ACCOUNT_ID}",
            "action": "add",
            "category": "number_management",
            "id": "{TASK_ID}",
            "node": "whistle_apps@qwd",
            "start_timestamp": 63632456149,
            "status": "executing",
            "submit_timestamp": 63632456101
            "total_count": 2
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

##### Task already started

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "cause": "{TASK_ID}",
        "message": "bad identifier",
        "reason": "task already started"
    },
    "error": "404",
    "message": "bad_identifier",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```

##### Task does not exist

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "cause": "{TASK_ID}",
        "message": "bad identifier"
    },
    "error": "404",
    "message": "bad_identifier",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```


#### Retrieve a task's input CSV

> GET /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/input

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/input
```

##### Success

Streams back the task's input in CSV format.

##### Task does not exist or did not have any input data

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "cause": "{TASK_ID}",
        "message": "bad identifier"
    },
    "error": "404",
    "message": "bad_identifier",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```


#### Retrieve a task's output CSV

> GET /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/output

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/output
```

##### Success

Streams back the task's output in CSV format.

##### Task does not exist or output not yet in database

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "cause": "{TASK_ID}",
        "message": "bad identifier"
    },
    "error": "404",
    "message": "bad_identifier",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```
