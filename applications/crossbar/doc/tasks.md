### Backgroud Jobs

Kazoo Tasks enables listing, adding, starting & removing generic background tasks.


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
    "request_id": "71f237d7ccce7a2418c8e026f18289aa",
    "revision": "undefined",
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
            "action": "add",
            "category": "number_management",
            "created": 63632526992,
            "id": "e5c92c4b50bcec520d5d7e1ce1b869",
            "status": "pending",
            "total_count": 1
        },
        {
            "account_id": "{ACCOUNT_ID}",
            "action": "add",
            "category": "number_management",
            "created": 63632526924,
            "id": "7c17c051d6553f0329d9f8c47b253c",
            "node": "whistle_apps@qwd",
            "start_timestamp": 63632526968,
            "status": "success",
            "success_count": 1,
            "total_count": 1
        }
    ],
    "page_size": 2,
    "request_id": "9fcff776cd4eb3d7d92389d9209db2d5",
    "revision": "601ffbf34fcbbc56597acf79847efde7",
    "status": "success"
}
```


#### Add a new task

> PUT /v2/accounts/{ACCOUNT_ID}/tasks

With CSV data:

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: text/csv" \
    --data-binary @path/to/your/file.csv \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks?category={CATEGORY}&action={ACTION}
```

Or with JSON data:

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"records": [{RECORDS}]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks?category={CATEGORY}&action={ACTION}
```

##### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "_read_only": {
            "account_id": "{ACCOUNT_ID}",
            "action": "{ACTION}",
            "category": "{CATEGORY}",
            "id": "edfb48ea9617fa6832e43ce676c53f",
            "submit_timestamp": 63632025993
            "total_count": {RECORDS_COUNT}
        }
    },
    "request_id": "6bc9187feafe54a5c16d07e1a493c04f",
    "revision": "undefined",
    "status": "success"
}
```

##### Failure: no task providers known yet

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "cause": "{CATEGORY}",
        "message": "bad identifier",
        "tip": "No APIs known yet: please try again in a second."
    },
    "error": "404",
    "message": "bad_identifier",
    "request_id": "0e77686d7dfc6ecb1b8c78b745c2ed92",
    "status": "error"
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
    "request_id": "86218c6dcd58505f1dc6e036b08cd151",
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
    "request_id": "151dc80b630e6cd1f50585dcd6c81268",
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
    "request_id": "f319f2a4fd112c91ba536dfa39fd50ff",
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
    "request_id": "296a70611e460b82628cb873c11e5c98",
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
    "request_id": "216e9b80decc66a706320a9bd11da544",
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
    "request_id": "5d2a8cd40b3e9242eeeb456cc76b5ad9",
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
    "request_id": "de3900aff99402497280f11928c81ea6",
    "revision": "undefined",
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
    "request_id": "8a495e9ed03f75e414e19c8923ece5f5",
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
    "request_id": "c5488723679244682ddcfd06fa7d4fcd",
    "status": "error"
}
```


#### Get a specific task's details

Optional: use `-H "Content-Type: text/csv"` to fetch the task's data file as a CSV.

> GET /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}

To fetch its CSV data:

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Accept: text/csv" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}
```

Streams back the contents of the CSV file.


To fetch a task's summary:

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
            "action": "add",
            "category": "number_management",
            "id": "{TASK_ID}",
            "status": "pending",
            "submit_timestamp": 63632220951
            "total_count": 2
        }
    },
    "request_id": "8341d44579d03e7aa2e97e1cc0c5123f",
    "revision": "undefined",
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
    "request_id": "d3f7ba1b65348a4257fc1a59e99b6203",
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
    "request_id": "c05d845800c67d1cd5c01e5276155436",
    "revision": "undefined",
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
    "request_id": "d85d7a75fba07d672c58ef2a984619f9",
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
    "request_id": "f1a27978cada1d8cc41bd2473edd3ade",
    "status": "error"
}
```


#### Retrieve an errors listing

> GET /v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/errors

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/{TASK_ID}/errors
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": "{ERROR_DATA}",
    "request_id": "ea84cd0a07faad793256a52084e63996",
    "revision": "7-1af9813cd3da385e5a018af885bec70b",
    "status": "success"
}
```
