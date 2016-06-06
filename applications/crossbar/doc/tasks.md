### Backgroud Jobs

Kazoo Tasks enables listing, adding, starting & removing generic background tasks.


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
            "action": "list",
            "category": "number_management",
            "id": "65a3276a4e3ecc5890f56eb3e6fc6a",
            "status": "pending"
            "submit_timestamp": 63632026121
        }
    ],
    "page_size": 1,
    "request_id": "4e94e914e7edb5bfbc74f6178b9cbbc3",
    "revision": "815ddcbe3afb6a4a14dfb2d19462bdfc",
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
        "tip": "No APIs known yet: GET /help then try again!"
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
                "message": "Empty CSV or some row(s) longer than others"
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

##### Missing values for mandatory fields

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "attachment": {
            "type": {
                "missing_mandatory_values": [
                    ",4159876543,"
                ]
            }
        }
    },
    "error": "500",
    "message": "invalid request",
    "request_id": "dd1b7c575f4dddd81f94827f419ddb55",
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
            "action": "list",
            "category": "number_management",
            "id": "{TASK_ID}",
            "status": "pending",
            "submit_timestamp": 63632222244
        }
    },
    "request_id": "00b7c5d89fc334b477626cd7bd91a85e",
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
            "action": "list",
            "category": "number_management",
            "id": "{TASK_ID}",
            "status": "pending",
            "submit_timestamp": 63632220951
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
            "action": "list",
            "category": "number_management",
            "id": "{TASK_ID}",
            "node": "whistle_apps@qwd",
            "start_timestamp": 63632222842,
            "status": "executing",
            "submit_timestamp": 63632222815
        }
    },
    "request_id": "c8df9abfd8fec56b50ddfe8a1796e7dc",
    "revision": "undefined",
    "status": "success"
}
```

##### Task already running

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


#### List available tasks

> GET /v2/accounts/{ACCOUNT_ID}/tasks/help

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks/help
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

