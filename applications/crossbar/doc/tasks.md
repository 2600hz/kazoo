### Backgroud Jobs

Kazoo Tasks enables listing, adding, starting & removing general background tasks.


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
            "arguments": [
                100000
            ],
            "function": "sleep",
            "module": "timer",
            "ended_at": 63631324961,
            "id": "ab1d2f34b79973a076079b942519dc",
            "is_success": true,
            "is_terminated": true,
            "ran_for": 100,
            "started_at": 63631324861,
            "submitted_at": 63631324811
        },
        {
            "account_id": "{ACCOUNT_ID}",
            "arguments": [
                "4157125234"
            ],
            "function": "normalize",
            "module": "knm_converters",
            "id": "7318ebc8f4c4bf40e741485f6a8679",
            "submitted_at": 63631324841
        }
    ],
    "request_id": "de1856ce11c0cf4ae04f206fcf6875ed",
    "revision": "undefined",
    "status": "success"
}
```


#### Add a new task

> PUT /v2/accounts/{ACCOUNT_ID}/tasks

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"module": "{MODULE}", "function": "{FUNCTION}", "arguments": [{ARGUMENTS}]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tasks
```

##### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "account_id": "{ACCOUNT_ID}",
        "arguments": [
            {ARGUMENTS}
        ],
        "function": "{FUNCTION}",
        "id": "754b9b2fec86eaed255e2c5f58df95",
        "module": "{MODULE}",
        "submitted_at": 63631506599
    },
    "request_id": "30d6cd3153859cc57561437dd61abbb5",
    "revision": "undefined",
    "status": "success"
}
```

##### Unknown module

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        "{MODULE}"
    ],
    "error": "404",
    "message": "bad identifier",
    "request_id": "69db4fd5ed24ac7ed5853197946a287a",
    "status": "error"
}
```

##### Function does not exist

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "M:F/A": "{MODULE}:{FUNCTION}/{ARGUMENTS_COUNT}",
        "cause": "no such function",
        "message": "bad identifier"
    },
    "error": "404",
    "message": "bad_identifier",
    "request_id": "5176170578fc954d7c113d2544e1687b",
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
        "account_id": "{ACCOUNT_ID}",
        "arguments": [],
        "function": "get_cookie",
        "module": "erlang",
        "id": "{TASK_ID}",
        "submitted_at": 63631325071
    },
    "request_id": "236f3588d30bbfe2511e30df5f3d8830",
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
        "account_id": "{ACCOUNT_ID}",
        "arguments": [
            "bla"
        ],
        "function": "sleep",
        "module": "timer",
        "ended_at": 63631325127,
        "error": "(<0.29082.4>) error: timeout_value\n[{timer,sleep,1,[{file,\"timer.erl\"},{line,153}]},\n {kz_task_worker,run_task,4,[{file,\"src/kz_task_worker.erl\"},{line,161}]}]",
        "id": "{TASK_ID}",
        "is_success": false,
        "is_terminated": true,
        "ran_for": 0,
        "started_at": 63631325127,
        "submitted_at": 63631325095
    },
    "request_id": "5468bd36454888e76266ef132730c73f",
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
        "account_id": "{ACCOUNT_ID}",
        "arguments": [
            100000
        ],
        "function": "sleep",
        "module": "timer",
        "id": "{TASK_ID}",
        "is_terminated": false,
        "started_at": 63631324861,
        "submitted_at": 63631324811
    },
    "request_id": "5b04f981863b1f5f4c702a9a540073f5",
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
