# Comments

## About Comments

Allows you to add comments to "any" documents in Kazoo.

## Fetch

> DELETE /v2/accounts/{ACCOUNT_ID}/comments

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/comments
```

```json
{
    "data": [
    ],
    "status": "success"
}
```


## Fetch a Comment

> GET /v2/accounts/{ACCOUNT_ID}/comments

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/comments
```

```json
{
  "data": "{COMMENT}",
  "status": "success"
}
```


## Add a Comment

> PUT /v2/accounts/{ACCOUNT_ID}/comments

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"comments": [{COMMENT_3}]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/comments
```

```json
{
    "data": {
        "comments": [
            "{COMMENT_1}",
            "{COMMENT_2}",
            "{COMMENT_3}"
        ]
    },
    "status": "success"
}
```


## Delete a Comment

> DELETE /v2/accounts/{ACCOUNT_ID}/comments/{COMMENT_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/comments/{COMMENT_ID}
```

```json
{
    "data": {
        "comments": [
            "{COMMENT_1}",
            "{COMMENT_2}"
        ]
    },
    "status": "success"
}
```


## Fetch a Comment

> GET /v2/accounts/{ACCOUNT_ID}/comments/{COMMENT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/comments/{COMMENT_ID}
```

```json
{
    "data": {
        "comments": [
            "{COMMENT_1}",
            "{COMMENT_2}"
        ]
    },
    "status": "success"
}
```


## Update a Comment

> POST /v2/accounts/{ACCOUNT_ID}/comments/{COMMENT_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": "{COMMENT}"}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/comments/{COMMENT_ID}
```

```json
{
    "data": "{COMMENT}",
    "status": "success"
}
```
