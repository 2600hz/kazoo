### Token Authentication

#### About Token Authentication

#### Schema



#### Fetch

> GET /v2/token_auth

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/token_auth
```

#### Remove

> DELETE /v2/token_auth

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/token_auth
```

#### Remove

> DELETE /v2/token_auth/refresh_tokens

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/token_auth/refresh_tokens
```

#### Change

> POST /v2/token_auth/refresh

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/token_auth/refresh
```

#### Remove

> DELETE /v2/token_auth/refresh_tokens/{REFRESH_TOKEN}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/token_auth/refresh_tokens/{REFRESH_TOKEN}
```

