### Token_auth

#### About Token_auth

#### Schema



#### Remove

> DELETE /v2/shared_auth

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/shared_auth
```

#### Fetch

> GET /v2/shared_auth

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/shared_auth
```

