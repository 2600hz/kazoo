### Auth

#### About Auth

#### Schema



#### Fetch

> GET /v2/auth/links

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links
```

#### Change

> POST /v2/auth/links

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links
```

#### Remove

> DELETE /v2/auth/links

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/links
```

#### Create

> PUT /v2/auth/callback

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/callback
```

#### Create

> PUT /v2/auth/authorize

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/authorize
```

#### Fetch

> GET /v2/auth/tokeninfo

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/auth/tokeninfo
```

