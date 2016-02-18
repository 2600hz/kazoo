### Schemas

#### About Schemas

#### Schema



#### Fetch

> GET /v2/schemas

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/schemas
```

#### Fetch

> GET /v2/schemas/{ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/schemas/{ID}
```

#### Create

> PUT /v2/schemas/{ID}/validation

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/schemas/{ID}/validation
```

