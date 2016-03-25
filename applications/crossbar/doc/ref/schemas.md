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

> GET /v2/schemas/{SCHEMADOC}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/schemas/{SCHEMADOC}
```

#### Create

> PUT /v2/schemas/{SCHEMADOC}/validation

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/schemas/{SCHEMADOC}/validation
```

