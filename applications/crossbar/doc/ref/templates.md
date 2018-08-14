# Templates

## About Templates

## Schema



## Fetch

> GET /v2/templates

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/templates
```

## Create

> PUT /v2/templates/{TEMPLATE_NAME}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/templates/{TEMPLATE_NAME}
```

## Remove

> DELETE /v2/templates/{TEMPLATE_NAME}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/templates/{TEMPLATE_NAME}
```

