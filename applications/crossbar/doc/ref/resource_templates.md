# Resource Templates

## About Resource Templates

## Schema



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_templates

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_templates
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/resource_templates

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_templates
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_templates/{RESOURCE_TEMPLATE_ID}
```

