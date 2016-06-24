### Resource_selectors

#### About Resource_selectors

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`selectors` | Data used for selectors | `array()` |   | `true`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/resource_selectors

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/selector

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/selector
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/selector/{SELECTORNAME}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/selector/{SELECTORNAME}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/selector/{SELECTORNAME}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/selector/{SELECTORNAME}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/selector/{SELECTORNAME}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/selector/{SELECTORNAME}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/selector/{SELECTORNAME}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/selector/{SELECTORNAME}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/selector/{SELECTORNAME}

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/selector/{SELECTORNAME}
```

