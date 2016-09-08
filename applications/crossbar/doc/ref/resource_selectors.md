### Resource_selectors

#### About Resource_selectors

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`selectors` | Data used for selectors | `array()` |   | `true`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/resource_selectors

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/name

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/name
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}/resource/{RESOURCE_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}/resource/{RESOURCE_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}/resource/{RESOURCE_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}/resource/{RESOURCE_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}/resource/{RESOURCE_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}/resource/{RESOURCE_ID}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}/resource/{RESOURCE_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/name/{SELECTOR_NAME}/resource/{RESOURCE_ID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/name/{SELECTOR_NAME}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/name/{SELECTOR_NAME}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/name/{SELECTOR_NAME}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/name/{SELECTOR_NAME}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/name/{SELECTOR_NAME}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/name/{SELECTOR_NAME}
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/name/{SELECTOR_NAME}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/resource_selectors/resource/{RESOURCE_ID}/name/{SELECTOR_NAME}
```

