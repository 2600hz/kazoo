### Storage

#### About Storage

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`attachments` |   | `#/definitions/storage.attachments` |   | `false`
`connections` |   | `#/definitions/storage.connections` |   | `false`
`plan` |   | `#/definitions/storage.plan` |   | `false`


#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/storage

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/storage
```

