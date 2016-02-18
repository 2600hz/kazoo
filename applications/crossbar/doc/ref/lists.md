### Lists

#### About Lists

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`description` | A friendly list description | `string(1..128)` |   | `false`
`name` | A friendly match list name | `string(1..128)` |   | `true`
`org` | Full legal name of the organization | `string` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/lists

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/lists

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST_ID}/entries
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/lists/{LIST}/entries/{ENTRY_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST}/entries/{ENTRY_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/lists/{LIST}/entries/{ENTRY_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST}/entries/{ENTRY_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/lists/{LIST}/entries/{ENTRY_ID}

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST}/entries/{ENTRY_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/lists/{LIST}/entries/{ENTRY_ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST}/entries/{ENTRY_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/lists/{LIST}/entries/{ENTRY_ID}/photo

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST}/entries/{ENTRY_ID}/photo
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/lists/{LIST}/entries/{ENTRY_ID}/vcard

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/lists/{LIST}/entries/{ENTRY_ID}/vcard
```

