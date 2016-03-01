### Faxboxes

#### About Faxboxes

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxboxes

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/faxboxes

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/faxboxes/{BOX_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes/{BOX_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxboxes/{BOX_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes/{BOX_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/faxboxes/{BOX_ID}

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes/{BOX_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/faxboxes/{BOX_ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes/{BOX_ID}
```

