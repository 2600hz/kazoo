# Faxboxes

## About Faxboxes

## Schema



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxboxes

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/faxboxes

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/faxboxes/{FAXBOX_ID}
```

