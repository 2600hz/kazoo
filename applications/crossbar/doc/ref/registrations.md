# Registrations

## About Registrations

## Schema



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/registrations

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/registrations
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/registrations

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/registrations
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/registrations/{USERNAME}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/registrations/{USERNAME}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/registrations/count

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/registrations/count
```

