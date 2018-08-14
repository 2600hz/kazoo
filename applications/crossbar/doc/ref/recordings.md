# Recordings

## About Recordings

## Schema



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/recordings

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/recordings
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/recordings/{RECORDING_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/recordings/{RECORDING_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/recordings/{RECORDING_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/recordings/{RECORDING_ID}
```

