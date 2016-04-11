### Ips

#### About Ips

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`ips` | List of IP addresses | `array(string)` |   | `false`
`ips.[]` |   | `string` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/ips

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/ips

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/ips/{IP}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips/{IP}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/ips/{IP}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips/{IP}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/ips/{IP}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips/{IP}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/ips/hosts

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips/hosts
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/ips/zones

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips/zones
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/ips/assigned

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/ips/assigned
```

