### Agents

#### About Agents

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/{ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/stats

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/stats
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/status

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/status
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/{ID}/queue_status

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{ID}/queue_status
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/agents/{ID}/queue_status

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{ID}/queue_status
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/{ID}/status

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{ID}/status
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/agents/{ID}/status

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{ID}/status
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/status/{ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/status/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/agents/status/{ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/status/{ID}
```

