### Agents

#### About Agents

#### Schema



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/{ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/stats

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/stats
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/status

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/status
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/{ID}/queue_status

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{ID}/queue_status
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/agents/{ID}/queue_status

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{ID}/queue_status
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/{ID}/status

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{ID}/status
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/agents/{ID}/status

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{ID}/status
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/status/{ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/status/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/agents/status/{ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/status/{ID}
```

