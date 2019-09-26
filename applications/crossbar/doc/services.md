# Services

## About Services

Fetch and inspect account's service.

## Schema




## Get Account Service

> GET /v2/accounts/{ACCOUNT_ID}/services

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services
```

## Update the Account Billing ID

> POST /v2/accounts/{ACCOUNT_ID}/services

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {
        "billing_id":"{BILLING_ID}"
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services
```

## Get Account Service Status

> GET /v2/accounts/{ACCOUNT_ID}/services/status

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/status
```

#### Fetch Service Audit Logs

> GET /v2/accounts/{ACCOUNT_ID}/services/audit

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/audit
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services/audit/{AUDIT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/audit/{AUDIT_ID}
```

## Get service changes summary per day

Using this API you can a list of services changes (additions/removal/usage) summary per day.

> GET /v2/accounts/{ACCOUNT_ID}/services/audit/summary

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/audit/summary
```

### Example

**Request:**


```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/audit/summary?created_from=63742805754&created_to=63743140658
```

**Response:**

```json
{
  "data": [
    {
      "account": {
        "2019-12-05": {
          "addition": 1,
          "last_timestamp": 63742805754,
          "quantity": 2,
          "removal": 0,
          "sum_quantity": false
        },
        "2019-12-09": {
          "addition": 1,
          "last_timestamp": 63743140658,
          "quantity": 2,
          "removal": 1,
          "sum_quantity": false
        }
      },
      "mailbox": {
        "2019-12-09": {
          "addition": 3,
          "last_timestamp": 63743140695,
          "quantity": 3,
          "removal": 0,
          "sum_quantity": false
        }
      },
      "softphone": {
        "2019-12-09": {
          "addition": 1,
          "last_timestamp": 63743140580,
          "quantity": 3,
          "removal": 0,
          "sum_quantity": false
        }
      },
      "user": {
        "2019-12-09": {
          "addition": 1,
          "last_timestamp": 63743140695,
          "quantity": 2,
          "removal": 0,
          "sum_quantity": false
        }
      }
    }
  ],
  "revision": "{REVISION}",
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Get changes summary per day for a single service category

Using this API you can a list of changes (additions/removal/usage) summary per day for a single service category.

> GET /v2/accounts/{ACCOUNT_ID}/services/audit_summary/{SOURCE_SERVICE}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/audit/summary/{SOURCE_SERVICE}
```

### Example

**Request:**


```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/audit/summary/user?created_from=63742805754&created_to=63743140658
```

**Response:**

```json
{
  "data": [
    {
      "2019-12-09": {
        "addition": 1,
        "last_timestamp": 63743140658,
        "quantity": 2,
        "removal": 1,
        "sum_quantity": false
      }
    },
    {
      "2019-12-05": {
        "addition": 1,
        "last_timestamp": 63742805754,
        "quantity": 2,
        "removal": 0,
        "sum_quantity": false
      }
    }
  ],
  "revision": "{REVISION}",
  "timestamp": "{TIMESTAMP}",
  "version": "{VERSION}",
  "node": "{NODE_HASH}",
  "request_id": "{REQUEST_ID}",
  "status": "success",
  "auth_token": "{AUTH_TOKEN}"
}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/services/{PLAN_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/{PLAN_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/services/{PLAN_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/{PLAN_ID}
```


## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services/manual

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/manual
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/services/manual

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/manual
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/services/manual

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/manual
```


## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services/overrides

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/overrides
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/services/overrides

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/overrides
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/services/overrides

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/overrides
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/services/quote

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/quote
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/services/reconciliation

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/reconciliation
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/services/synchronization

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/synchronization
```


## Change

> POST /v2/accounts/{ACCOUNT_ID}/services/topup

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/topup
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services/summary

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/summary
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services/available

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/available
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/services/editable

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/services/editable
```
