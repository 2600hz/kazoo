# Port Requests

## About Port Requests

#### Schema

Schema for a port request



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`bill.account_number` | Account Number to identify account | `string()` |   | `false` |  
`bill.btn` | Billing Telephone Number (BTN) to identify account | `string()` |   | `false` |  
`bill.carrier` | The name of the losing carrier | `string()` |   | `false` |  
`bill.locality` | The locality (city) of the billing address the losing carrier has on record | `string()` |   | `false` |  
`bill.name` | The losing carrier billing/account name | `string()` |   | `false` |  
`bill.pin` | Personal Identification Number (PIN) to identify account | `string()` |   | `false` |  
`bill.postal_code` | The zip/postal code of the billing address the losing carrier has on record | `string()` |   | `false` |  
`bill.region` | The region (state) of the billing address the losing carrier has on record | `string()` |   | `false` |  
`bill.street_address` | The street name of the billing address the losing carrier has on record | `string()` |   | `false` |  
`bill.street_number` | The street number of the billing address the losing carrier has on record | `string()` |   | `false` |  
`bill.street_post_dir` | Street Post-Direction | `string('E' | 'N' | 'NE' | 'NW' | 'S' | 'SE' | 'SW' | 'W')` |   | `false` |  
`bill.street_pre_dir` | Street Pre-Direction | `string('E' | 'N' | 'NE' | 'NW' | 'S' | 'SE' | 'SW' | 'W')` |   | `false` |  
`bill.street_type` | The street type of the billing address the losing carrier has on record | `string()` |   | `false` |  
`bill` | Billing information of the losing carrier | `object()` |   | `false` |  
`comments` | The history of comments made on a port request | `["array(", "[#/definitions/comment](#comment)", ")"]` |   | `false` |  
`name` | A friendly name for the port request | `string(1..128)` |   | `true` |  
`notifications.email.send_to` | A list or string of email recipient(s) | `string() | array(string())` |   | `false` |  
`notifications.email` | Inbound Email Notifications | `object()` |   | `false` |  
`notifications` | Status notifications | `object()` |   | `false` |  
`numbers./\+?[0-9]+/` |   | `object()` |   | `false` |  
`numbers` | The numbers to port in | `object()` |   | `true` |  
`reference_number` | Winning carrier reference number or order ID | `string()` |   | `false` |  
`signee_name` | The name of the person authorizing the release of numbers from the losing carrier | `string()` |   | `false` |  
`signing_date` | The date in Gregorian timestamp on which the document releasing the numbers from the losing carrier was signed | `integer()` |   | `false` |  
`transfer_date` | Requested transfer date in Gregorian timestamp | `integer()` |   | `false` |  
`winning_carrier` | The name of winning carrier | `string()` |   | `false` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/port_requests

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/last_submitted

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/last_submitted
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/timeline

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/timeline
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/loa

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/loa
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/canceled

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/canceled
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/rejected

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/rejected
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/completed

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/completed
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/scheduled

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/scheduled
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/pending

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/pending
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/submitted

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/submitted
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments/{ATTACHMENT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments/{ATTACHMENT_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments/{ATTACHMENT_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments/{ATTACHMENT_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments/{ATTACHMENT_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments/{ATTACHMENT_ID}
```

