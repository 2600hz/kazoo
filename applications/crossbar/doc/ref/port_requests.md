### Port_requests

#### About Port_requests

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`bill` | Billing information of the losing carrier | `object` |   | `false`
`bill.extended_address` | The suite/floor/apt of the billing address the losing carrier has on record | `string` |   | `false`
`bill.locality` | The locality (city) of the billing address the losing carrier has on record | `string` |   | `false`
`bill.name` | The losing carrier billing/account name | `string` |   | `false`
`bill.postal_code` | The zip/postal code of the billing address the losing carrier has on record | `string` |   | `false`
`bill.region` | The region (state) of the billing address the losing carrier has on record | `string` |   | `false`
`bill.street_address` | The address of the billing address the losing carrier has on record | `string` |   | `false`
`comments` | The history of comments made on a port request | `array(object)` |   | `false`
`name` | A friendly name for the port request | `string(1..128)` |   | `true`
`notifications` | Status notifications | `object` |   | `false`
`notifications.email` | Email notifications | `object` |   | `false`
`notifications.email.send_to` | A list or string of email recipent(s) | `string, array(string)` |   | `false`
`numbers` | The numbers to port in | `object` |   | `true`
`numbers.\+?[0-9]+` |   | `object` |   | `false`
`port_state` | What state the port request is currently in | `string('unconfirmed', 'pending', 'submitted', 'scheduled', 'completed', 'rejected', 'canceled')` | `unconfirmed` | `false`
`scheduled_date` | Requested scheduled date in gregorain timestamp | `integer` |   | `false`
`transfer_date` | Requested transfer date in gregorain timestamp | `integer` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/port_requests

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/canceled

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/canceled
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/rejected

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/rejected
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/completed

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/completed
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/scheduled

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/scheduled
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/pending

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/pending
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/submitted

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/submitted
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/loa

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/loa
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/canceled

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/canceled
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/rejected

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/rejected
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/completed

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/completed
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/scheduled

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/scheduled
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/pending

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/pending
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/submitted

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/submitted
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}
```

