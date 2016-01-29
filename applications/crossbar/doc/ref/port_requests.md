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
`comments` | The history of comments made on a port request | `array` |   | `false`
`name` | A friendly name for the port request | `string` |   | `true`
`notifications` | Status notifications | `object` |   | `false`
`notifications.email` | Email notifications | `object` |   | `false`
`notifications.email.send_to` | A list or string of email recipent(s) | `stringarray` |   | `false`
`numbers` | The numbers to port in | `object` |   | `true`
`port_state` | What state the port request is currently in | `string` | `unconfirmed` | `false`
`scheduled_date` | Requested scheduled date in gregorain timestamp | `integer` |   | `false`
`transfer_date` | Requested transfer date in gregorain timestamp | `integer` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNTID}/port_requests

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/port_requests

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/port_requests/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/port_requests/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/{ID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/port_requests/{ID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/{ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/port_requests/canceled

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/canceled
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/port_requests/rejected

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/rejected
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/port_requests/completed

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/completed
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/port_requests/scheduled

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/scheduled
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/port_requests/pending

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/pending
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/port_requests/submitted

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/submitted
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/port_requests/{ID}/loa

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/{ID}/loa
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/port_requests/{ID}/attachments

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/{ID}/attachments
```

#### Create

> PUT /v2/accounts/{ACCOUNTID}/port_requests/{ID}/attachments

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/{ID}/attachments
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/port_requests/{ID}/canceled

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/{ID}/canceled
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/port_requests/{ID}/rejected

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/{ID}/rejected
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/port_requests/{ID}/completed

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/{ID}/completed
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/port_requests/{ID}/scheduled

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/{ID}/scheduled
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/port_requests/{ID}/pending

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/{ID}/pending
```

#### Patch

> PATCH /v2/accounts/{ACCOUNTID}/port_requests/{ID}/submitted

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/{ID}/submitted
```

#### Remove

> DELETE /v2/accounts/{ACCOUNTID}/port_requests/{ID}/attachments/{ATTACHMENTID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/{ID}/attachments/{ATTACHMENTID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNTID}/port_requests/{ID}/attachments/{ATTACHMENTID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/{ID}/attachments/{ATTACHMENTID}
```

#### Change

> POST /v2/accounts/{ACCOUNTID}/port_requests/{ID}/attachments/{ATTACHMENTID}

```curl
curl -v http://{SERVER}:8000//v2/accounts/{ACCOUNTID}/port_requests/{ID}/attachments/{ATTACHMENTID}
```

