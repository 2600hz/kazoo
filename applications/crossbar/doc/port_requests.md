### Port Requests

Manage and track number port requests through the Port Requests API.

A port request can be in one of five **states**:

* `unconfirmed`: A port request has been created, but the details have not been confirmed and the port process has not started.
* `submitted`: Indicates the number port is ready to be processed and sent to the losing carrier.
* `scheduled`: The port is in progress and the losing carrier has been notified.
* `completed`: The port request has been finished, and numbers are activated.
* `rejected`: The port request has been cancelled, or something has gone wrong during the port process. The port can be resubmitted.


#### List port requests

> GET /v2/accounts/{ACCOUNT_ID}/port_requests

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests
```


#### Listing by port state

You can issue GET requests to find all ports in a particular state too.

All requests are not paginated, with the exception of the `completed` state.
Use pagination toggles for date range as desired.

##### Listing by `pending` port

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/pending

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/pending
```

##### Listing by `submitted` port

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/submitted

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/submitted
```

##### Listing by `pending` port

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/pending

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/pending
```

##### Listing by `scheduled` port

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/scheduled

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/scheduled
```

##### Listing by `completed` port

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/completed

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/completed
```

##### Listing by `rejected` port

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/rejected

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/rejected
```

##### Listing by `canceled` port

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/canceled

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/canceled
```


#### List port requests of self and sub accounts

    curl -v -X GET \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/descendants/port_requests


#### Create a new port request

> PUT /v2/accounts/{ACCOUNT_ID}/port_requests

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"numbers":{"+12025559000":{}}, "name":"Porting 202.555.9000"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests
```


#### List port request details

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}
```


#### Edit a port request

> POST /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"numbers":{"+12025559000":{"state":"NY"}}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}
```


#### DELETE a port request

> DELETE /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}
```


#### List attachments on a port request

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments
```


#### Add an attachment to a port request

> PUT /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/pdf" \
    --data-binary @/path/to/file.pdf \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments?filename=file.pdf'
```


#### Get an attachment from a port request

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Accept: application/pdf" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID} > file.pdf
```


#### Replace an attachment on a port request

> POST /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/pdf" \
    --data-binary @/path/to/file.pdf \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}
```


#### Delete an attachment on a port request

> DELETE /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}
```


#### Indicate a port is ready to be processed

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/submitted

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/submitted
```


#### Put port in pending

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/pending

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/pending
```


#### Put port in progress (sent to losing carrier)

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/scheduled

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/scheduled
```


#### Complete port, numbers will activate in the system, account will be billed

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/completed

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/completed
```


#### Reject a port

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/rejected

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/rejected
```


#### Cancel a port

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/canceled

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/canceled
```


#### Build an LOA PDF from a port request

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/loa

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Accept: application/x-pdf" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/loa
```


#### Get a port request by phone number

- Verb: `GET`
- Url: `/accounts/{{ACCOUNT_ID}}/port_requests?by_number={{NUMBER}}`
- Payload: None

##### Response

    {"page_size": 1,
     "data": [{
        "carrier": "PACIFIC BELL",
        "bill": {
            "name": "John Doe",
            "address": "116, natoma street",
            "locality": "San Francisco",
            "region": "Ca",
            "postal_code": "95109"
        },
        "name": "Port request test",
        "notifications": {
            "email": {
                "send_to": "someone@2600hz.com"
            }
        },
        "transfer_date": 63598114800,
        "port_state": "submitted",
        "numbers": {
            "{{NUMBER}}": {}
        },
        "sent": false,
        "uploads": {
            "loa.pdf": {
                "content_type": "application/pdf",
                "length": 59196
            },
            "bill.pdf": {
                "content_type": "application/pdf",
                "length": 8304
            }
        },
        "updated": 63597642011,
        "created": 63597642009,
        "id": "84e0a824c6b74fe1e3ec48962a600ef2"
     }],
     "status": "success"
    }


#### Get port request for account and descendants

- Verb: `GET`
- Url: `/accounts/{{ACCOUNT_ID}}/descendants/port_requests?by_number={{NUMBER}}`
- Payload: None

##### Response

    {"page_size": 1,
     "data": [{
        "carrier": "PACIFIC BELL",
        "bill": {
            "name": "John Doe",
            "address": "116, natoma street",
            "locality": "San Francisco",
            "region": "Ca",
            "postal_code": "95109"
        },
        "name": "Port request test",
        "notifications": {
            "email": {
                "send_to": "someone@2600hz.com"
            }
        },
        "transfer_date": 63598114800,
        "port_state": "submitted",
        "numbers": {
            "{{NUMBER}}": {}
        },
        "sent": false,
        "uploads": {
            "loa.pdf": {
                "content_type": "application/pdf",
                "length": 59196
            },
            "bill.pdf": {
                "content_type": "application/pdf",
                "length": 8304
            }
        },
        "updated": 63597642011,
        "created": 63597642009,
        "id": "84e0a824c6b74fe1e3ec48962a600ef2"
     }],
     "status": "success"
    }
