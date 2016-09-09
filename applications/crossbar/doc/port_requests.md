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

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "account_id": "{ACCOUNT_ID}",
            "account_name": "{ACCOUNT_NAME}",
            "port_requests": [
                {
                    "account_id": "{ACCOUNT_ID}",
                    "created": 63630097779,
                    "id": "462da37f8be11e46161fb40bc71173a9",
                    "name": "Porting 202.555.9000",
                    "numbers": {
                        "+12025559000": {}
                    },
                    "port_state": "unconfirmed",
                    "sent": false,
                    "updated": 63630097779,
                    "uploads": {}
                }
            ]
        }
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Get a port request by phone number

> GET /v2/accounts/{ACCOUNT_ID}/port_requests

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests?by_number={PHONE_NUMBER}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "account_id": "{ACCOUNT_ID}",
            "created": 63630107130,
            "id": "0684aa1e2785d62c76ce27d2451a1c26",
            "name": "Porting 202.555.9000",
            "numbers": {
                "{PHONE_NUMBER}": {}
            },
            "port_state": "canceled",
            "sent": false,
            "updated": 63630120578,
            "uploads": {
                "file.pdf": {
                    "content_type": "application/pdf",
                    "length": 90931
                }
            }
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Listing by port state

You can issue GET requests to find all ports in a particular state too.

All requests are not paginated, with the exception of the `completed` state.
Use pagination toggles for date range as desired.

##### Listing by `unconfirmed` port

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/unconfirmed

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/unconfirmed
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "account_id": "{ACCOUNT_ID}",
            "account_name": "{ACCOUNT_NAME}",
            "port_requests": [
                {
                    "account_id": "{ACCOUNT_ID}",
                    "created": 63630097779,
                    "id": "462da37f8be11e46161fb40bc71173a9",
                    "name": "Porting 202.555.9000",
                    "numbers": {
                        "+12025559000": {}
                    },
                    "port_state": "unconfirmed",
                    "sent": false,
                    "updated": 63630097779,
                    "uploads": {}
                }
            ]
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

##### Listing by `submitted` port

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/submitted

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/submitted
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "account_id": "{ACCOUNT_ID}",
            "account_name": "{ACCOUNT_NAME}",
            "port_requests": [
                {
                    "account_id": "009afc511c97b2ae693c6cc4920988e8",
                    "created": 63630130298,
                    "id": "c320d6506f1afcd715a1a49bac019c51",
                    "name": "My port req1",
                    "numbers": {
                        "+12025559042": {}
                    },
                    "port_state": "submitted",
                    "sent": false,
                    "updated": 63630130376,
                    "uploads": {}
                }
            ]
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


##### Listing by `pending` port

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/pending

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/pending
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "account_id": "{ACCOUNT_ID}",
            "account_name": "{ACCOUNT_NAME}",
            "port_requests": [
                {
                    "account_id": "009afc511c97b2ae693c6cc4920988e8",
                    "created": 63630130298,
                    "id": "c320d6506f1afcd715a1a49bac019c51",
                    "name": "My port req1",
                    "numbers": {
                        "+12025559042": {}
                    },
                    "port_state": "pending",
                    "sent": false,
                    "updated": 63630130450,
                    "uploads": {}
                }
            ]
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


##### Listing by `scheduled` port

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/scheduled

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/scheduled
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "account_id": "{ACCOUNT_ID}",
            "account_name": "{ACCOUNT_NAME}",
            "port_requests": [
                {
                    "account_id": "009afc511c97b2ae693c6cc4920988e8",
                    "created": 63630130298,
                    "id": "c320d6506f1afcd715a1a49bac019c51",
                    "name": "My port req1",
                    "numbers": {
                        "+12025559042": {}
                    },
                    "port_state": "scheduled",
                    "sent": false,
                    "updated": 63630130490,
                    "uploads": {}
                }
            ]
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


##### Listing by `completed` port

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/completed

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/completed
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [],
    "page_size": 0,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "start_key": [
        "009afc511c97b2ae693c6cc4920988e8",
        "completed",
        63630130530
    ],
    "status": "success"
}
```


##### Listing by `rejected` port

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/rejected

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/rejected
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "account_id": "{ACCOUNT_ID}",
            "account_name": "{ACCOUNT_NAME}",
            "port_requests": [
                {
                    "account_id": "009afc511c97b2ae693c6cc4920988e8",
                    "created": 63630130298,
                    "id": "c320d6506f1afcd715a1a49bac019c51",
                    "name": "My port req1",
                    "numbers": {
                        "+12025559042": {}
                    },
                    "port_state": "rejected",
                    "sent": false,
                    "updated": 63630130557,
                    "uploads": {}
                }
            ]
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


##### Listing by `canceled` port

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/canceled

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/canceled
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "account_id": "{ACCOUNT_ID}",
            "account_name": "{ACCOUNT_NAME}",
            "port_requests": [
                {
                    "account_id": "009afc511c97b2ae693c6cc4920988e8",
                    "created": 63630107130,
                    "id": "0684aa1e2785d62c76ce27d2451a1c26",
                    "name": "Porting 202.555.9000",
                    "numbers": {
                        "+12025559000": {}
                    },
                    "port_state": "canceled",
                    "sent": false,
                    "updated": 63630120578,
                    "uploads": {
                        "file.pdf": {
                            "content_type": "application/pdf",
                            "length": 90931
                        }
                    }
                }
            ]
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "start_key": [
        "009afc511c97b2ae693c6cc4920988e8",
        "canceled",
        63630129922
    ],
    "status": "success"
}
```


#### List port requests of self and sub accounts

> GET /v2/accounts/{ACCOUNT_ID}/descendants/port_requests

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/descendants/port_requests
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Get port request for account and descendants

> GET /v2/accounts/{ACCOUNT_ID}/descendants/port_requests

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/descendants/port_requests?by_number={PHONE_NUMBER}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "bill": {
                "address": "116, natoma street",
                "locality": "San Francisco",
                "name": "John Doe",
                "postal_code": "95109",
                "region": "Ca"
            },
            "carrier": "PACIFIC BELL",
            "created": 63597642009,
            "id": "84e0a824c6b74fe1e3ec48962a600ef2",
            "name": "Port request test",
            "notifications": {
                "email": {
                    "send_to": "someone@2600hz.com"
                }
            },
            "numbers": {
                "{PHONE_NUMBER}": {}
            },
            "port_state": "submitted",
            "sent": false,
            "transfer_date": 63598114800,
            "updated": 63597642011,
            "uploads": {
                "bill.pdf": {
                    "content_type": "application/pdf",
                    "length": 8304
                },
                "loa.pdf": {
                    "content_type": "application/pdf",
                    "length": 59196
                }
            }
        }
    ],
    "page_size": 1,
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Create a new port request

> PUT /v2/accounts/{ACCOUNT_ID}/port_requests

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"numbers":{"{PHONE_NUMBER}":{}}, "name":"{PORT_REQUEST_NAME}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests
```

##### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "id": "462da37f8be11e46161fb40bc71173a9",
        "name": "{PORT_REQUEST_NAME}",
        "numbers": {
            "{PHONE_NUMBER}": {}
        },
        "port_state": "unconfirmed"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

##### Failure: a port already exists for this number

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "{PHONE_NUMBER}": {
            "type": {
                "cause": "{PHONE_NUMBER}",
                "message": "Number is on a port request already: 41ed5722d24bfc69bc479208b274be6b"
            }
        }
    },
    "error": "500",
    "message": "invalid request",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```

##### Failure: an account already owns this number

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "{PHONE_NUMBER}": {
            "type": {
                "cause": "{PHONE_NUMBER}",
                "message": "Number exists on the system already"
            }
        }
    },
    "error": "500",
    "message": "invalid request",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```


#### List port request details

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630097779,
        "id": "{PORT_REQUEST_ID}",
        "name": "Porting 202.555.9000",
        "numbers": {
            "+12025559000": {}
        },
        "port_state": "unconfirmed",
        "sent": false,
        "updated": 63630097779,
        "uploads": {}
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Edit a port request

> POST /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"numbers":{"{PHONE_NUMBER}":{"state":"NY"}}, "name": "{PORT_REQUEST_NAME}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630097779,
        "id": "{PORT_REQUEST_ID}",
        "name": "{PORT_REQUEST_NAME}",
        "numbers": {
            "{PHONE_NUMBER}": {
                "state": "NY"
            }
        },
        "port_state": "unconfirmed",
        "sent": false,
        "updated": 63630104652,
        "uploads": {}
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### DELETE a port request

> DELETE /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "id": "{PORT_REQUEST_ID}",
        "name": "Porting 202.555.9000",
        "numbers": {
            "+12025559000": {
                "state": "NY"
            }
        },
        "port_state": "unconfirmed"
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### List attachments on a port request

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "file.pdf": {
            "content_type": "application/pdf",
            "length": 90931
        },
        "otherfile.pdf": {
            "content_type": "application/pdf",
            "length": 767684
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Add an attachment to a port request

Note: if `ATTACHMENT_ID` does not end with `.pdf` the extension will be appended.

> PUT /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/pdf" \
    --data-binary @/path/to/file.pdf \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments?filename={ATTACHMENT_ID}'
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {},
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Get an attachment from a port request

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments/{ATTACHMENT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Accept: application/pdf" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments/{ATTACHMENT_ID}
```

Streams back the contents of the PDF file `{ATTACHMENT_ID}`.


#### Replace an attachment on a port request

> POST /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments/{ATTACHMENT_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/pdf" \
    --data-binary @/path/to/file.pdf \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments/{ATTACHMENT_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {},
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Delete an attachment on a port request

> DELETE /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments/{ATTACHMENT_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments/{ATTACHMENT_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {},
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Indicate a port is ready to be processed

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/submitted

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/submitted
```

##### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630107130,
        "id": "{PORT_REQUEST_ID}",
        "name": "Porting 202.555.9000",
        "numbers": {
            "+12025559000": {}
        },
        "port_state": "submitted",
        "sent": false,
        "updated": 63630120443,
        "uploads": {
            "file.pdf": {
                "content_type": "application/pdf",
                "length": 90931
            }
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

###### Failure: charges have to be accepted

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "activation_charges": 10.0,
        "number_services": {
            "port": {
                "activation_charges": 10.0
            }
        }
    },
    "error": "402",
    "message": "accept charges",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```


#### Put port in pending

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/pending

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/pending
```

##### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630107130,
        "id": "{PORT_REQUEST_ID}",
        "name": "Porting 202.555.9000",
        "numbers": {
            "+12025559000": {}
        },
        "port_state": "pending",
        "sent": false,
        "updated": 63630120502,
        "uploads": {
            "file.pdf": {
                "content_type": "application/pdf",
                "length": 90931
            }
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

##### Failure: target state illegal given current state

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "port_state": {
            "enum": {
                "cause": "pending",
                "message": "Cannot move to new state from current state"
            }
        }
    },
    "error": "500",
    "message": "invalid request",
    "request_id": "{REQUEST_ID}",
    "status": "error"
}
```


#### Put port in progress (sent to losing carrier)

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/scheduled

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/scheduled
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630107130,
        "id": "{PORT_REQUEST_ID}",
        "name": "Porting 202.555.9000",
        "numbers": {
            "+12025559000": {}
        },
        "port_state": "scheduled",
        "sent": false,
        "updated": 63630120528,
        "uploads": {
            "file.pdf": {
                "content_type": "application/pdf",
                "length": 90931
            }
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Complete port, numbers will activate in the system, account will be billed

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/completed

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/completed
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630107130,
        "id": "{PORT_REQUEST_ID}",
        "name": "Porting 202.555.9000",
        "numbers": {
            "+12025559000": {}
        },
        "port_state": "completed",
        "sent": false,
        "updated": 63630120570,
        "uploads": {
            "file.pdf": {
                "content_type": "application/pdf",
                "length": 90931
            }
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Reject a port

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/rejected

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/rejected
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630107130,
        "id": "{PORT_REQUEST_ID}",
        "name": "Porting 202.555.9000",
        "numbers": {
            "+12025559000": {}
        },
        "port_state": "rejected",
        "sent": false,
        "updated": 63630120570,
        "uploads": {
            "file.pdf": {
                "content_type": "application/pdf",
                "length": 90931
            }
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Cancel a port

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/canceled

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/canceled
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630107130,
        "id": "{PORT_REQUEST_ID}",
        "name": "Porting 202.555.9000",
        "numbers": {
            "+12025559000": {}
        },
        "port_state": "canceled",
        "sent": false,
        "updated": 63630120578,
        "uploads": {
            "file.pdf": {
                "content_type": "application/pdf",
                "length": 90931
            }
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```


#### Build an LOA PDF from a port request

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/loa

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Accept: application/x-pdf" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/loa
```

Streams back the contents of the generated Letter Of Authorization PDF.
