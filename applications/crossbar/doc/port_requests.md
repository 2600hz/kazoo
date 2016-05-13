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
    "request_id": "6b667214680d1cc3143b8a187d820af6",
    "revision": "undefined",
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
    "request_id": "24060fb2eb79e6dd0d29b290e2cb7085",
    "revision": "bb7443aecba069b09d7197410dc02fbe",
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
    "request_id": "ecd09332796338bd1cfada0a74e6bd28",
    "revision": "db7651aaeed4a373915b516963cd6dd4",
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
    "request_id": "1bf407e087f04e6096f7f010c4eef045",
    "revision": "e4058257bbc9e33ceab14632c5662eaf",
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
    "request_id": "156c9979f04229a0088653bfbff0b08c",
    "revision": "cc55f42be1e2294a9291a2dca8087d10",
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
    "request_id": "ab348636e435cd01888356607b183697",
    "revision": "ae048f434e5b16749c721eb0ab8fa781",
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
    "request_id": "388313efcf12e4eda5672b5336828350",
    "revision": "undefined",
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
    "request_id": "92200b874efd5cbb641599f47cd2431a",
    "revision": "26e7f1549eed11fb1d5418c58424baf6",
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
    "request_id": "745be8bfd341636eabb9a0b9572846eb",
    "revision": "bddb6f5c0566d7a84ca2877abff750a8",
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
    "request_id": "48f4e5e053801a13b4e4e3e3ba9c10c4",
    "revision": "undefined",
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
                "{{NUMBER}}": {}
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
    "request_id": "82db6e47a0adafc1db83369723390dce",
    "revision": "4dd6dc369615b519373a4deeaa1567bd",
    "status": "success"
}
```


#### Create a new port request

> PUT /v2/accounts/{ACCOUNT_ID}/port_requests

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"numbers":{"{PHONE_NUMBER}":{}}, "name":"{PORTREQUEST_NAME}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests
```

##### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "id": "462da37f8be11e46161fb40bc71173a9",
        "name": "{PORTREQUEST_NAME}",
        "numbers": {
            "{PHONE_NUMBER}": {}
        },
        "port_state": "unconfirmed"
    },
    "request_id": "54672607e9884437069abb7e5a891a5c",
    "revision": "1-184548f264275cf9351872e21a195152",
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
    "request_id": "7cdd44775dc7b583a2628e05588c439b",
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
    "request_id": "05ec9cf8811661fed103864e2ed07bcd",
    "status": "error"
}
```


#### List port request details

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630097779,
        "id": "{PORTREQUEST_ID}",
        "name": "Porting 202.555.9000",
        "numbers": {
            "+12025559000": {}
        },
        "port_state": "unconfirmed",
        "sent": false,
        "updated": 63630097779,
        "uploads": {}
    },
    "request_id": "32afa58869e88fd709f88739f2d0bb12",
    "revision": "1-184548f264275cf9351872e21a195152",
    "status": "success"
}
```


#### Edit a port request

> POST /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"numbers":{"+12025559000":{"state":"NY"}}, "name": "{PORTREQUEST_NAME}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630097779,
        "id": "{PORTREQUEST_ID}",
        "name": "{PORTREQUEST_NAME}",
        "numbers": {
            "+12025559000": {
                "state": "NY"
            }
        },
        "port_state": "unconfirmed",
        "sent": false,
        "updated": 63630104652,
        "uploads": {}
    },
    "request_id": "aed9b0228ef02bcd92322442b02204e9",
    "revision": "2-b43873df23aedaaa96682ec2da32e688",
    "status": "success"
}
```


#### DELETE a port request

> DELETE /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "id": "{PORTREQUEST_ID}",
        "name": "Porting 202.555.9000",
        "numbers": {
            "+12025559000": {
                "state": "NY"
            }
        },
        "port_state": "unconfirmed"
    },
    "request_id": "80b315b87c9db92a2ae755c581d838a3",
    "revision": "2-b43873df23aedaaa96682ec2da32e688",
    "status": "success"
}
```


#### List attachments on a port request

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments
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
    "request_id": "ed71507eeccbd3eeab8ccd7bd6cf023c",
    "revision": "6-ade05f2d7e539be79f425bfb39e41cef",
    "status": "success"
}
```


#### Add an attachment to a port request

Note: if `ATTACHMENT_ID` does not end with `.pdf` the extension will be appended.

> PUT /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/pdf" \
    --data-binary @/path/to/file.pdf \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments?filename={ATTACHMENT_ID}'
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {},
    "request_id": "e9db1aa74945884eed5a4545a405b490",
    "revision": "2-7ab1daee08211c782a6cdc5425886be4",
    "status": "success"
}
```


#### Get an attachment from a port request

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Accept: application/pdf" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}
```

Streams back the contents of the PDF file `{ATTACHMENT_ID}`.


#### Replace an attachment on a port request

> POST /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/pdf" \
    --data-binary @/path/to/file.pdf \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {},
    "request_id": "5ccaf7eb473c350c2e460ae5b4a76697",
    "revision": "4-288d2d9e121c8e5d5e0d137626d70536",
    "status": "success"
}
```


#### Delete an attachment on a port request

> DELETE /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/attachments/{ATTACHMENT_ID}
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {},
    "request_id": "1178da494ef5a953240b33eeaa7df859",
    "revision": "undefined",
    "status": "success"
}
```


#### Indicate a port is ready to be processed

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/submitted

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/submitted
```

##### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630107130,
        "id": "{PORTREQUEST_ID}",
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
    "request_id": "6a05c7559aabd2ff55f13e364ccfa63c",
    "revision": "14-f5fcbb593efe8719d9b3e29cb745ead9",
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
    "request_id": "9abd3b133b4bf2e2f006fa164a842441",
    "status": "error"
}
```


#### Put port in pending

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/pending

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/pending
```

##### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630107130,
        "id": "{PORTREQUEST_ID}",
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
    "request_id": "f9012ea6d97a1fa3a54e3f5615e5ad24",
    "revision": "15-eefd336bebf12ec109880527a01387c2",
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
    "request_id": "a6c9b35b2fde177b833ff79e78dcc21c",
    "status": "error"
}
```


#### Put port in progress (sent to losing carrier)

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/scheduled

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/scheduled
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630107130,
        "id": "{PORTREQUEST_ID}",
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
    "request_id": "9942dcff784138431b268b6bd140862f",
    "revision": "16-1377609b9166abf7a5dbb6b25a5a3890",
    "status": "success"
}
```


#### Complete port, numbers will activate in the system, account will be billed

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/completed

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/completed
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630107130,
        "id": "{PORTREQUEST_ID}",
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
    "request_id": "b760e5f682423a3e7224b1afc21e0f48",
    "revision": "1-8edacf2b310d0fd1e919fda51ed10306",
    "status": "success"
}
```


#### Reject a port

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/rejected

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/rejected
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630107130,
        "id": "{PORTREQUEST_ID}",
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
    "request_id": "8edacf2b310d0fd1e919fda51ed10306",
    "revision": "17-84f0e12cfa1b4227e3a324286f5e067b",
    "status": "success"
}
```


#### Cancel a port

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/canceled

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/canceled
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "created": 63630107130,
        "id": "{PORTREQUEST_ID}",
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
    "request_id": "cb1cff969c1513d8a21df2e4c5bbe341",
    "revision": "18-82cca84c2b3d9019309a68f1d95c2d0c",
    "status": "success"
}
```


#### Build an LOA PDF from a port request

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/loa

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Accept: application/x-pdf" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORTREQUEST_ID}/loa
```

Streams back the contents of the generated Letter Of Authorization PDF.
