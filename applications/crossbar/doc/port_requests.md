# Number Port Requests

* [About Port Requests](#about-port-requests)
* [Schema](#schema)
* Utils
    + [Build an LOA PDF from a port request](#build-an-loa-pdf-from-a-port-request)
    + [Get port request by phone number](#get-a-port-request-by-phone-number)
    + [Get port request by phone number for account and descendants](#get-port-request-for-account-and-descendants)
* Port Request Listing
    - [List all](#list-port-requests)
    - [List all by number](#list-port-requests-by-number-of-self-and-subaccounts)
    - [List all for sub accounts](#list-port-requests-of-self-and-sub-accounts)
    - [List by state](#listing-by-port-state)
        + [`unconfirmed`](#listing-by-unconfirmed-port)
        + [`submitted`](#listing-by-submitted-port)
        + [`pending`](#listing-by-pending-port)
        + [`scheduled`](#listing-by-scheduled-port)
        + [`completed`](#listing-by-completed-port)
        + [`rejected`](#listing-by-rejected-port)
        + [`canceled`](#listing-by-canceled-port)
* Port Request Management
    - [Create](#create-a-new-port-request)
        + [Success](#success)
        + [Failure: a port already exists for this number](#failure-a-port-already-exists-for-this-number)
        + [Failure: an account already owns this number](#failure-an-account-already-owns-this-number)
    - [Get](#get-port-request-details)
    - [Update](#edit-a-port-request)
    - [Delete](#delete-a-port-request)
* Timeline
    - [Listing transitions and comments](#listing-transitions-and-comments)
* Update Status
    - [`submitted`](#indicate-a-port-is-ready-to-be-processed)
        + [Success](#success_1)
        + [Failure: charges have to be accepted](#failure-charges-have-to-be-accepted)
    - [`pending`](#put-port-in-pending)
        + [Success](#success_2)
        + [Failure: target state illegal given current state](#failure-target-state-illegal-given-current-state)
    - [`scheduled`](#put-port-in-progress-sent-to-losing-carrier)
    - [`completed`](#complete-port-numbers-will-activate-in-the-system-account-will-be-billed)
    - [`rejected`](#reject-a-port)
    - [`canceled`](#cancel-a-port)
* Attachments
    - [Create](#add-an-attachment-to-a-port-request)
    - [List](#list-attachments-on-a-port-request)
    - [Get](#get-an-attachment-from-a-port-request)
    - [Update](#replace-an-attachment-on-a-port-request)
    - [Delete](#delete-an-attachment-on-a-port-request)

## About Port Requests

Manage and track number port requests through the Port Requests API.

A port request can be in one of seven **states**:

* `unconfirmed`: A port request has been created, but the details have not been confirmed and the port process has not started.
* `submitted`: Indicates the number port is ready to be processed and sent to the losing carrier.
* `pending`: The port was submitted to the losing carrier.
* `scheduled`: The port is in progress and the losing carrier has been notified.
* `completed`: The port request has been finished, and numbers are activated.
* `rejected`: Something has gone wrong during the port process. The port can be resubmitted.
* `canceled`: The port request is definitely canceled and cannot be resubmitted.

![porting state flow](images/port-request-states-flow.svg)

#### Schema

Schema for a port request



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`bill.carrier` | The name of the losing carrier | `string()` |   | `false` |  
`bill.extended_address` | The suite/floor/apt of the billing address the losing carrier has on record | `string()` |   | `false` |  
`bill.locality` | The locality (city) of the billing address the losing carrier has on record | `string()` |   | `false` |  
`bill.name` | The losing carrier billing/account name | `string()` |   | `false` |  
`bill.postal_code` | The zip/postal code of the billing address the losing carrier has on record | `string()` |   | `false` |  
`bill.region` | The region (state) of the billing address the losing carrier has on record | `string()` |   | `false` |  
`bill.street_address` | The street name of the billing address the losing carrier has on record | `string()` |   | `false` |  
`bill.street_number` | The street number of the billing address the losing carrier has on record | `string()` |   | `false` |  
`bill.street_type` | The street type of the billing address the losing carrier has on record | `string()` |   | `false` |  
`bill` | Billing information of the losing carrier | `object()` |   | `false` |  
`comments` | The history of comments made on a port request | `array(object())` |   | `false` |  
`name` | A friendly name for the port request | `string(1..128)` |   | `true` |  
`notifications.email.send_to` | A list or string of email recipient(s) | `string() | array(string())` |   | `false` |  
`notifications.email` | Inbound Email Notifications | `object()` |   | `false` |  
`notifications` | Status notifications | `object()` |   | `false` |  
`numbers./\+?[0-9]+/` |   | `object()` |   | `false` |  
`numbers` | The numbers to port in | `object()` |   | `true` |  
`port_state` | What state the port request is currently in | `string('unconfirmed' | 'pending' | 'submitted' | 'scheduled' | 'completed' | 'rejected' | 'canceled')` | `unconfirmed` | `false` |  
`signee_name` | The name of the person authorizing the release of numbers from the losing carrier | `string()` |   | `false` |  
`signing_date` | The date in Gregorian timestamp on which the document releasing the numbers from the losing carrier was signed | `integer()` |   | `false` |  
`transfer_date` | Requested transfer date in Gregorian timestamp | `integer()` |   | `false` |  



#### Fetch

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

#### Search a port request by phone number from superduper admin across all sub-accounts

> GET /v2/port_requests

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/port_requests?by_number={PHONE_NUMBER}
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
                    "schedule_on": {
                        "date_time": "2017-06-24 12:00",
                        "timezone": "America/New_York"
                    },
                    "scheduled_date": 63665539200,
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


#### Get port request details

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
## Patch a port request

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"your_custom_info":{"carrier_port_id": "apc-8535937-gtk123", "carrier_name": "ace phone co"}}' \
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
        "uploads": {},
        "your_custom_info": {
            "carrier_port_id": "apc-8535937-gtk123",
            "carrier_name": "ace phone co"
        }
    }
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
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments?filename={ATTACHMENT_ID}
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

#### Listing all port requests by their last transition to the `submitted` state

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/last_submitted

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/last_submitted
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "{PORT_REQUEST_ID}",
            "transition": {
                "authorization": {
                    "account": {
                        "id": "{AUTH_ACCOUNT_ID}",
                        "name": "{AUTH_ACCOUNT_NAME}"
                    },
                    "user": {
                        "id": "0d46906ff1eb36bff4d09b5b32fc14be",
                        "first_name": "John",
                        "last_name": "Doe"
                    }
                },
                "reason": "this was approved by Jane Doe",
                "timestamp": 63664096014,
                "transition": {
                    "new": "submitted",
                    "previous": "unconfirmed"
                },
                "type": "transition"
            }
        ]
    },
    "node": "{NODE}",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success",
    "timestamp": "2017-06-07T23:07:09",
    "version": "4.1.12"
}
```


#### Listing transitions and comments

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/timeline

This shows the port request's timeline as a sorted list of transitions and comments.

Admins are able to list every transitions and comments regardless of their privacy setting.
Non admins only see transitions and public comments.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/timeline
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "authorization": {
                "account": {
                    "id": "{AUTH_ACCOUNT_ID}",
                    "name": "{AUTH_ACCOUNT_NAME}"
                },
                "user": {
                    "id": "0d46906ff1eb36bff4d09b5b32fc14be",
                    "first_name": "John",
                    "last_name": "Doe"
                }
            },
            "timestamp": 63663993575,
            "transition": {
                "new": "unconfirmed"
            },
            "type": "transition"
        },
        {
            "content": "the previous comment was private, this one is not",
            "superduper_comment": false,
            "timestamp": 63664000760,
            "user_id": "0d46906ff1eb36bff4d09b5b32fc14be"
        },
        {
            "content": "this is not private",
            "superduper_comment": false,
            "timestamp": 63664000768,
            "user_id": "0d46906ff1eb36bff4d09b5b32fc14be"
        },
        {
            "authorization": {
                "account": {
                    "id": "{AUTH_ACCOUNT_ID}",
                    "name": "{AUTH_ACCOUNT_NAME}"
                },
                "user": {
                    "id": "0d46906ff1eb36bff4d09b5b32fc14be",
                    "first_name": "John",
                    "last_name": "Doe"
                }
            },
            "reason": "this was approved by Jane Doe",
            "timestamp": 63664096014,
            "transition": {
                "new": "submitted",
                "previous": "unconfirmed"
            },
            "type": "transition"
        }
    ],
    "node": "{NODE}",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success",
    "timestamp": "2017-06-07T23:07:09",
    "version": "4.1.12"
}
```


### Updating a port request's status

When PATCHing a port request a reason can be added to the transition with the following request value:
* `reason`: an optional string that can be used to describe the reason for the transition

This information will then be available in the timeline.

Note: request values can be set either in the query string or in the data payload.

#### Indicate a port is ready to be processed

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/submitted

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/submitted?reason=this+was+approved+by+Jane+Doe
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

Note: `schedule_on` is a required field for this state transition.
Note: `scheduled_date` is an automatically added timestamp computed from the value of the `schedule_on` object.

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"schedule_on": {"timezone":"America/Los_Angeles", "date_time":"2017-06-24 12:00"}}}' \
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
        "schedule_on": {
            "date_time": "2017-06-24 12:00",
            "timezone": "America/Los_Angeles"
        },
        "scheduled_date": 63658292400,
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
