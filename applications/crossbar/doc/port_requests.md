# Number Port Requests

## About Port Requests

Manage and track number port requests through the Port Requests API.


<details>
<summary><strong>Table of Content</strong></summary>

* [About](#about-port-requests)
    - [Port states](#port-request-states)
    - [Port authority](#port-authority-port-agent)
    - [Schema](#schema)
* [Port Request Listing](#list-port-requests)
    - [List port requests by type](#list-port-requests-by-type)
    - [Search/list a port request by phone number](#searchlist-a-port-request-by-phone-number)
    - [List port requests that are managed by you](#list-port-requests-that-are-managed-by-you)
    - [List port requests for an account](#list-port-requests-for-an-account)
    - [List port requests of sub-accounts](#list-port-requests-of-sub-accounts)
    - [Listing latest submitted port requests](#listing-all-port-requests-by-their-last-transition-to-the-submitted-state)
    - [Listing transitions and comments](#listing-transitions-and-comments)
* [Port Request Management](#port-request-management)
    - [Create](#create-a-new-port-request)
        + [Success](#success)
        + [Failure: a port already exists for this number](#failure-a-port-already-exists-for-this-number)
        + [Failure: an account already owns this number](#failure-an-account-already-owns-this-number)
    - [Get](#get-port-request-details)
    - [Update](#edit-a-port-request)
    - [Patch](#patch-a-port-request)
    - [Delete](#delete-a-port-request)
* Attachment Management
    - [Create](#add-an-attachment-to-a-port-request)
    - [List](#list-attachments-on-a-port-request)
    - [Get](#get-an-attachment-from-a-port-request)
    - [Update](#replace-an-attachment-on-a-port-request)
    - [Delete](#delete-an-attachment-on-a-port-request)
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
* [Build an LOA PDF from a port request](#build-an-loa-pdf-from-a-port-request)
</details>

### Port request stats

A port request can be in one of seven **states**:

* `unconfirmed`: A port request has been created, but the details have not been confirmed and the port process has not started.
* `submitted`: Indicates the number port is ready to be processed and sent to the losing carrier.
* `pending`: The port was submitted to the losing carrier.
* `scheduled`: The port is in progress and the losing carrier has been notified.
* `completed`: The port request has been finished, and numbers are activated.
* `rejected`: Something has gone wrong during the port process. The port can be resubmitted.
* `canceled`: The port request is definitely canceled and cannot be resubmitted.

#### Port states diagram

![porting state flow](images/port-request-states-flow.svg)

### Port authority (Port Agent)

Port authority (sometimes called port agent or simply agent) is an account in Kazoo system which is responsible for managing
their sub account port requests. They are responsible to submitted and oversees port request to losing/winner carrier.

Master account is default port authority.

If you want to manage port request for your sub account, you have to white label your account
and set "I will manage port request from my account" in Branding application, "Advanced" tab.
If you select that option, you have to set an Email address for port "Support Contact" in the same section.

<details>
<summary><strong>Port Request Schema</strong></summary>

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



</details>


## List port requests

Port request API URL path has specific meaning:

* `/v2/port_requests`: List port requests that the authenticated account is port authority for.
* `/v2/accounts/{ACCOUNT_ID}/port_requests`: List only port request for a specific account.
* `/v2/accounts/{ACCOUNT_ID}/descendants/port_requests`: List all sub-accounts port requests.

### List port requests by type

You can issue GET requests to find all ports in a particular state.

All requests are not paginated, with the exception of the `completed` state. Use pagination toggles for date range as desired.

To do this add `by_types` query string to the request. If you don't set  `by_types`, by default `active` port request will be shown. Possible values are:

* `all`: Lists port requests in any state
* `active`: Lists port requests which are in these states:
    * `unconfirmed`
    * `submitted`
    * `pending`
    * `scheduled`
    * `rejected`
* `progressing`: Lists port requests which are in these states:
    * `submitted`
    * `pending`
    * `scheduled`
* `suspended`: Lists port requests which are in these states:
    * `unconfirmed`
    * `rejected`
* `completed`: Lists port requests which are in these states:
    * `completed`
    * `canceled`
* `last_submitted`: Lists all port requests by their last transition to the `submitted` state
* Port requests states: List port requests bu their states:
    * `unconfirmed`
    * `submitted`
    * `pending`
    * `scheduled`
    * `completed`
    * `rejected`
    * `canceled`

#### Example usage of by_type

Here an example of setting `by_types`:

```
/v2/port_requests?by_types=prgressing
/v2/accounts/{ACCOUNT_ID}/port_requests?by_types=prgressing
```

You can also specify multiple types by separating them by comma like example below:

```
/v2/port_requests?by_types=pending,scheduled,rejected
/v2/accounts/{ACCOUNT_ID}/port_requests?by_types=pending,scheduled,rejected
```

### Search/list a port request by phone number

For finding a port request by a phone number set `by_number` in query string. Including an account ID in the URL will change how the port requests are searched:

* `/v2/port_requests?by_number={PHONE_NUMBER}`: This will search for port request in the authenticated account and all its sub-accounts.
* `/v2/accounts/{ACCOUNT_ID}/port_requests`: This will search in the `{ACCOUNT_ID}` and all its sub-account

<details>
<summary><strong>Example request/response</strong></summary>

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
</details>


### List port requests that are managed by you

This lists all port requests that you (authenticated account) are managing and port authority for.
See [Port Authority](#port-authority-port-agent) for more details.

> GET /v2/port_requests

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/port_requests
```

<details>
<summary><strong>Sample response</strong></summary>

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
</details>


### List port requests for an account

This only lists port requests of a single account.

> GET /v2/accounts/{ACCOUNT_ID}/port_requests

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests
```

Response is same as [listing for agent](#list-port-requests-that-are-managed-by-you).


### List port requests of sub-accounts

> GET /v2/accounts/{ACCOUNT_ID}/descendants/port_requests

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/descendants/port_requests
```

Response is same as [listing for agent](#list-port-requests-that-are-managed-by-you).


### Listing all port requests by their last transition to the `submitted` state

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/last_submitted

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/last_submitted
```

<details>
<summary><strong>Sample response</strong></summary>

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
</details>


### Listing transitions and comments

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/timeline

This shows the port request's timeline as a sorted list of transitions and comments.

Admins are able to list every transitions and comments regardless of their privacy setting.
Non admins only see transitions and public comments.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/timeline
```

<details>
<summary><strong>Sample response</strong></summary>

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
            "account_id": "0d46906ff1eb36bff4d09b5b32fc14be",
            "action_required": true,
            "content": "the comment is private, and user is required to make an action port request",
            "is_private": true,
            "timestamp": 63664000760,
            "user_id": "0d46906ff1eb36bff4d09b5b32fc14be"
        },
        {
            "account_id": "0d46906ff1eb36bff4d09b5b32fc14be",
            "content": "this is not private",
            "action_required": false,
            "is_private": false,
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
</details>


## Port request management

### Create a new port request

> PUT /v2/accounts/{ACCOUNT_ID}/port_requests

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"numbers":{"{PHONE_NUMBER}":{}}, "name":"{PORT_REQUEST_NAME}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests
```
<details>
<summary><strong>Responses</strong></summary>

#### Success

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "id": "462da37f8be11e46161fb40bc71173a9",
        "name": "{PORT_REQUEST_NAME}",
        "numbers": {
            "{PHONE_NUMBER}": {}
        },
        "port_state": "unconfirmed",
        "uploads": {},
        "_read_only": {
            "port_authority": "{PORT_AUTHORITY_ACCOUNT_ID}",
            "port_authority_name": "{PORT_AUTHORITY_ACCOUNT_NAME}"
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

#### Failure: a port already exists for this number

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

#### Failure: an account already owns this number

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
</details>


### Get port request details

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}
```

<details>
<summary><strong>Sample response</strong></summary>

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
        "uploads": {},
        "_read_only": {
            "port_authority": "{PORT_AUTHORITY_ACCOUNT_ID}",
            "port_authority_name": "{PORT_AUTHORITY_ACCOUNT_NAME}"
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
</details>


### Edit a port request

> POST /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"numbers":{"{PHONE_NUMBER}":{"state":"NY"}}, "name": "{PORT_REQUEST_NAME}"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}
```

<details>
<summary><strong>Sample response</strong></summary>

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
        "_read_only": {
            "port_authority": "{PORT_AUTHORITY_ACCOUNT_ID}",
            "port_authority_name": "{PORT_AUTHORITY_ACCOUNT_NAME}"
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
</details>

### Patch a port request

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"your_custom_info":{"carrier_port_id": "apc-8535937-gtk123", "carrier_name": "ace phone co"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}
```

<details>
<summary><strong>Sample response</strong></summary>

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
        },
        "_read_only": {
            "port_authority": "{PORT_AUTHORITY_ACCOUNT_ID}",
            "port_authority_name": "{PORT_AUTHORITY_ACCOUNT_NAME}"
        }
    }
}
```
</details>

### DELETE a port request

> DELETE /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}
```

<details>
<summary><strong>Sample response</strong></summary>

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
        "port_state": "unconfirmed",
        "_read_only": {
            "port_authority": "{PORT_AUTHORITY_ACCOUNT_ID}",
            "port_authority_name": "{PORT_AUTHORITY_ACCOUNT_NAME}"
        }
    },
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```
</details>

## Attachment Management

### List attachments on a port request

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments
```

<details>
<summary><strong>Sample response</strong></summary>

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
</details>


### Add an attachment to a port request

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


### Get an attachment from a port request

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments/{ATTACHMENT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Accept: application/pdf" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/attachments/{ATTACHMENT_ID}
```

Streams back the contents of the PDF file `{ATTACHMENT_ID}`.


### Replace an attachment on a port request

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


### Delete an attachment on a port request

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


## Updating a port request status

When PATCHing a port request a reason can be added to the transition with the following request value:
* `reason`: an optional string that can be used to describe the reason for the transition

This information will then be available in the timeline.

Note: request values can be set either in the query string or in the data payload.

### Indicate a port is ready to be processed

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/submitted

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/submitted?reason=this+was+approved+by+Jane+Doe
```

<details>
<summary><strong>Sample response</strong></summary>

#### Success

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

#### Failure: charges have to be accepted

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
</details>


### Put port in pending

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/pending

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/pending
```

<details>
<summary><strong>Sample response</strong></summary>

#### Success

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

#### Failure: target state illegal given current state

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
</details>


### Put port in progress (sent to losing carrier)

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/scheduled

!!! note
-   `schedule_on` is a required field for this state transition.

!!! note
-   `scheduled_date` is an automatically added timestamp computed from the value of the `schedule_on` object.

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"schedule_on": {"timezone":"America/Los_Angeles", "date_time":"2017-06-24 12:00"}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/scheduled
```

<details>
<summary><strong>Sample response</strong></summary>

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
</details>


### Complete port, numbers will activate in the system, account will be billed

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/completed

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/completed
```

<details>
<summary><strong>Sample response</strong></summary>

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
</details>


### Reject a port

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/rejected

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/rejected
```

<details>
<summary><strong>Sample response</strong></summary>

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
</details>


### Cancel a port

> PATCH /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/canceled

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/canceled
```

<details>
<summary><strong>Sample response</strong></summary>

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
</details>


## Build an LOA PDF from a port request

Download the generated Letter Of Authorization PDF file.

> GET /v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/loa

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Accept: application/x-pdf" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/port_requests/{PORT_REQUEST_ID}/loa
```
