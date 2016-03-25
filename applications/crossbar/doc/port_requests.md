
### Port Requests
Manage and track number port requests through the Port Requests API.

A port request can be in one of five states:
* Unconfirmed: A port request has been created, but the details have not been confirmed and the port process has not started.
* Submitted: Indicates the number port is ready to be processed and sent to the losing carrier.
* Scheduled: The port is in progress and the losing carrier has been notified.
* Completed: The port request has been finished, and numbers are activated.
* Rejected: The port request has been cancelled, or something has gone wrong during the port process. The port can be resubmitted.

##### List port requests

    curl -v -X GET \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests

###### Listing by port state

You can issue GET requests to find all ports in a particular state too:

    curl -v -X GET \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{STATE_NAME}

Where `{STATE_NAME}` is one of:

* submitted
* pending
* scheduled
* completed
* rejected
* canceled

All requests are not paginated, with the exception of the `completed` state. Use pagination toggles for date range as desired.

##### List port requests of self and sub accounts

    curl -v -X GET \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/descendants/port_requests

##### Create a new port request

    curl -v -X PUT \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"numbers":{"+12025559000":{}}, "name":"Porting 202.555.9000"}}' \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests

##### List port request details

    curl -v -X GET \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{{PORT_REQUEST_ID}}

##### Edit a port request

    curl -v -X POST \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"numbers":{"+12025559000":{"state":"NY"}}}}' \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{{PORT_REQUEST_ID}}

##### DELETE a port request

    curl -v -X DELETE \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    -H "Content-Type: application/json" \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{{PORT_REQUEST_ID}}

#### Attachment manipulation

##### List attachments on a port request

    curl -v -X GET \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    -H "Content-Type: application/json" \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{{PORT_REQUEST_ID}}/attachments

##### Add an attachment to a port request

    curl -v -X PUT \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    -H "Content-Type: application/pdf" \
    --data-binary @/path/to/file.pdf \
    'http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{{PORT_REQUEST_ID}}/attachments?filename=file.pdf'

##### Get an attachment from a port request

    curl -v -X GET -H \
    "X-Auth-Token: {{AUTH_TOKEN}}" -H \
    "Accept: application/pdf" \
    'http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{{PORT_REQUEST_ID}}/attachments/file.pdf' > file.pdf

##### Replace an attachment on a port request

    curl -v -X POST \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    -H "Content-Type: application/pdf" \
    --data-binary @/path/to/file.pdf \
    'http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{{PORT_REQUEST_ID}}/attachments/file.pdf'

##### Delete an attachment on a port request

    curl -v -X DELETE \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    -H "Content-Type: application/json" \
    'http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{{PORT_REQUEST_ID}}/attachments/otp.pdf'

#### State changes

##### Indicate a port is ready to be processed

    curl -v -X PATCH \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    -H "Content-Type: application/json" \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{{PORT_REQUEST_ID}}/submitted

##### Put port in pending

    curl -v -X PATCH \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    -H "Content-Type: application/json" \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{{PORT_REQUEST_ID}}/pending

##### Put port in progress (sent to losing carrier)

    curl -v -X PATCH \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    -H "Content-Type: application/json" \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{{PORT_REQUEST_ID}}/scheduled

##### Complete port, numbers will activate in the system, account will be billed

    curl -v -X PATCH \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    -H "Content-Type: application/json" \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{{PORT_REQUEST_ID}}/completed

##### Reject a port

    curl -v -X PATCH \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    -H "Content-Type: application/json" \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{{PORT_REQUEST_ID}}/rejected

##### Cancel a port

    curl -v -X PATCH \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    -H "Content-Type: application/json" \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{{PORT_REQUEST_ID}}/canceled

#### Extra features

##### Build an LOA PDF from a port request

    curl -v -X GET \
    -H "Accept: application/x-pdf" \
    -H "X-Auth-Token: {{AUTH_TOKEN}}" \
    http://{{SERVER}}:8000/v2/accounts/{{ACCOUNT_ID}}/port_requests/{{PORT_REQUEST_ID}}/loa

#### Get port request for account

##### Request

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

##### Request

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
