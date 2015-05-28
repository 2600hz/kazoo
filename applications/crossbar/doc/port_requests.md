/*
Section: Crossbar
Title: Port Requests
Language: en-US
*/

# Port Requests
Manage and track number port requests through the Port Requests API.

A port request can be in one of five states:
* Unconfirmed: A port request has been created, but the details have not been confirmed and the port process has not started.
* Submitted: Indicates the number port is ready to be processed and sent to the losing carrier.
* Scheduled: The port is in progress and the losing carrier has been notified.
* Completed: The port request has been finished, and numbers are activated.
* Rejected: The port request has been cancelled, or something has gone wrong during the port process. The port can be resubmitted.


## Credentials Hash: 4d02ff46ad889921836b706c3c0e0b36
## Account Name: Master
## Auth Token ID: 6cf321ea39fe960d9b855786a0216064
## Account ID: 5b78db2f23f35aa022f5c3c0a5df1b92
## Port Request ID: b38b134866f87eb196c408f40ededc83

### Create an auth token on the master account
```
curl -v -X PUT -H "content-type:application/json" http://thinky64.2600hz.com:8000/v1/user_auth -d '{"data":{"credentials":"4d02ff46ad889921836b706c3c0e0b36", "account_name":"Master"}}' | pp
```

### List port requests
```
curl -v -X GET -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests | pp
```

### List port requests of self and sub accounts
```
curl -v -X GET -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests/descendants | pp
```

### Create a new port request
```
curl -v -X PUT -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" -H "Content-Type: application/json" http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests -d '{"data":{"numbers":{"+12025559000":{}}}}' | pp
```

### List port request details
```
curl -v -X GET -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests/b38b134866f87eb196c408f40ededc83 | pp
```

### Edit a port request
```
curl -v -X POST -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" -H "Content-Type: application/json" http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests/b38b134866f87eb196c408f40ededc83 -d '{"data":{"numbers":{"+12025559000":{"state":"NY"}}}}' | pp
```

### DELETE a port request
```
curl -v -X DELETE -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" -H "Content-Type: application/json" http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests/b38b134866f87eb196c408f40ededc83 | pp
```

## Attachment manipulation

### List attachments on a port request
```
curl -v -X GET -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" -H "Content-Type: application/json" http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests/b38b134866f87eb196c408f40ededc83/attachments | pp
```

### Add an attachment to a port request
```
curl -v -X PUT -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" -H "Content-Type: application/pdf" 'http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests/b38b134866f87eb196c408f40ededc83/attachments?filename=otp.pdf' --data-binary @/home/james/Documents/ErlangandOTPinAction.pdf | pp
```

### Get an attachment from a port request
```
curl -v -X GET -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" -H "Accepts: application/pdf" 'http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests/b38b134866f87eb196c408f40ededc83/attachments/otp.pdf' > f.pdf
```

### Replace an attachment on a port request
```
curl -v -X POST -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" -H "Content-Type: application/pdf" 'http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests/b38b134866f87eb196c408f40ededc83/attachments/otp.pdf' --data-binary @/home/james/Documents/ErlangandOTPinAction.pdf | pp
```

### Delete an attachment on a port request
```
curl -v -X DELETE -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" -H "Content-Type: application/json" 'http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests/b38b134866f87eb196c408f40ededc83/attachments/otp.pdf'
```

## State changes

### Indicate a port is ready to be processed
```
curl -v -X PUT -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" -H "Content-Type: application/json" http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests/b38b134866f87eb196c408f40ededc83/submitted | pp
```

### Put port in progress (sent to losing carrier)
```
curl -v -X PUT -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" -H "Content-Type: application/json" http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests/b38b134866f87eb196c408f40ededc83/scheduled | pp
```

### Complete port, numbers will activate in the system, account will be billed
```
curl -v -X PUT -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" -H "Content-Type: application/json" http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests/b38b134866f87eb196c408f40ededc83/completed | pp
```

### Reject a port
```
curl -v -X PUT -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" -H "Content-Type: application/json" http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests/b38b134866f87eb196c408f40ededc83/rejected | pp
```

## Extra features

### Build an LOA PDF from a port request
```
curl -v -X GET -H "Accepts: application/x-pdf" -H "X-Auth-Token: 6cf321ea39fe960d9b855786a0216064" http://thinky64.2600hz.com:8000/v1/accounts/5b78db2f23f35aa022f5c3c0a5df1b92/port_requests/b38b134866f87eb196c408f40ededc83/loa | pp
```

## Get port request for account

### Request

- Verb: `GET`
- Url: `/accounts/{ACCOUNT_ID}/port_requests?by_number={NUMBER}`
- Payload: None


### Response

```
{
    "page_size": 1,
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
            "{NUMBER}": {}
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
```

## Get port request for account and descendants

### Request

- Verb: `GET`
- Url: `/accounts/{ACCOUNT_ID}/port_requests/descendants?by_number={NUMBER}`
- Payload: None


### Response

```
{
    "page_size": 1,
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
            "{NUMBER}": {}
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
```