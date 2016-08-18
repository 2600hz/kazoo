

Kazoo tracks presence subscriptions and those states can be accessed/manipulated via this API.

There are three main ways to access presence information:

* Devices
* Users
* Arbitrary extensions

#### Devices

This API will use the `presence_id' of the device, if present; otherwise it will use the SIP username of the device.

##### POST to reset presence state

    curl -v -X POST http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/presence -d '{"data":{"reset":true}}'

#### Users

This API will use the `presence_id` of the user is applicable; otherwise it will reset all the user's devices' states

##### POST to reset presence state

    curl -v -X POST http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/presence -d '{"data":{"reset":true}}'

#### Arbitrary extensions

Sometimes folks subscribe for parking slots or other values that aren't represented in the Kazoo REST API.

##### POST to reset presence state

    curl -v -X POST http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/presence/{EXTENSION} -d '{"data":{"reset":true}'

Where `{EXTENSION}` could be `*3101`, `110011`, or whatever other extensions are allowed.

### Search

It is possible to search/list all subscriptions for an account:

    curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER_URL}:8000/v2/accounts/{ACCOUNT_ID}/presence
    {"auth_token": "{AUTH_TOKEN}",
     "data": {
         "subscriptions": {
             "{EXTENSION}": {
                 "dialog": {
                     "{CALL_ID}": {
                         "expires": 1820,
                         "from": "{SIP_USERNAME}@{ACCOUNT_REALM}",
                         "notify": {
                             "body": "undefined",
                             "reply": 0,
                             "sequence": 0
                         },
                         "stalker": "BLF-kamailio.2600hz.com",
                         "timestamp": 63606201099,
                         "version": 1
                     }
                 }
             },
             "{SIP_USERNAME}": {
                 "dialog": {
                     "{CALL_ID}": {
                         "expires": 1820,
                         "from": "{SIP_USERNAME}@{ACCOUNT_REALM}",
                         "notify": {
                             "body": "undefined",
                             "reply": 0,
                             "sequence": 0
                         },
                         "stalker": "BLF-kamailio.2600hz.com",
                         "timestamp": 63606201394,
                         "version": 1
                     }
                 }
             }
         }
     },
     "request_id": "{REQUEST_ID}",
     "revision": "undefined",
     "status": "success"
    }
