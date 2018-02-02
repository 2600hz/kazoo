# Presence

## About Presence

Kazoo tracks presence subscriptions and those states can be accessed/manipulated via this API.

There are three main ways to access presence information:

* Devices
* Users
* Arbitrary extensions

## List Subscriptions

> GET /v2/accounts/{ACCOUNT_ID}/presence

It is possible to search/list all subscriptions for an account:

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/presence
```

```json
{
     "auth_token": "{AUTH_TOKEN}",
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
     "revision": "{REVISION}",
     "status": "success"
}
```

## Reset presence state

Sometimes folks subscribe for parking slots or other values that are not represented in the Kazoo REST API.

> POST /v2/accounts/{ACCOUNT_ID}/presence/{EXTENSION}

Where `{EXTENSION}` could be `*3101`, `110011`, or whatever other extensions are allowed.

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"action": "reset"}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/presence/{EXTENSION}
```

## Devices

This API will use the `presence_id` of the device, if present; otherwise it will use the SIP user name of the device.

### Post To Reset Presence State

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"action":"reset"}} \
    'http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/presence'
```

### Post To Update Presence State

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"action":"set","state":"{PRESENCE_STATE}"}}' \
    'http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/presence'
```

## Users

This API will use the `presence_id` of the user is applicable; otherwise it will use all the user's devices' states.

### Post To Reset Presence State

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"action":"reset"}}' \
    'http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/presence'
```

### Post To Update Presence State


```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"action":"reset","state":"{PRESENCE_STATE}"}}' \
    'http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/presence'
```
