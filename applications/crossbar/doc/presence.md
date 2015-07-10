/*
Section: Crossbar
Title: Presence
Language: en-US
Version: 3.22
*/

Kazoo tracks presence subscriptions and those states can be accessed/manipulated via this API.

There are three main ways to access presence information:

* Devices
* Users
* Arbitrary extensions

## Devices

This API will use the `presence_id' of the device, if present; otherwise it will use the SIP username of the device.

### GET presence state

    curl -v -X GET http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/presence

### POST to reset presence state

    curl -v -X POST http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/presence -d '{"data":{"reset":true}}'

## Users

This API will use the `presence_id` of the user is applicable; otherwise it will reset all the user's devices' states

### GET presence state

    curl -v -X GET http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/presence

### POST to reset presence state

    curl -v -X POST http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/presence -d '{"data":{"reset":true}}'

## Arbitrary extensions

Sometimes folks subscribe for parking slots or other values that aren't represented in the Kazoo REST API.

### GET presence state

    curl -v -X GET http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/presence/{EXTENSION}

Where `{EXTENSION}` could be `*3101`, `110011`, or whatever other extensions are allowed.

### POST to reset presence state

    curl -v -X POST http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/presence/{EXTENSION} -d '{"data":{"reset":true}'
