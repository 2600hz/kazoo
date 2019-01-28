# Metaflows

## About Metaflows

Metaflows allow functionality to be executed on an in-progress call, triggered by DTMFs from the caller/callee or an API call.

For instance, a callee could setup a metaflow on their user doc such that when they receive a call, they can press `*9` to initiate a recording of the call.

#### Schema

Actions applied to a call outside of the normal callflow, initiated by the caller(s)



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`binding_digit` | What DTMF will trigger the collection and analysis of the subsequent DTMF sequence | `string('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' | '*' | '#')` | `*` | `false` |  
`digit_timeout` | How long to wait between DTMF presses before processing the collected sequence (milliseconds) | `integer()` |   | `false` |  
`listen_on` | Which leg(s) of the call to listen for DTMF | `string('both' | 'self' | 'peer')` |   | `false` |  
`numbers./^[0-9]+$/` | A metaflow node defines a module to execute, data to provide to that module, and one or more children to branch to | [#/definitions/metaflow](#metaflow) |   | `false` |  
`numbers` | A list of static numbers with their flows | `object()` |   | `false` |  
`patterns./.+/` | A metaflow node defines a module to execute, data to provide to that module, and one or more children to branch to | [#/definitions/metaflow](#metaflow) |   | `false` |  
`patterns` | A list of patterns with their flows | `object()` |   | `false` |  

### metaflow

A metaflow node defines a module to execute, data to provide to that module, and one or more children to branch to


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`children./.+/` | A metaflow node defines a module to execute, data to provide to that module, and one or more children to branch to | [#/definitions/metaflow](#metaflow) |   | `false` |  
`children` | Children metaflows | `object()` |   | `false` |  
`data` | The data/arguments of the metaflow module | `object()` | `{}` | `false` |  
`module` | The name of the metaflow module to execute at this node | `string(1..64)` |   | `true` |  



## Fetch account-level metaflows

These are available to all users and devices within the account

> GET /v2/accounts/{ACCOUNT_ID}/metaflows

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/metaflows
```

### No metaflows assigned

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {},
    "node": "{NODE_HASH}",
    "request_id": "{REQUEST_ID}",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "4.2.42"
}
```

## Set metaflows on the account

> POST /v2/accounts/{ACCOUNT_ID}/metaflows

```shell
curl -v -X POST \
    -H "X-Auth-Token: $AUTH_TOKEN" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/metaflows \
    -d '{"data":{"binding_digit":"*", "patterns":{"2(\\d+)":{"module":"transfer", "data":{"takeback_dtmf":"*1"}}}}}'
```
```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "binding_digit": "*",
        "patterns": {
            "2(\\d+)": {
                "data": {
                    "captures": [
                        "no_match"
                    ],
                    "takeback_dtmf": "*1",
                    "transfer_type": "blind"
                },
                "module": "transfer"
            }
        }
    },
    "node": "{NODE_HASH}",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "4.2.42"
}
```

In the above example, the `binding_digit` is `*` which, when pressed on the phone's keypad, tells Kazoo that a metaflow is starting. `2(\\d+)` tells Kazoo to look for a 2 and then capture one or more digits.

Thus `*21234` would instruct Kazoo to blind-transfer the caller to extension `1234`.

## Remove metaflows from the account

> DELETE /v2/accounts/{ACCOUNT_ID}/metaflows

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/metaflows
```
```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {},
    "node": "{NODE_HASH}",
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "4.2.42"
}
```

## Users and devices

If you want to assign metaflows to a particular user or device, the API commands are the same except you need to include the user or device in the URI:

- Account: `/v2/accounts/{ACCOUNT_ID}/metaflows`
- User: `/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/metaflows`
- Device: `/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/metaflows`

## Metaflow actions

To see what actions can be set in a metaflow, look at the relevant documentation in [Konami](/applications/konami/doc/README.md)
