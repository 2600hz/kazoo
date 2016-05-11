### Voicemail Boxes

#### About Voicemail Boxes

Voicemail boxes store messages, recorded from the caller, for the voicemail box owner to listen to at a later time.

#### Differences between version 1 and 2 of Voicemail box

As of Kazoo 4.0 all new voicemail messages will be stored in the account MODbs.

Regarding this change the v2 vmboxes API will ___no longer___ returns the messages array. For compatiblity issues v1 vmboxes API tries to return the messages array along the vmboxes API response, but because of how things work behind this moving to MODBs, manipulation the messages array on the v1 is ___strongly___ discuraged.

The existing messages API _should be_ used to manage messages in a voicemail box for _both_ versions.

For more information about voicemail changes see documentation for kazoo_voicemail.

#### List all account's voicemail boxes

List a summary of voicemail boxes in an account.

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes
```

##### Response

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "id": "3a63acc3694ba189947235ae4727941b",
            "name": "VMBox 0",
            "mailbox": "3000",
            "owner_id": "f1d98a5df729f95cd208ee9430e3b21b",
            "messages": 4
        }
    ],
    "revision": "97fff1929029c3d8ed4a62aa276413f1",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Create a new voicemail box

> PUT /v2/accounts/{ACCOUNT_ID}/vmboxes

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {"name": "VMBox 0", "require_pin": true, "is_setup": false, "pin": "0000", "mailbox": "3000", "timezone": "America/Los_Angeles", "check_if_owner": true, "delete_after_notify": false, "not_configurable": false, "notify_email_addresses": [], "save_after_notify": false, "skip_greeting": false, "skip_instructions": false, "owner_id": "f1d98a5df729f95cd208ee9430e3b21b", "media":{}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes
```

##### Response

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "name": "VMBox 0",
        "require_pin": true,
        "is_setup": false,
        "pin": "0000",
        "mailbox": "3000",
        "timezone": "America/Los_Angeles",
        "check_if_owner": true,
        "delete_after_notify": false,
        "not_configurable": false,
        "notify_email_addresses": [],
        "save_after_notify": false,
        "skip_greeting": false,
        "skip_instructions": false,
        "id": "3a63acc3694ba189947235ae4727941b",
        "owner_id": "f1d98a5df729f95cd208ee9430e3b21b",
        "media": {}
    },
    "revision": "27-d5b3672c6e21e23d7bf206ac516df5c0",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Remove a voicemail box

> DELETE /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}
```

##### Response

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "name": "VMBox 0",
        "require_pin": true,
        "is_setup": false,
        "pin": "0000",
        "mailbox": "3000",
        "timezone": "America/Los_Angeles",
        "check_if_owner": true,
        "delete_after_notify": false,
        "not_configurable": false,
        "notify_email_addresses": [],
        "save_after_notify": false,
        "skip_greeting": false,
        "skip_instructions": false,
        "id": "3a63acc3694ba189947235ae4727941b",
        "owner_id": "f1d98a5df729f95cd208ee9430e3b21b",
        "media": {}
    },
    "revision": "27-d5b3672c6e21e23d7bf206ac516df5c0",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Fetch a voicemail box

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}
```

##### Response

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "name": "VMBox 0",
        "require_pin": true,
        "is_setup": false,
        "pin": "0000",
        "mailbox": "3000",
        "timezone": "America/Los_Angeles",
        "check_if_owner": true,
        "delete_after_notify": false,
        "not_configurable": false,
        "notify_email_addresses": [],
        "save_after_notify": false,
        "skip_greeting": false,
        "skip_instructions": false,
        "id": "3a63acc3694ba189947235ae4727941b",
        "owner_id": "f1d98a5df729f95cd208ee9430e3b21b",
        "media": {}
    },
    "revision": "27-d5b3672c6e21e23d7bf206ac516df5c0",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Patch a voicemail box

> PATCH /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"some_key":"some_value"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}
```

##### Response

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "name": "VMBox 0",
        "require_pin": true,
        "is_setup": false,
        "pin": "0000",
        "mailbox": "3000",
        "timezone": "America/Los_Angeles",
        "check_if_owner": true,
        "delete_after_notify": false,
        "not_configurable": false,
        "notify_email_addresses": [],
        "save_after_notify": false,
        "skip_greeting": false,
        "skip_instructions": false,
        "id": "3a63acc3694ba189947235ae4727941b",
        "owner_id": "f1d98a5df729f95cd208ee9430e3b21b",
        "media": {},
        {"some_key":"some_value"}
    },
    "revision": "27-d5b3672c6e21e23d7bf206ac516df5c0",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Change a voicemail box's settings

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {"name": "VMBox 0", "require_pin": true, "is_setup": false, "pin": "0000", "mailbox": "3000", "timezone": "America/Los_Angeles", "check_if_owner": true, "delete_after_notify": false, "not_configurable": false, "notify_email_addresses": [], "save_after_notify": false, "skip_greeting": false, "skip_instructions": false, "owner_id": "f1d98a5df729f95cd208ee9430e3b21b", "media":{}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes \
```

##### Response

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "name": "VMBox 0",
        "require_pin": true,
        "is_setup": false,
        "pin": "0000",
        "mailbox": "3000",
        "timezone": "America/Los_Angeles",
        "check_if_owner": true,
        "delete_after_notify": false,
        "not_configurable": false,
        "notify_email_addresses": [],
        "save_after_notify": false,
        "skip_greeting": false,
        "skip_instructions": false,
        "id": "3a63acc3694ba189947235ae4727941b",
        "owner_id": "f1d98a5df729f95cd208ee9430e3b21b",
        "media": {}
    },
    "revision": "27-d5b3672c6e21e23d7bf206ac516df5c0",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Remove all or a list of messages from a voicemail box

> DELETE /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages

Deleting all message is easy, just use `DELETE` method on message API endpoint to delete all account's messages.

Optional payload for deleting a group of messages:

* One can specify a `folder` in the payload of a request to delete all messages in that folder(e.g. `{"folder": "saved"}` to delete all saved messages)

**Note:** If you didn't move voicemail messages to the new format already, messages that are in old format will be moved to the new MODB format, which will cause their message id(media id) to change to the new format.

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages
```

#### Fetch all messages for a voicemail box

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages

```shell
curl curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages
```

##### Response

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "timestamp": 63630058722,
            "from": "1001@aeac33.sip.2600hz.com",
            "to": "1000@aeac33.sip.2600hz.com",
            "caller_id_number": "1001",
            "caller_id_name": "userb userb",
            "call_id": "79959ZDNmM2I5ZTliMzA0NzA4N2FjNjlmODA5OWVkZjUxZWU",
            "folder": "new",
            "length": 3140,
            "media_id": "201605-6aadef09f6fcf5fd8bcdfca312e923ba"
        },
        {
            "timestamp": 63630058413,
            "from": "1002@aeac33.sip.2600hz.com",
            "to": "1000@aeac33.sip.2600hz.com",
            "caller_id_number": "1002",
            "caller_id_name": "userd userd",
            "call_id": "79959MmNiMmJiMTIxODhjZjk0ZDhmOGNkMjJkN2MwNGQyNWY",
            "folder": "new",
            "length": 5500,
            "media_id": "201605-f0c3c16551a5ff7b5753a381892e2e01"
        }
    ],
    "revision": "undefined",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Remove a message from the voicemail box

> DELETE /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/{MSG_ID}

**Note:** If you didn't move voicemail messages to the new format already, messages that are in old format will be moved to the new MODB format, which will cause their message id(media id) to change to the new format.

```shell
curl curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/201605-6aadef09f6fcf5fd8bcdfca312e923ba
```

##### Response

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "timestamp": 63630058722,
        "from": "1001@aeac33.sip.2600hz.com",
        "to": "1000@aeac33.sip.2600hz.com",
        "caller_id_number": "1001",
        "caller_id_name": "userb userb",
        "call_id": "79959ZDNmM2I5ZTliMzA0NzA4N2FjNjlmODA5OWVkZjUxZWU",
        "folder": "new",
        "length": 3140,
        "media_id": "201605-6aadef09f6fcf5fd8bcdfca312e923ba"
    },
    "revision": "undefined",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Fetch a message from the voicemail box

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/{MSG_ID}

```shell
curl curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/201605-6aadef09f6fcf5fd8bcdfca312e923ba
```

**Note:** If message doesn't have a folder assign to it by any chance, it will be set to `new` by this method. Please also refer to the note for change the folder of a message regards of possible change of message id.

##### Response

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "timestamp": 63630058722,
        "from": "1001@aeac33.sip.2600hz.com",
        "to": "1000@aeac33.sip.2600hz.com",
        "caller_id_number": "1001",
        "caller_id_name": "userb userb",
        "call_id": "79959ZDNmM2I5ZTliMzA0NzA4N2FjNjlmODA5OWVkZjUxZWU",
        "folder": "new",
        "length": 3140,
        "media_id": "201605-6aadef09f6fcf5fd8bcdfca312e923ba"
    },
    "revision": "undefined",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Change the folder of a message

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/{MSG_ID}

**Note:** If you didn't move voicemail messages to the new format already, messages that are in old format will be moved to the new MODB format, which will cause their message id(media id) to change to the new format.

```shell
curl curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"folder": "saved"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/201605-6aadef09f6fcf5fd8bcdfca312e923ba
```

##### Response

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "timestamp": 63630058722,
        "from": "1001@aeac33.sip.2600hz.com",
        "to": "1000@aeac33.sip.2600hz.com",
        "caller_id_number": "1001",
        "caller_id_name": "userb userb",
        "call_id": "79959ZDNmM2I5ZTliMzA0NzA4N2FjNjlmODA5OWVkZjUxZWU",
        "folder": "saved",
        "length": 3140,
        "media_id": "201605-6aadef09f6fcf5fd8bcdfca312e923ba"
    },
    "revision": "undefined",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Fetch the raw audio of the message

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/{MSG_ID}/raw

**Note:** If message doesn't have a folder assign to it by any chance, it will be set to `new` by this method. Please also refer to the note for change the folder of a message regards of possible change of message id.

```shell
curl curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VMBOX_ID}/messages/201605-6aadef09f6fcf5fd8bcdfca312e923ba/raw
```
