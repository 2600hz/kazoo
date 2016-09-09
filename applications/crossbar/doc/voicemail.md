### Voicemail Boxes

#### About Voicemail Boxes

Voicemail boxes store messages, recorded from the caller, for the voicemail box owner to listen to at a later time.

#### Differences between version 1 and 2 of Voicemail box

As of Kazoo 4.0 all new voicemail messages will be stored in the account MODbs.

Regarding this change the v2 vmboxes API will ___no longer___ returns the messages array. For compatibility issues v1 vmboxes API tries to return the messages array along the vmboxes API response, but because of how things work behind this moving to MODBs, manipulation the messages array on the v1 is ___strongly___ discouraged.

The existing messages API _should be_ used to manage messages in a voicemail box for _both_ versions.

For more information about voicemail changes see documentation for kazoo_voicemail.

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`check_if_owner` | Determines if when the user calls their own voicemail they should be prompted to sign in | `boolean` | `true` | `false`
`delete_after_notify` | Delete the voicemail after the notification has been sent | `boolean` | `false` | `false`
`is_setup` | Determines if the user has completed the initial configuration | `boolean` | `false` | `false`
`mailbox` | The voicemail box number | `string(1..30)` |   | `true`
`media` | The media (prompt) parameters | `object` | `{}` | `false`
`media.unavailable` | The ID of a media object that should be used as the unavailable greeting | `string(32)` |   | `false`
`name` | A friendly name for the voicemail box | `string(1..128)` |   | `true`
`not_configurable` | Determines if the user can configure this voicemail. | `boolean` | `false` | `false`
`notify` |   | `object` |   | `false`
`notify.callback` |   | `#/definitions/notify.callback` |   | `false`
`notify_email_addresses` | List of email addresses to send notifications to (in addition to owner's email, if any) | `array(string)` | `[]` | `false`
`notify_email_addresses.[]` |   | `string` |   | `false`
`owner_id` | The ID of the user object that 'owns' the voicemail box | `string(32)` |   | `false`
`pin` | The pin number for the voicemail box | `string(4..15)` |   | `false`
`require_pin` | Determines if a pin is required to check the voicemail from the users devices | `boolean` | `false` | `false`
`save_after_notify` | Save the voicemail after the notification has been sent | `boolean` | `false` | `false`
`skip_greeting` | Determines if the greeting should be skipped | `boolean` | `false` | `false`
`skip_instructions` | Determines if the instructions after the greeting and prior to composing a message should be played | `boolean` | `false` | `false`
`timezone` | The default timezone | `string(5..32)` |   | `false`

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
    "revision": "{REVISION}",
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
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### list all voicemail messages on an account

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/messages

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages?paginate=true
```

##### Response

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "0e820108c0f4ca391500f3be1b02bdfa": {
                "timestamp": 63630058722,
                "from": "1001@aeac33.sip.2600hz.com",
                "to": "1000@aeac33.sip.2600hz.com",
                "caller_id_number": "1001",
                "caller_id_name": "userb userb",
                "call_id": "79959ZDNmM2I5ZTliMzA0NzA4N2FjNjlmODA5OWVkZjUxZWU",
                "folder": "new",
                "length": 3140,
                "media_id": "201605-6aadef09f6fcf5fd8bcdfca312e923ba"
            }
        },
        {
            "0e820108c0f4ca391500f3be1b02bdfa": {
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
        }
    ],
    "next_start_key": [],
    "page_size": 50,
    "revision": "{REVERSION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Remove a voicemail box

> DELETE /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}
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
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Fetch a voicemail box

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}
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
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Patch a voicemail box

> PATCH /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"some_key":"some_value"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}
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
        "some_key": "some_value"
    },
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Change a voicemail box's settings

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}

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
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Remove all or a list of messages from a voicemail box

> DELETE /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages

Deleting all message is easy, just use `DELETE` method on message API endpoint to delete all account's messages.

Optional payload for deleting a group of messages:

* One can apply a filter to delete all messages in a particular folder(e.g. new or saved) by adding a query string `?folder=saved` to the URL or set it in the payload as `{"data": {"folder": "saved"}}`
* Or providing an array of message ids, e.g `{"data": {"messages": [MSG_ID1, MSG_ID2, ...]}}`.

**Note:** If you didn't move voicemail messages to the new format already, messages that are in old format will be moved to the new MODB format, which will cause their message id to change to the new format.

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages
```

##### Response

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "suceeded": ["201605-6aadef09f6fcf5fd8bcdfca312e923ba"],
        "failed": [{"201605-49be0985ea3a33046f8073083517d27b":"not_found"}]
    },
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Fetch all messages for a voicemail box

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages
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
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Change a list of messages

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages

Provide an array of message ids, e.g `{"data": {"messages": ["MSG_ID1", "MSG_ID2", "MSG_ID3"]}}` you can do following change operations on them. It will return two objects: the first is all the message ids that were successfully changed and the second one is those that failed with the reasons.

* **Change the folder of messages:** set the folder that messages should move to (e.g. new or saved) by adding a query string `?folder=saved` to the URL or set it in the payload as `{"data": {"folder": "saved"}}`.

* **Move messages to another voicemail box:** set the destination voicemail box ID in payload like: `{"data": {"source_id": "{NEW_VM_BOX_ID}"}}`

* **Copy messages to a single or a list of voicemail boxes** set the destination voicemail box ID in payload like: `{"data": {"source_id": ["{NEW_VM_BOX_ID}"]}}`

**Note:** If you didn't move voicemail messages to the new format already, messages that are in old format will be moved to the new MODB format, which will cause their message id to change to the new format.

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {"folder": "saved", "messages": ["MSG_ID1", "MSG_ID2", "MSG_ID3"]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages
```

##### Response

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "suceeded": ["201605-6aadef09f6fcf5fd8bcdfca312e923ba"],
        "failed": [{"201605-49be0985ea3a33046f8073083517d27b":"not_found"}]
    },
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```


#### Fetch the raw audio of a list of messages as a ZIP file

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/raw

You can provide a list of voicemail message ID in the playload and get raw audio of them in a single ZIP file.

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -H "Accept: application/zip" \
    -d '{"data": {"messages": ["MSG_ID1", "MSG_ID2", "MSG_ID3"]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/raw
```

#### Remove a message from the voicemail box

> DELETE /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}

**Note:** If you didn't move voicemail messages to the new format already, messages that are in old format will be moved to the new MODB format, which will cause their message id to change to the new format.

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/201605-6aadef09f6fcf5fd8bcdfca312e923ba
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
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Fetch a message from the voicemail box

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/201605-6aadef09f6fcf5fd8bcdfca312e923ba
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
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Change a message

* **Change the folder of a message:** set the folder that message should move to (e.g. new or saved) by adding a query string `?folder=saved` to the URL or set it in the payload as `{"data": {"folder": "saved"}}`.

* **Move a message to another voicemail box:** set the destination voicemail box ID in payload like: `{"data": {"source_id": "{NEW_VM_BOX_ID}"}}`

* **Copy messages to a single or a list of voicemail boxes** set the destination voicemail box ID in payload like: `{"data": {"source_id": ["{NEW_VM_BOX_ID}"]}}`

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}

**Note:** If you didn't move voicemail messages to the new format already, messages that are in old format will be moved to the new MODB format, which will cause their message id to change to the new format.

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"folder": "saved"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/201605-6aadef09f6fcf5fd8bcdfca312e923ba
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
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

#### Fetch the raw audio of the message

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}/raw

**Note:** If message doesn't have a folder assign to it by any chance, it will be set to `new` by this method. Please also refer to the note for change the folder of a message regards of possible change of message id.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/201605-6aadef09f6fcf5fd8bcdfca312e923ba/raw
```
