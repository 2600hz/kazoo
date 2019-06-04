# Voicemail

## About Voicemail

Voicemail boxes store messages, recorded from the caller, for the voicemail box owner to listen to at a later time.

### Differences between Kazoo version 3.x and 4.x

As of Kazoo 4.0 all new voicemail messages will be stored in the account MODbs.

Regarding this change voicemail API will ___no longer___ returns the messages array when fetching mailbox settings.
The existing `/messages` API _should be_ used to manage messages in a voicemail box.

For more information about voicemail changes see documentation for kazoo_voicemail.

#### Schema

Schema for a voicemail box



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`check_if_owner` | Determines if when the user calls their own voicemail they should be prompted to sign in | `boolean()` | `true` | `false` | `supported`
`delete_after_notify` | Move the voicemail to delete folder after the notification has been sent | `boolean()` | `false` | `false` | `supported`
`is_setup` | Determines if the user has completed the initial configuration | `boolean()` | `false` | `false` | `supported`
`is_voicemail_ff_rw_enabled` | callflow allow fastforward and rewind during voicemail message playback | `boolean()` | `false` | `false` |  
`mailbox` | The voicemail box number | `string(1..30)` |   | `true` | `supported`
`media.unavailable` | The ID of a media object that should be used as the unavailable greeting | `string(32)` |   | `false` | `supported`
`media` | The media (prompt) parameters | `object()` | `{}` | `false` | `supported`
`media_extension` | Voicemail audio format | `string('mp3' | 'mp4' | 'wav')` | `mp3` | `false` | `supported`
`name` | A friendly name for the voicemail box | `string(1..128)` |   | `true` | `supported`
`not_configurable` | Determines if the user can configure this voicemail. | `boolean()` | `false` | `false` | `supported`
`notify.callback` | Schema for a callback options | [#/definitions/notify.callback](#notifycallback) |   | `false` |  
`notify` |   | `object()` |   | `false` | `supported`
`notify_email_addresses.[]` |   | `string()` |   | `false` | `supported`
`notify_email_addresses` | List of email addresses to send notifications to (in addition to owner's email, if any) | `array(string())` | `[]` | `false` | `supported`
`oldest_message_first` | Play older voicemail messages before new ones | `boolean()` | `false` | `false` | `supported`
`owner_id` | The ID of the user object that 'owns' the voicemail box | `string(32)` |   | `false` | `supported`
`pin` | The pin number for the voicemail box | `string(4..15)` |   | `false` | `supported`
`require_pin` | Determines if a pin is required to check the voicemail from the users devices | `boolean()` | `false` | `false` | `supported`
`save_after_notify` | Move the voicemail to save folder after the notification has been sent (This setting will override delete_after_notify) | `boolean()` | `false` | `false` | `supported`
`seek_duration_ms` | callflow fastforward and rewind seek duration | `integer()` | `10000` | `false` |  
`skip_envelope` | Determines if the envelope should be skipped | `boolean()` | `false` | `false` | `beta`
`skip_greeting` | Determines if the greeting should be skipped | `boolean()` | `false` | `false` | `supported`
`skip_instructions` | Determines if the instructions after the greeting and prior to composing a message should be played | `boolean()` | `false` | `false` | `supported`
`timezone` | The default timezone | `string(5..32)` |   | `false` | `supported`

### notify.callback

Schema for a callback options


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`attempts` | How many attempts without answer will system do | `integer()` |   | `false` |  
`disabled` | Determines if the system will call to callback number | `boolean()` |   | `false` |  
`interval_s` | How long will system wait between call back notification attempts | `integer()` |   | `false` |  
`number` | Number for callback notifications about new messages | `string()` |   | `false` |  
`schedule` | Schedules interval between callbacks | `array(integer())` |   | `false` |  
`timeout_s` | How long will system wait for answer to callback | `integer()` |   | `false` |  



## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes
```

**Response**

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

## Create a new voicemail box

> PUT /v2/accounts/{ACCOUNT_ID}/vmboxes

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {"name": "VMBox 0", "require_pin": true, "is_setup": false, "pin": "0000", "mailbox": "3000", "timezone": "America/Los_Angeles", "check_if_owner": true, "delete_after_notify": false, "not_configurable": false, "notify_email_addresses": [], "save_after_notify": false, "skip_greeting": false, "skip_instructions": false, "owner_id": "f1d98a5df729f95cd208ee9430e3b21b", "media":{}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes
```

**Response**

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

## list all voicemail messages on an account

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages?paginate=true
```

**Response**

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

## Remove a voicemail box

> DELETE /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}
```

**Response**

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

## Fetch a voicemail box

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}
```

**Response**

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

## Patch a voicemail box

> PATCH /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"some_key":"some_value"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}
```

**Response**

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

## Change a voicemail box's settings

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {"name": "VMBox 0", "require_pin": true, "is_setup": false, "pin": "0000", "mailbox": "3000", "timezone": "America/Los_Angeles", "check_if_owner": true, "delete_after_notify": false, "not_configurable": false, "notify_email_addresses": [], "save_after_notify": false, "skip_greeting": false, "skip_instructions": false, "owner_id": "f1d98a5df729f95cd208ee9430e3b21b", "media":{}}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes \
```

**Response**

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

## Create a new voicemail message

There are two methods for creating a new voicemail message - they differ in how you attach the media file.

In the first method, you can create a voicemail document first in one request and then put the media file into the document with a second request using `/messages/{VM_MSG_ID}` API endpoint.

> PUT /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d {"data":{"caller_id_name":"someone","caller_id_number":"6001","folder":"new","from":"someone@farfaraway.com"}} \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages
```

**Response**

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
            "timestamp": 63630058722,
            "from": "someone@farfaraway.com",
            "to": "1000@sip.somewhere.com",
            "caller_id_number": "6001",
            "caller_id_name": "someone",
            "call_id": "79959ZDNmM2I5ZTliMzA0NzA4N2FjNjlmODA5OWVkZjUxZWU",
            "folder": "new",
            "length": 3140,
            "media_id": "201605-fadnew0mf6fcfgfd8bcdfca312e924bq"
    },
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

And then you can use PUT method on `/messages/201605-fadnew0mf6fcfgfd8bcdfca312e924bq` to add the media to file (see PUT method for a message below).

In the second method, you can use a single PUT request and send a multipart content-type to add both the JSON metadata about the message and the media file itself, in a single request.

```shell
curl -v -X PUT \
     -H "Content-Type: multipart/mixed" \
     -F "content=@message.json; type=application/json" \
     -F "content=@voice.mp3; type=audio/mp3" \
     -H 'X-Auth-Token: {AUTH_TOKEN}' \
     http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/messages
```

The response is same as above.

## Remove all or a list of messages from a voicemail box

> DELETE /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages

Deleting all message is easy, just use `DELETE` method on message API endpoint to delete all account's messages.

Optional payload for deleting a group of messages:

* One can apply a filter to delete all messages in a particular folder(e.g. new or saved) by adding a query string `?folder=saved` to the URL or set it in the payload as `{"data": {"folder": "saved"}}`
* Or providing an array of message ids, e.g `{"data": {"messages": [MSG_ID1, MSG_ID2, ...]}}`.

!!! note
    If you didn't move voicemail messages to the new format already, messages that are in old format will be moved to the new MODB format, which will cause their message id to change to the new format.

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages
```

**Response**

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "succeeded": ["201605-6aadef09f6fcf5fd8bcdfca312e923ba"],
        "failed": [{"201605-49be0985ea3a33046f8073083517d27b":"not_found"}]
    },
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```

## Fetch all messages for a voicemail box

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages
```

**Response**

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

## Change a list of messages

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages

Provide an array of message ids, e.g `{"data": {"messages": ["MSG_ID1", "MSG_ID2", "MSG_ID3"]}}` you can do following change operations on them. It will return two objects: the first is all the message ids that were successfully changed and the second one is those that failed with the reasons.

* **Change the folder of messages:** set the folder that messages should move to (e.g. new or saved) by adding a query string `?folder=saved` to the URL or set it in the payload as `{"data": {"folder": "saved"}}`.

* **Move messages to another voicemail box:** set the destination voicemail box ID in payload like: `{"data": {"source_id": "{NEW_VM_BOX_ID}"}}`

* **Copy messages to a single or a list of voicemail boxes** set the destination voicemail box ID in payload like: `{"data": {"source_id": ["{NEW_VM_BOX_ID}"]}}`

!!! note
    If you didn't move voicemail messages to the new format already, messages that are in old format will be moved to the new MODB format, which will cause their message id to change to the new format.

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data": {"folder": "saved", "messages": ["MSG_ID1", "MSG_ID2", "MSG_ID3"]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages
```

**Response**

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
        "succeeded": ["201605-6aadef09f6fcf5fd8bcdfca312e923ba"],
        "failed": [{"201605-49be0985ea3a33046f8073083517d27b":"not_found"}]
    },
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```


## Fetch the raw audio of a list of messages as a ZIP file

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/raw

You can provide a list of voicemail message ID in the payload and get raw audio of them in a single ZIP file.

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -H "Accept: application/zip" \
    -d '{"data": {"messages": ["MSG_ID1", "MSG_ID2", "MSG_ID3"]}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/raw
```

## Remove a message from the voicemail box

> DELETE /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}

!!! note
    If you didn't move voicemail messages to the new format already, messages that are in old format will be moved to the new MODB format, which will cause their message id to change to the new format.

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/201605-6aadef09f6fcf5fd8bcdfca312e923ba
```

**Response**

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

## Fetch a message from the voicemail box

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/201605-6aadef09f6fcf5fd8bcdfca312e923ba
```

!!! note
    If message doesn't have a folder assign to it by any chance, it will be set to `new` by this method. Please also refer to the note for change the folder of a message regards of possible change of message id.

**Response**

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

## Change a message

* **Change the folder of a message:** set the folder that message should move to (e.g. new or saved) by adding a query string `?folder=saved` to the URL or set it in the payload as `{"data": {"folder": "saved"}}`.

* **Move a message to another voicemail box:** set the destination voicemail box ID in payload like: `{"data": {"source_id": "{NEW_VM_BOX_ID}"}}`

* **Copy a message to a single or a list of voicemail boxes** set the destination voicemail box ID in payload like: `{"data": {"source_id": ["{NEW_VM_BOX_ID}"]}}`

> POST /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}

!!! note
    If you didn't move voicemail messages to the new format already, messages that are in old format will be moved to the new MODB format, which will cause their message id to change to the new format.

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"folder": "saved"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/201605-6aadef09f6fcf5fd8bcdfca312e923ba
```

**Response**

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

## Fetch the raw audio of the message

> GET /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}/raw

!!! note
    If message doesn't have a folder assign to it by any chance, it will be set to `new` by this method. Please also refer to the note for change the folder of a message regards of possible change of message id.

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/201605-6aadef09f6fcf5fd8bcdfca312e923ba/raw
```

## Add a new voicemail media file to a message

If you added a message based on the first method mentioned above (using PUT method on `/messages`), you can use this to upload the media file for the created message.

!!! note
    If there's already a media file attachment inside the message document it will be removed and replaced with the new media file!

> PUT /v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/{VM_MSG_ID}/raw

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: multipart/mixed" \
    -F "content=@voice.mp3; type=audio/mp3" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/vmboxes/{VM_BOX_ID}/messages/201605-fadnew0mf6fcfgfd8bcdfca312e924bq/raw
```

**Response**

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": {
            "timestamp": 63630058722,
            "from": "someone@farfaraway.com",
            "to": "1000@sip.somewhere.com",
            "caller_id_number": "6001",
            "caller_id_name": "someone",
            "call_id": "79959ZDNmM2I5ZTliMzA0NzA4N2FjNjlmODA5OWVkZjUxZWU",
            "folder": "new",
            "length": 3140,
            "media_id": "201605-fadnew0mf6fcfgfd8bcdfca312e924bq"
    },
    "revision": "{REVISION}",
    "request_id": "{REQUEST_ID}",
    "status": "success"
}
```
