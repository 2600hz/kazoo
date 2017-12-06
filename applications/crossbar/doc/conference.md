### Conferences

#### About Conferences

Conferences documents are enriched with realtime information, namely: number of members, number of moderators, duration of the conference,
conference locked status. The realtime information is added to conference document under _read_only key (to avoid accident document update).

#### Schema

Schema for conferences



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`bridge_password` | the password used for a conference bridge | `string()` |   | `false`
`bridge_username` | the username used for a conference bridge | `string()` |   | `false`
`caller_controls` | caller controls (config settings) | `string()` |   | `false`
`conference_numbers.[]` |   | `string()` |   | `false`
`conference_numbers` | Defines conference numbers that can be used by members or moderators | `array(string())` | `[]` | `false`
`focus` | This is a read-only property indicating the media server hosting the conference | `string()` |   | `false`
`max_members_media` | Media to play when the conference is full | `string()` |   | `false`
`max_participants` | The maximum number of participants that can join | `integer()` |   | `false`
`member.join_deaf` | Determines if a member will join deaf | `boolean()` | `false` | `false`
`member.join_muted` | Determines if a member will join muted | `boolean()` | `true` | `false`
`member.numbers.[]` |   | `string()` |   | `false`
`member.numbers` | Defines the conference (call in) number(s) for members | `array(string())` | `[]` | `false`
`member.pins.[]` |   | `string()` |   | `false`
`member.pins` | Defines the pin number(s) for members | `array(string())` | `[]` | `false`
`member` | Defines the discovery (call in) properties for a member | `object()` | `{}` | `false`
`moderator.join_deaf` | Determines if a moderator will join deaf | `boolean()` | `false` | `false`
`moderator.join_muted` | Determines if a moderator will join muted | `boolean()` | `false` | `false`
`moderator.numbers.[]` |   | `string()` |   | `false`
`moderator.numbers` | Defines the conference (call in) number(s) for moderators | `array(string())` | `[]` | `false`
`moderator.pins.[]` |   | `string()` |   | `false`
`moderator.pins` | Defines the pin number(s) for moderators | `array(string())` | `[]` | `false`
`moderator` | Defines the discovery (call in) properties for a moderator | `object()` | `{}` | `false`
`moderator_controls` | profile on the switch for controlling the conference as a moderator | `string()` |   | `false`
`name` | A friendly name for the conference | `string(1..128)` |   | `false`
`owner_id` | The user ID who manages this conference | `string(32)` |   | `false`
`play_entry_tone` | Whether to play an entry tone, or the entry tone to play | `boolean() | string()` |   | `false`
`play_exit_tone` | Whether to play an exit tone, or the exit tone to play | `boolean() | string()` |   | `false`
`play_name` | Do we need to announce new conference members? | `boolean()` | `false` | `false`
`play_welcome` | Whether to play the welcome prompt | `boolean()` |   | `false`
`profile` | The XML profile name used to configure the conference | `string()` |   | `false`
`require_moderator` | does the conference require a moderator | `boolean()` |   | `false`
`wait_for_moderator` | should members wait for a moderator before joining the conference | `boolean()` |   | `false`



#### Perform an action on a conference

> PUT /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}

```shell
curl -v -X PUT \
    -d '{"action": "{CONFERENCE_ACTION}", "data": {"ACTION":"DATA"}}' \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}
```

 Action | Description
 ------ | -----------
 `lock` | Lock the conference; no new participants may join
 `unlock` | Unlock the conference; new participants may join
 `dial` | Dial an endpoint (user/device/DID)
 `play` | Play media to the conference (all participants)

##### Dialing an endpoint

Sometimes you want to dial out from a conference to an endpoint (versus waiting for the caller to dial into the conference). Similar to how the `group` callflow works, you can include device and user IDs; unlike groups, you can include DIDs as well (similar to quickcall/click2call).

###### Schema

Schema for conference dial API command



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`caller_id_name` | Caller ID Name to use when dialing out to endpoints | `string()` |   | `false`
`caller_id_number` | Caller ID Number to use when dialing out to endpoints | `string()` |   | `false`
`endpoints.[]` |   | `string()` |   | `true`
`endpoints` |   | `array(string())` |   | `true`
`target_call_id` | Existing UUID to use as a hint for where to start the conference | `string()` |   | `false`
`timeout` | How long to try to reach the endpoint(s) | `integer()` |   | `false`



###### Endpoints

Dial-able endpoints are
1. Devices (by device id)
2. Users (by user id)
3. Phone Numbers
4. SIP URIs (`sip:user@realm`)

Note: Phone numbers will involve some internal legs being generated (loopback legs) to process the number as if it was a call coming in for the desired number. This means billing and limits will be applied just the same as if a user dialed the number from their device.

###### Examples

```json
{
    "action":"dial"
    ,"data":{
        ,"data":{
            "endpoints":["{DEVICE_ID}","{USER_ID}","{NUMBER}","sip:{URI}"],
            "caller_id_name":"Conference XYZ",
            "caller_id_number":"5551212"
        }
    }
}
```

As when making [quickcalls](./quickcall.md), you can include `custom_application_vars`:

```json
{
    "action":"dial"
    ,"data":{
        ,"custom_application_vars":{
            "foo":"bar"
        }
        "data":{
            "endpoints":["{DEVICE_ID}","{USER_ID}","{NUMBER}","sip:{URI}"],
            "caller_id_name":"Conference XYZ",
            "caller_id_number":"5551212"
        }
    }
}
```

You can also include the outbound call id you'd like the leg to use:

```json
{
    "action":"dial"
    ,"data":{
        ,"custom_application_vars":{
            "foo":"bar"
        }
        "data":{
            "endpoints":["{DEVICE_ID}","{USER_ID}","{NUMBER}","sip:{URI}"],
            "caller_id_name":"Conference XYZ",
            "caller_id_number":"5551212",
            "outbound_call_id":"xyz-abc"
        }
    }
}
```

##### Dialing out to a dynamic conference

Sometimes you want to create ad-hoc conferences and put a participant in there. You can `PUT` a conference and the endpoints to dial out to create a temporary conference. The `{CONFERENCE_ID}` you supply will be used to name the conference and any conference schema parameters in the request will be used when creating the conference. For example:

```json
{
    "action":"dial"
    ,"data":{
        "data":{
            "endpoints":["{DEVICE_ID}","{USER_ID}","{NUMBER}","sip:{URI}"],
            "caller_id_name":"Conference XYZ",
            "caller_id_number":"5551212",
            "play_entry_tone": true,
            "play_exit_tone": true,
            "play_name": false
        }
    }
}
```

These properties will be merged into a "default" conference document and then executed the same as if the conference was preconfigured.

##### Playing media to a conference

Playing a media file to everyone in a conference:

```json
{
    action":"play"
    ,"data"{
        "data":{"media_id":"{MEDIA_ID}"}
    }
}
```

`{MEDIA_ID}` can be a pre-uploaded media ID or a URL to fetch media from.

#### Perform an action on participants

> PUT /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants

```shell
curl -v -X PUT \
    -d '{"data": {"action": {PARTICIPANTS_ACTION}}}' \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants
```

 Action | Description
 ------ | -----------
 `mute` | Mute all the participants that are currently unmuted
 `unmute` | Unmute all the participants that are currently muted
 `deaf` | Stop sending conference audio to all participants
 `undeaf` | Start sending conference audio to all participants
 `kick` | Kick all the participants from the conference
 `relate` | Relate two participants

##### Relate participants

 The `relate` action takes a `data` object:

```
{
    "data":{
        "action":"relate"
        ,"data":{
            "participant_id":{ID}
            ,"other_participant":{ID}
            ,"relationship":"{RELATIONSHIP}"
        }
    }
}
```

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`other_participant` | The other participant ID to relate | `string() | integer()` |   | `true`
`participant_id` | The participant ID to relate | `string() | integer()` |   | `true`
`relationship` | The relationship to establish between the two participants | `string('deaf' | 'clear' | 'mute')` | `clear` | `false`

```shell
curl -v -X PUT \
    -H "X-Auth-Token: $AUTH_TOKEN" \
    "http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants" \
    -d'{"data":{"action":"relate","data":{"participant_id":23, "other_participant":24}}}'
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": "relating participants",
    "node": "{NODE}",
    "request_id": "{REQUEST_ID}",
    "revision": "1-100475067fa624422c9a21bd976c7b84",
    "status": "success",
    "timestamp": "{TIMESTAMP}",
    "version": "4.2.2"
}
```

#### Perform an action on participant

> PUT /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants/{PARTICIPANT_ID}

```shell
curl -v -X PUT \
    -d '{"data": {"action": {PARTICIPANT_ACTION}}}' \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants/{PARTICIPANT_ID}
```

 Action | Description
 ------ | -----------
 `mute` | Mute the participant
 `unmute` | Unmute the participant
 `deaf` | Stop sending conference audio to the participant
 `undeaf` | Start sending conference audio to the participant
 `kick` | Kick the participant from the conference
 `play` | Play media to a single participant

##### Playing media to a conference

Playing a media file to everyone in a conference:

```json
{"data"{
    "action":"play",
    "data":{"media_id":"{MEDIA_ID}"}
 }
}
```

`{MEDIA_ID}` can be a pre-uploaded media ID or a URL to fetch media from.

#### List of conferences example

```json
[
    {
        "id": "",
        "name": "",
        "owner_id": "",
        "member": {
            "join_muted": false,
            "join_deaf": false,
            "numbers": [],
            "pins": []
        },
        "moderator": {
            "join_deaf": false,
            "join_muted": false,
            "numbers": [],
            "pins": []
        },
        "members": 0,
        "admins": 0,
        "duration": 0,
        "is_locked": false
    },
    ...
]
```

#### Conference document

```json
{
    "name": "Conf",
    "id": "",
    "owner_id": "",
    "play_entry_tone": true,
    "play_exit_tone": true,
    "play_name": false,
    "conference_numbers": [],
    "member": {
        "join_muted": false,
        "join_deaf": false,
        "numbers": [],
        "pins": []
    },
    "ui_metadata": {
        "ui": "kazoo-ui"
    },
    "moderator": {
        "join_deaf": false,
        "join_muted": false,
        "numbers": [],
        "pins": []
    },
    "_read_only": {
        "members": 0,
        "admins": 0,
        "duration": 0,
        "is_locked": false,
        "participants": [
            {
                "call_id": "",
                "conference_name": "",
                "conference_uuid": "",
                "switch_hostname": "",
                "floor": false,
                "hear": true,
                "speak": true,
                "talking": false,
                "mute_detect": false,
                "participant_id": 1,
                "energy_level": 20,
                "current_energy": 0,
                "video": false,
                "is_moderator": false,
                "join_time": 63635217275,
                "duration": 10
            },
            ...
        ]
    }
}
```

`join_time` is participant's join time as epoch, duration is number of seconds participant participate in conference.

Here we can see values set up for a Member, then for a Moderator.

The last field, `play_entry_tone`, is at the root of the document: meaning this field applies to everyone in the conference.

#### Available fields

* **play_entry_tone** and **play_exit_tone**: can be either a boolean or a non-empty string.
    * `true` means play the default tone when someone joins (or leaves) the conference
    * `false` disables the tone from being played
    * A string like a *tone string* or a *URI to a media file* can be inputed.

#### Actions

Actions are JSON objects in format:

```json
{
    "action": {action}
}
```

#### Conference actions

    lock: lock conference (prevent participants to join)
    unlock: unlock conference (allow everybody to join)

#### Participants actions

    mute/unmute: mute/unmute all participants except moderators
    deaf/undeaf: deaf/undeaf all participants except moderators
    kick: kick every participant out

#### Participant actions

    mute/unmute: mute/unmute participant
    deaf/undeaf: deaf/undeaf participant
    kick: kick participant

### Web-socket events

A client may subscribe to conference event using websocket connection. Participant events are published as
`amqp conference.event.{conference_id}.{call_id}`, where call_id is participant"s call.

The list of published events is determined by `publish_participant_event` parameter of ecallmgr configuration,
if parameter is unset, then all events are published.

#### Participant events

    add-member
    del-member
    stop-talking
    start-talking
    mute-member
    unmute-member
    deaf-member
    undeaf-member

#### Example event

```json
{
    "custom_channel_vars": {
        "account_id": "9d351ad7ffd6f846313af9eed3bb7b85",
        "authorizing_id": "6507f40b09a61fbb8b025dbad9316eb5",
        "authorizing_type": "device",
        "owner_id": "32d8788da9506b4b1991d5bb86d27b0a",
        "presence_id": "1000@kamailio.kazoo",
        "fetch_id": "56507071-a216-4e0a-a28f-ff3bd9c86ac3",
        "bridge_id": "934800819",
        "precedence": 5,
        "realm": "kamailio.kazoo",
        "username": "sip1",
        "call_interaction_id": "63635497023-3e247b2e"
    },
    "channel_presence_id": "1000@kamailio.kazoo",
    "caller_id_number": "sip1",
    "caller_id_name": "sip1",
    "mute_detect": false,
    "video": false,
    "energy_level": 20,
    "current_energy": 0,
    "talking": false,
    "speak": true,
    "hear": true,
    "floor": false,
    "participant_id": 20,
    "instance_id": "d5765180-53d5-4104-860e-b352f3f8e6b1",
    "conference_id": "5edbfdd3b825314a71b0a05957392edb",
    "focus": "freeswitch@freeswitch.kazoo",
    "call_id": "934800819",
    "event": "add-member",
    "node": "kazoo_apps@jh460",
    "msg_id": "a6fbbf034b5cd3af",
    "event_name": "participant_event",
    "event_category": "conference",
    "app_version": "4.0.0",
    "app_name": "ecallmgr",
    "routing_key": "participant_event"
}
```
