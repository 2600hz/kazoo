### Conferences

#### About Conferences

Conferences documents are enriched with realtime information, namely: number of members, number of moderators, duration of the conference,
conference locked status. The realtime information is added to conference document under _read_only key (to avoid accident document update).

#### Schema

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`conference_numbers` | Defines conference numbers that can be used by members or moderators | `array(string)` | `[]` | `false`
`conference_numbers.[]` |   | `string` |   | `false`
`focus` | This is a read-only property indicating the media server hosting the conference | `string` |   | `false`
`member` | Defines the discovery properties for a member | `object` | `{}` | `false`
`member.join_deaf` | Determines if a member will join deaf | `boolean` | `false` | `false`
`member.join_muted` | Determines if a member will join muted | `boolean` | `true` | `false`
`member.numbers` | Defines the conference number(s) for members | `array(string)` | `[]` | `false`
`member.numbers.[]` |   | `string` |   | `false`
`member.pins` | Defines the pin number(s) for members | `array(string)` | `[]` | `false`
`member.pins.[]` |   | `string` |   | `false`
`moderator` | Defines the discovery properties for a moderator | `object` | `{}` | `false`
`moderator.join_deaf` | Determines if a moderator will join deaf | `boolean` | `false` | `false`
`moderator.join_muted` | Determines if a moderator will join muted | `boolean` | `false` | `false`
`moderator.numbers` | Defines the conference number(s) for moderators | `array(string)` | `[]` | `false`
`moderator.numbers.[]` |   | `string` |   | `false`
`moderator.pins` | Defines the pin number(s) for moderators | `array(string)` | `[]` | `false`
`moderator.pins.[]` |   | `string` |   | `false`
`name` | A friendly name for the conference | `string(1..128)` |   | `false`
`owner_id` | The user ID who manages this conference | `string(32)` |   | `false`
`play_name` | Do we need to announce new conference members? | `boolean` | `false` | `false`
`profile` | The XML profile name used to configure the conference | `string` |   | `false`


#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/conferences

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences
```

#### Create

> PUT /v2/accounts/{ACCOUNT_ID}/conferences

```curl
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences
```

#### Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}

```curl
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}
```

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}
```

#### Patch

> PATCH /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}

```curl
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}
```

#### Change

> POST /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}

```curl
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}
```

#### Perform an action on conference

> PUT /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}

```curl
curl -v -X PUT \
    -d '{"data": {"action": {CONFERENCE_ACTION}}}' \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}
```

CONFERENCE_ACTION: lock, unlock

#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants

```curl
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants
```

#### Perform an action on participants

> PUT /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants

```curl
curl -v -X PUT \
    -d '{"data": {"action": {PARTICIPANTS_ACTION}}}' \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants
```

PARTICIPANTS_ACTION: mute/unmute/deaf/undeaf/kick

#### Perform an action on participant

> PUT /v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants/{PARTICIPANT_ID}

```curl
curl -v -X PUT \
    -d '{"data": {"action": {PARTICIPANT_ACTION}}}' \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/conferences/{CONFERENCE_ID}/participants/{PARTICIPANT_ID}
```

PARTICIPANT_ACTION: mute/unmute/deaf/undeaf/kick

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

join_time is participant"s join time as epoch, duration is number of seconds participant participate in conference.

Here we can see values set up for a Member, then for a Moderator.

The last field, **play_entry_tone**, is at the root of the document: meaning this field applies to everyone in the conference.

#### Available fields

* **play_entry_tone** and **play_exit_tone**: can be either a boolean or a non-empty string.
    * `true` means play the default tone when someone joins (or leaves) the conference
    * `false` disables the tone from being played
    * A string like a *tone string* or a *URI to a media file* can be inputed.

#### Actions

Actions are JSON objects in format: 
```
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
amqp conference.event.{conference_id}.{call_id}, where call_id is participant"s call. 

The list of published events is determined by *publish_participant_event* parameter of ecallmgr configuration,
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
  "Custom-Channel-Vars": {},
  "Channel-Presence-ID": "sip1@kamailio.kazoo",
  "Caller-ID-Number": "sip1",
  "Caller-ID-Name": "sip1",
  "Mute-Detect": false,
  "Video": false,
  "Energy-Level": 0,
  "Current-Energy": 1452,
  "Talking": true,
  "Speak": true,
  "Hear": false,
  "Floor": true,
  "Participant-ID": 1,
  "Instance-ID": "981c98fe-964c-4a82-a06d-128ead6cf77c",
  "Conference-ID": "58cd77f988191a314ceb821e7f9d6a35",
  "Focus": "freeswitch@freeswitch.kazoo",
  "Call-ID": "899417183",
  "Event": "start-talking",
  "Node": "kazoo_apps@jh460",
  "Msg-ID": "78bb6cd5dba480ee",
  "Event-Name": "participant_event",
  "Event-Category": "conference",
  "App-Version": "4.0.0",
  "App-Name": "ecallmgr",
  "routing_key": "participant_event"
}
```
