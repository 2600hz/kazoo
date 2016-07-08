
### Conference

Conference documents are enriched with realtime information, namely: number of members, number of moderators, duration of the conference,
conference locked status. The realtime information is added to conference document under _read_only key (to avoid accident document update).

#### List of conferences

```json
[{
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
  "_read_only": {
    "members": 0,
    "admins": 0,
    "duration": 0,
    "is_locked": false
  }
},
....
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
        ....
    ]
  }
}
```

join_time is participant's join time as epoch, duration is number of seconds participant participate in conference.

Here we can see values set up for a Member, then for a Moderator.

The last field, **play_entry_tone**, is at the root of the document: meaning this field applies to everyone in the conference.

#### Available fields

* **play_entry_tone** and **play_exit_tone**: can be either a boolean or a non-empty string.
    * `true` means play the default tone when someone joins (or leaves) the conference
    * `false` disables the tone from being played
    * A string like a *tone string* or a *URI to a media file* can be inputed.


### URI Schema

URI: v2/accounts/{AccountId}/conferences

    GET: list of conferences with realtime data
    POST: create new conference

URI: v2/accounts/{AccountId}/conferences/{ConferenceID}

    GET: a conference with realtime data (participants)
    POST: set conference
    PATCH: update conference
    DELETE: delete conference
    PUT: perform an action on conference

URI: v2/accounts/{AccountId}/conferences/{ConferenceID}/participants

    GET: a list of conference participants
    PUT: perform an action on conference participants

URI: v2/accounts/{AccountId}/conferences/{ConferenceID}/participants/{ParticipantId}

    PUT: perform an action on conference participant

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

A client may subscribe to conference event using websocket connection.

####
