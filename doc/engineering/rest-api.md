REST API
========

Overview
--------

URI addresses a specific resource or resources. HTTP GET action returns the resource as JSON object.
HTTP POST action executes an action (a JSON object) on the resource, and returns results as JSON object.

Examples
--------

URI: v2/accounts/{AccountId}/conferences

    GET: list of conferences

URI: v2/accounts/{AccountId}/conferences/{ConferenceID}

    GET: a conference with details
    POST: conference-wide actions (lock/unlock)

URI: v2/accounts/{AccountId}/conferences/{ConferenceID}/participants

    GET: a list of conference participants
    POST: all-participant wide actions (mute/unmute/deaf/undeaf/kick)

URI: v2/accounts/{AccountId}/conferences/{ConferenceID}/participants/{ParticipantId}

    GET: n/a
    POST: participant actions (mute/unmute/deaf/undeaf/kick)

Action examples
---------------

```
POST v2/accounts/{AccountId}/conferences/{ConferenceID}/participants/{ParticipantId}
{
    data: {
        Action: mute/unmute/deaf/undeaf/kick
    }
}
```

POST v2/accounts/{AccountId}/conferences/{ConferenceID}
```
{
    data: {
        Action: mute/unmute/deaf/undeaf/kick
    }
}
```