### API for managing agent status
This API provides convenient way for agent status management without need to dial feature codes. It's useful for various call center agent/supervisor interfaces.

##### Log In/Log Out agent to/from some queue

/agents/AID/queue_status (GET, POST):

```json
{
  "data":{
    "action":{{action}},
    "queue_id":{{queue_id}}
  }
}
```

where

{{action}} - "login" | "logout"
and {{queue_id}} is an ID of the queue

##### Set agent status:

/agents/AID/status  (GET, POST):

```json
{
  "data":{
    "status":{{status}},
    "timeout":{{timeout}},
    "presence_id":{{id}},
    "presence_state":{{state}}
  }
}
```

where
{{status}} - "login" | "logout" | "pause" | "resume"
{{timeout}} - timeout for "pause" status
`presence_id` и `presence_state` - optional fields for presence information

If the agent is on call in time of request, then "pause",  "resume"  and "logout" commands will be executed right after the agent is back from the call.

When `logout` is issued when agent is on call, the agent will be assigned special status of `pending_logged_out` until hang up (after which the logout command will be executed). While in this status, agent can't log in again until logout is completed.
