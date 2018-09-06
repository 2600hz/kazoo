### Queues

#### About Queue Webhooks

Kazoo queues support sending notification webhooks when certain events occur. HTTP notifications can be sent either as GET or POST.

#### Notification after Pickup/Notification on Hangup

> GET /your_endpoint
> POST /your_endpoint

Query string/POST body fields:

Key | Type | Description
--- | ---- | -----------
`account_id` | `string()` | Account ID of the queue call
`agent_id` | `string()` | ID of the agent whom was assigned to the call
`agent_call_id` | `string()` | ID of the call leg for the agent whom was assigned to the call
`call_state` | `string('pickup' | 'hangup')` | Identifies the event type
`caller_id_name` | `string()` | Caller ID name of the queue caller
`caller_id_number` | `string()` | Caller ID number of the queue caller
`member_call_id` | `string()` | ID of the call (the original call in case of a callback)
`now` | `integer()` | Gregorian timestamp of the time the event was sent
`queue_id` | `string()` | ID of the queue the call is in

```json
{
    "account_id": "{ACCOUNT_ID}",
    "agent_id": "{AGENT_ID}",
    "agent_call_id": "{AGENT_CALL_ID}",
    "call_state": "pickup",
    "caller_id_name": "{CID_NAME}",
    "caller_id_number": "{CID_NUMBER}",
    "member_call_id": "{MEMBER_CALL_ID}",
    "now": {GREGORIAN_SECONDS},
    "queue_id": "{QUEUE_ID}"
}
```
