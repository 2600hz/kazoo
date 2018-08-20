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
    "account_id": "63463bc16b7af744f88c05567219c839",
    "agent_id": "0a1fde86ac5e801b57ae58510710b31f",
    "agent_call_id": "72b893f06327393eaae2617282a9c4b3-0a1fde86ac5e801b57ae58510710b31f-e899509d",
    "call_state": "pickup",
    "caller_id_name": "1003 Guy",
    "caller_id_number": "1003",
    "member_call_id": "6sIfYg-xQMw71l-6t5nJMkFu6Xxrbix8",
    "now": 63702009674,
    "queue_id": "f55a9c721180cb044c3a28239f47f460"
}
```
