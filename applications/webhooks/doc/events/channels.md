# Channel Events

Typically, events would be fired for new calls, when a call is answered, and when a call is finished:

* `channel_answer`
* `channel_bridge`
* `channel_create`
* `channel_destroy`

These events are sharing common fields below though their existence and their values, especially `custom_channel_vars` and `custom_application_vars`, varies and are depend on specific call types.

## Base Channel event sample

Here is a sample of what you can expect to receive when a channel event is triggered. As said above the values and existence is varied based on the where from/to call is originate and the type of the call:

```json
{
    "account_id": "91eb05e5c68d7ca05e7618b6821d2d16",
    "authorizing_id": "7f2b66a620cf9523d349cc5de8f21f37",
    "authorizing_type": "device",
    "call_direction": "inbound",
    "call_forwarded": false,
    "call_id": "NDlkNGI3OTZlMDgzNWQxNGYxNTA3NjQ4NDZjNDFkOTc",
    "callee_id_name": "Caller Test",
    "callee_id_number": "+15556783945",
    "caller_id_name": "Test Man",
    "caller_id_number": "+15556783945",
    "custom_application_vars": {},
    "custom_channel_vars": {},
    "emergency_resource_used": false,
    "from": "+15556783945@matrix.test.com",
    "inception": "+15556783945@matrix.test.com",
    "is_internal_leg": false,
    "local_resource_id": "d88c4e753d18755ca71adbae784e5f10",
    "local_resource_used": true,
    "other_leg_call_id": "af1e1e12f1bcf519a96f2235ab8eeec4",
    "owner_id": "d860229e722bf8f738737365b0b1e333",
    "request": "+15552345678@matrix.test.com",
    "reseller_id": "026f4f0a6d99f9a19b6aa4aa75501d7d",
    "timestamp": "63724349409",
    "to": "+15552345678@matrix.test.com"
}
```


## Channel Answer

This webhook is triggered when a channel establishes two-way audio, such as a voicemail box or the called party answering.

### Info

* **Name:** channel_answer
* **Friendly name:** Channel Answer

### Modifiers

_None._

### Sample

In addition to [Base Channel Payload](#base-channel-event-sample) this event includes below fields:

```json
    "hook_event": "channel_answer"
```


## Channel Bridge

This webhook is triggered when two channels are bridged together, such as two users/devices connected together.

### Info

* **Name:** channel_bridge
* **Friendly name:** Channel Bridge

### Modifiers

_None._

### Sample

In addition to [Base Channel Payload](#base-channel-event-sample) this event includes below fields:

```json
    "hook_event": "channel_bridge",
    "original_number": "+15555674567",
    "other_leg_destination_number": "+1345678349"
```


## Channel Create

This webhook is triggered when a new channel is created.

### Info

* **Name:** channel_create
* **Friendly name:** Channel Create

### Modifiers

_None._

### Sample

In addition to [Base Channel Payload](#base-channel-event-sample) this event includes below fields:

```json
    "hook_event": "channel_create"
```


## Channel Destroy

This webhook is triggered when a channel is destroyed, usually as a result of a hangup.

### Info

* **Name:** channel_destroy
* **Friendly name:** Channel Destroy

### Modifiers

_None._

### Sample

In addition to [Base Channel Payload](#base-channel-event-sample) this event includes below fields:

```json
    "hook_event": "channel_destroy",
    "hangup_cause": "NORMAL_CLEARING",
    "hangup_code": "sip:200",
    "duration_seconds": 6,
    "ringing_seconds": 0,
    "billing_seconds": 6
```
