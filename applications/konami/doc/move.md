## Move

### About Move

When a user is talking on one of their endpoints (say their Kazoo Mobile phone) and they move locations to have access to their IP Desk phone, it is helpful to move the call from the mobile phone to the deskphone with the other party unaware of the change.

#### Schema

Move a call from one device to another



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`auto_answer` | Whether to auto-answer the new leg | `boolean()` | `false` | `false`
`can_call_self` | Can intercept devices of the same targeted user | `boolean()` | `true` | `false`
`device_id` | Move the call to a specific device | `string()` |   | `false`
`dial_strategy` | How to ring the endpoints, if multiple | `string()` | `simultaneous` | `false`
`owner_id` | User ID to use for finding endpoints | `string()` |   | `false`



### How It Works

Alice is talking with Bob on her mobile phone while she walks to the office. Once she sits down, it is more convenient to transfer the call to her desk phone, but Alice doesn't want to pause the conversation. Instead, she invokes her Konami Move key sequence. Her desk phone starts ringing, and once she answers the desk phone, the call with Bob is immediately available (and her mobile phone has hung up).

### Configure the metaflow

The *move* module should be placed under the "numbers" key in the "metaflows" object.

```json
    "metaflows":{
        "numbers":{
            "8":{
                "module":"move"
                ,"data":{"owner_id":"alices_owner_id"}
            }
        }
        ,"patterns":{...}
        ,"binding_key":"*"
    }
```
`owner_id` optional. If not set it will use device owner id.
