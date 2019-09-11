# Call Recording

## About Call Recording

Setting up call recording allows you to have a permanent record of a conversation that took place.

## Configuring Call Recording

Call recording can be enabled in a number of ways:

* At the account level
* At the user level
* At the device level
* Explicitly as a callflow action

Call recording also requires you to have a web server capable of receiving an HTTP PUT request with the contents of the recording. You are then free to save the recording in a way that meets your needs.

### Account, User, or Device

Configuring recording at the account level means all calls, inbound and outbound, to this account will start the call recording system.

Configuring recording at the user level starts recording for any calls to/from a device owned by that user.

Configuring recording at the device level starts recording for any calls to/from the device.

#### Precedence of settings

Precedence of settings is: Device > User

If a user turns on call recording but a device has explicitly disabled it, the device will not be recorded when the user makes a call with it. If the device's settings are left undefined, the user's settings will be applied.

The account's settings are considered independently of the endpoint's. So a user who has disabled recording, within an account that has enabled recording, will still have calls recorded according to the account's settings.

#### Recording settings matrix

##### Account Settings

When an onnet device makes an internal call:

| Setting                       | Source | Destination | Recording Started                          |
| -------                       | ------ | ----------- | -----------------                          |
| Account -> Inbound  -> Onnet  | onnet  | onnet       | yes                                        |
| Account -> Inbound  -> Offnet | onnet  | onnet       | no                                         |
| Account -> Outbound -> Onnet  | onnet  | onnet       | yes (if inbound -> onnet isn't configured) |
| Account -> Outbound -> Offnet | onnet  | onnet       | no                                         |

When an onnet device makes an external call:

| Setting                       | Source | Destination | Recording Started                          |
| -------                       | ------ | ----------- | -----------------                          |
| Account -> Inbound  -> Onnet  | onnet  | offnet      | yes                                        |
| Account -> Inbound  -> Offnet | onnet  | offnet      | no                                         |
| Account -> Outbound -> Onnet  | onnet  | offnet      | no                                         |
| Account -> Outbound -> Offnet | onnet  | offnet      | yes (if inbound -> onnet isn't configured) |

When an offnet device makes an internal call:

| Setting                       | Source | Destination | Recording Started                        |
| -------                       | ------ | ----------- | -----------------                        |
| Account -> Inbound  -> Onnet  | offnet | onnet       | no                                       |
| Account -> Inbound  -> Offnet | offnet | onnet       | yes                                      |
| Account -> Outbound -> Onnet  | offnet | onnet       | yes (if inbound-offnet isn't configured) |
| Account -> Outbound -> Offnet | offnet | onnet       | no                                       |

##### Endpoint Settings

When an onnet device makes an internal call:

| Setting                        | Source | Destination | Recording Started |
| -------                        | ------ | ----------- | ----------------- |
| Endpoint -> Inbound  -> Onnet  | onnet  | onnet       | yes¹              |
| Endpoint -> Inbound  -> Offnet | onnet  | onnet       | no                |
| Endpoint -> Outbound -> Onnet  | onnet  | onnet       | yes               |
| Endpoint -> Outbound -> Offnet | onnet  | onnet       | no                |

When an onnet device makes an external call:

| Setting                        | Source | Destination | Recording Started |
| -------                        | ------ | ----------- | ----------------- |
| Endpoint -> Inbound  -> Onnet  | onnet  | offnet      | no                |
| Endpoint -> Inbound  -> Offnet | onnet  | offnet      | no                |
| Endpoint -> Outbound -> Onnet  | onnet  | offnet      | no                |
| Endpoint -> Outbound -> Offnet | onnet  | offnet      | yes               |

When an offnet endpoint makes a call to an onnet device:

| Setting                        | Source | Destination | Recording Started |
| -------                        | ------ | ----------- | ----------------- |
| Endpoint -> Inbound  -> Onnet  | offnet | onnet       | no                |
| Endpoint -> Inbound  -> Offnet | offnet | onnet       | yes               |
| Endpoint -> Outbound -> Onnet  | offnet | onnet       | no                |
| Endpoint -> Outbound -> Offnet | offnet | onnet       | no                |

#### Enabling recording

To enable call recording, add `"call_recording":{...}` to the document of choice. For example, if you have a user with user ID of `{USER_ID}`, you can patch the user's document using Crossbar:

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{
    "call_recording":{...}
    }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}
```

#### Concrete example

Sales wants calls to and from customers to be recorded but not calls within the account. The sales users would have their `call_recording` settings set as:

```json
{"id":"{USER_ID}"
 ,"call_recording":{
   "inbound":{
     "offnet":{
       ...recording settings...
     }
   }
   ,"outbound":{
     "offnet":{
       ...recording settings...
     }
   }
 }
}
```

### The call recording settings

Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`enabled` | is recording enabled | `boolean()` |   | `false` |
`format` | What format to store the recording on disk | `string('mp3' | 'wav')` |   | `false` |
`record_min_sec` | The minimum length, in seconds, the recording must be to be considered successful. Otherwise it is deleted | `integer()` |   | `false` |
`record_on_answer` | Recording should start on answer | `boolean()` |   | `false` |
`record_on_bridge` | Recording should start on bridge | `boolean()` |   | `false` |
`record_sample_rate` | What sampling rate to use on the recording | `integer()` |   | `false` |
`time_limit` | Time limit, in seconds, for the recording | `integer()` |   | `false` |
`url` | The URL to use when sending the recording for storage | `string()` |   | `false` |

### Callflow action - Record Call

You can enable call recording on a per-callflow basis by using the `record_call` callflow action. The callflow action can take the following parameters:

Key | Description | Type | Default | Required | Support
--- | ----------- | ---- | ------- | -------- | --------
`action` | Whether to start or stop the recording | `string('start', 'stop')` | `start` | `true` |
`format` | What format to store the recording on disk | `string('mp3', 'wav')` |   | `false` |
`label` | Label to include in the origin of call recording | `string()` |   | `false` |
`media_name` | the name of media | `string` |   | `false` |
`record_min_sec` | The minimum length, in seconds, the recording must be to be considered successful. Otherwise it is deleted | `integer` |   | `false` |
`record_on_answer` | Whether to delay the recording until the channel is answered | `boolean` | `false` | `false` |
`record_on_bridge` | Whether to delay the recording until the channel is bridged | `boolean` | `false` | `false` |
`record_sample_rate` | What sampling rate to use on the recording | `integer` |   | `false` |
`time_limit` | Time limit, in seconds, for the recording | `integer` | `3600` | `false` |
`url` | The URL to use when sending the recording for storage | `string` |   | `false` |

### Callflow action - Record Caller

The `record_caller` callflow action is meant for recording things like voicemails or IVR prompts. The schema for the callflow action is:

Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`format` | What format to store the recording on disk | `string('mp3' | 'wav')` |   | `false` |
`method` | What HTTP method to use when sending the recording | `string('put' | 'post')` | `put` | `false` |
`time_limit` | Time limit, in seconds, for the recording | `integer()` | `3600` | `false` |
`url` | The URL to use when sending the recording for storage | `string()` |   | `false` |
