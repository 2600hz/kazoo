### Call Recording

#### About Call Recording

Setting up call recording allows you to have a permanent record of a conversation that took place.

#### Configuring Call Recording

Call recording can be enabled in a number of ways:

* At the account level
* At the user level
* At the device level
* Explicitly as a callflow action

Call recording also requires you to have a web server capable of receiving an HTTP PUT request with the contents of the recording. You are then free to save the recording in a way that meets your needs.

##### Account, User, or Device

Configuring recording at the account level means all calls, inbound and outbound, to this account will start the call recording system.

Configuring recording at the user level starts recording for any calls to/from a device owned by that user.

Configuring recording at the device level starts recording for any calls to/from the device.

To enable call recording, add `"record_call":true` to the document of choice. For example, if you have a user with user ID of `{USER_ID}`, you can patch the user's document using Crossbar:

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    -d '{"data":{"record_call":true}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}
```

If successful, all calls to/from this user will be recorded.

##### Callflow action - Record Call

You can enable call recording on a per-callflow basis by using the `record_call` callflow action. The callflow action can take the following parameters:

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | Toggle whether to start or stop a recording | `string('start', 'stop')` |   | `true`
`format` | Disk Format to write the recording | `string('mp3', 'wav')` |   | `false`
`record_min_sec` | Minimal record time, in seconds, to store recordings | `integer` |   | `false`
`record_on_answer` | Whether to delay starting the recording until the call is answered | `boolean` |   | `false`
`record_sample_rate` | Sampling rate of the recording, in Hz | `integer` |   | `false`
`time_limit` | Limit, in seconds, of how long to record the call | `integer` |   | `false`
`url` | What URL to use as a base for where to send the recording after it finishes (stores locally if not included) | `string` |   | `false`

##### Callflow action - Record Caller

The `record_caller` callflow action is meant for recording things like voicemails or IVR prompts. The schema for the callflow action is:

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`format` | What format to store the recording on disk | `string('mp3', 'wav')` |   | `false`
`method` | What HTTP method to use when sending the recording | `string('put', 'post')` |   | `false`
`time_limit` | Time limit, in seconds, for the recording | `integer` | `3600` | `false`
`url` | The URL to use when sending the recording for storage (stores locally if not included) | `string` |   | `false`
