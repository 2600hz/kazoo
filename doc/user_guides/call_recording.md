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

##### Callflow action

You can enable call recording on a per-callflow basis by using the `record_call` callflow action. The callflow action can take the following parameters:

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | Toggle whether to start or stop a recording | `string('start', 'stop')` |   | `true`
`format` | Disk Format to write the recording | `string('mp3', 'wav')` |   | `false`
`record_min_sec` | Minimal record time, in seconds, to store recordings | `integer` |   | `false`
`record_on_answer` | Whether to delay starting the recording until the call is answered | `boolean` |   | `false`
`record_sample_rate` | Sampling rate of the recording, in Hz | `integer` |   | `false`
`time_limit` | Limit, in seconds, of how long to record the call | `integer` |   | `false`
`url` | What URL to use as a base for where to send the recording after it finishes | `string` |   | `false`

#### Disambiguate

The callflow modules are hard to differentiate at first. Hopefully this helps.

`cf_record_caller` uses the [`record`](https://wiki.freeswitch.org/wiki/Misc._Dialplan_Tools_record) command, intended for recording messages (such as voicemails).

`cf_record_call` starts a `wh_media_recording` process (by default) which uses the `record_call` command which utilizes [`uuid_record`](https://wiki.freeswitch.org/wiki/Mod_commands#uuid_record) to record the call leg's audio stream.
