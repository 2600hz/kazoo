# Milliwatt *Echo and tone for monitoring*

Provides rudimentary testing of RTP via an echo test and a tone test.

## Configuration

Start Milliwatt as well: `sup kapps_controller start_app milliwatt`

Each test can be configured to match a list of numbers or caller IDs. The SIP `to` will be matched against the test's `number` array and the call's caller ID number will be matched against the test's `caller_id` array.

## Echo Test

The `echo` test established an audio path with the caller and echos back any audio received.

The `echo` test is configured in the `system_config/milliwatt` document under the `echo` key:

```json
{"_id":"milliwatt"
,"default":{
    "echo":{
        "number":["5555555552"]
        ,"caller_id":["12345"]
    }
 }
}
```

Additionally, the `echo` test can configure how long to let the call last for: `"echo":{"duration":10000}` where `10000` is 10 seconds (configuration value is in milliseconds).

## Tone Test

The `tone` test will play a specified tone to the caller.

The `tone` test is configured in the `system_config/milliwatt` document under the `tone` key:

```json
{"_id":"milliwatt"
,"default":{
    "tone":{
        "number":["5555555552"]
        ,"caller_id":["12345"]
    }
 }
}
```

Additionally the `tone` test has a few more configuration knobs:

| Key             | Description                                     | Default    |
| `frequencies`   | The list of frequencies to play                 | `["2600"]` |
| `frequency_on`  | How long to play the tone(s) for, in ms         | `5000`     |
| `frequency_off` | How long to play the tone(s) for, in ms         | `30000`    |
| `duration`      | How long, in milliseconds, to continue the call | `30000`    |
