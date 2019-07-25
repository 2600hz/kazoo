## Voicemail

### About Voicemail

Puts the caller into the Voicemail system.

#### Schema

Validator for the Voicemail callflow element



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | Whether to check voicemail box or compose a new voicemail message | `string('check' | 'compose')` | `compose` | `false` |  
`callerid_match_login` | Whether to match the caller ID to a voicemail box | `boolean()` | `false` | `false` |  
`id` | The ID of the voicemail box | `string(32)` |   | `false` |  
`interdigit_timeout` | The amount of time (in milliseconds) to wait for the caller to press the next digit after pressing a digit | `integer()` | `2000` | `false` |  
`max_message_length` | Max length of the message that caller can leave in voicemail box | `integer()` | `500` | `false` |  
`single_mailbox_login` | Allow login if caller has a single mailbox | `boolean()` | `false` | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  






### Checking voicemail

There are a couple ways to create feature codes to check a voicemail box.

#### Generic voicemail check

```json
{
  "flow":{
    "module":"voicemail"
      ,"data":{
        "action":"check"
      }
    }
  }
  ,"numbers":["*97"]
}
```

If the calling device has an `owner_id` and a voicemail box has the same `owner_id`, that voicemail box will be loaded.

If the voicemail box does not require a pin, the caller will be put into the prompts to check the box.

If the voicemail box does require a pin, the caller will need to enter the pin to access the box.

#### Generic voicemail check with auto-login

```json
{
  "flow": {
    "module": "voicemail"
    ,"data": {
      "action": "check",
      "single_mailbox_login": true
    }
  }
  ,"numbers": [
    "*98"
  ]
}
```

If the owner of the device also only owns one voicemail box, they will be logged in automatically to that box (pin rules apply).

#### Generic voicemail check feature code

```json
{
  "flow": {
    "module": "voicemail"
    ,"data": {
      "action": "check"
    }
  }
  ,"patterns": [
    "\\*98(\\d+)"
  ]
}
```

If you would like to tie a BLF key to monitor and check a voicemail box, create a pattern (with the literal `*` escaped as `\\*`).

For example, to monitor and check box 3456, the BLF key could be tied to `*983456`. This would then check box `3456` and if the device owner matches the box owner, auto-login would occur (pin rules apply here too).

!!! note
    If you want to do MWI subscriptions, you must configure the account or system to do so. In the `callflow` system_config document (or the account's config doc), set `dialog_subscribed_mwi_prefix` to the prefix (in this above case, `*98` would be the value): `sup kapps_config set_default callflow dialog_subscribed_mwi_prefix '*98'`.
