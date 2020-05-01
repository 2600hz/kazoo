## Disa

### About Disa

#### Schema

Validator for the DISA callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`enforce_call_restriction` | Enforce that callers can't call numbers they wouldn't normally be allowed to call | `boolean()` | `false` | `false` |  
`interdigit` | Increase to allow more time between DTMF presses | `integer()` |   | `false` |  
`max_digits` | Maximum digits allowed when collecting destination number | `integer()` | `15` | `false` |  
`pin` | PIN code to allow caller use this feature | `string()` | "" | `false` |  
`preconnect_audio` | What to play for the caller before collecting the destination number | `string('dialtone' | 'ringing')` | `dialtone` | `false` |  
`retries` | Maximum number of retries to collect PIN and/or destination number | `integer()` | `3` | `false` |  
`ring_repeat_count` | How many times to repeat the ringing (if preconnect_audio is used) | `integer()` | `1` | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`use_account_caller_id` | Whether to override the caller ID with the account's external caller ID settings | `boolean()` |   | `false` |  



