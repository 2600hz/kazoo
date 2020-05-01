## Voicemail

### About Voicemail

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



