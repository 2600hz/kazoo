## Send Dtmf

### About Send Dtmf

Send DTMF to the call

#### Schema

Validator for the Send DTMF callflow action



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`digits` | String of DTMF tones to send | `string()` |   | `true` |  
`duration_ms` | How long, in milliseconds, to send each DTMF | `integer() | string()` | `2000` | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



