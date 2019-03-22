## Collect Dtmf

### About Collect Dtmf

Collect DTMF from the caller and store it in the call record. Typically used in conjunction with [Pivot](./pivot.md).

#### Schema

Validator for the Collect DTMF callflow element



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`collection_name` | Store collected DTMF in a named key | `string()` |   | `false` |  
`interdigit_timeout` | How long, in milliseconds, to wait for the next DTMF | `integer()` |   | `false` |  
`max_digits` | How many DTMFs to collect from the caller | `integer()` | `1` | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`terminator` | What DTMF will terminate collection before the timeout occurs | `string('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' | '#' | '*')` | `#` | `false` |  
`terminators.[]` |   | `string('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' | '#' | '*')` |   | `false` |  
`terminators` | What DTMFs will terminate collection before the timeout occurs | `array(string('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' | '#' | '*'))` |   | `false` |  
`timeout` | How long, in milliseconds, to wait for the first DTMF | `integer()` | `5000` | `false` |  



