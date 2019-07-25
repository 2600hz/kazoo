## Sleep

### About Sleep

Waits a period of time and continues to the next callflow.

#### Schema

Validator for the sleep callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`duration` | How long to pause before continuing the callflow | `integer()` | `0` | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`unit` | What time unit is 'duration' in | `string('ms' | 's' | 'm' | 'h')` | `s` | `false` |  



