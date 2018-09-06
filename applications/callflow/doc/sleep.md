## Sleep

### About Sleep

Waits a period of time and continues to the next callflow.

#### Schema

Validator for the sleep callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`duration` | How long to pause before continuing the callflow | `integer()` | `0` | `false` |  
`unit` | What time unit is 'duration' in | `string('ms' | 's' | 'm' | 'h')` | `s` | `false` |  



