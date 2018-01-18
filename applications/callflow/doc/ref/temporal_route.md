## Temporal Route

### About Temporal Route

#### Schema

Validator for the temporal_route callflow's data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | What action to perform | `string('menu' | 'enable' | 'disable' | 'reset')` |   | `false` |  
`interdigit_timeout` | How long, in milliseconds, to wait for the next keypress | `integer()` |   | `false` |  
`rule_set` | ID of the rule set | `string()` |   | `false` |  
`rules.[]` |   | `string()` |   | `false` |  
`rules` | List of rule IDs to use | `array(string())` | `[]` | `false` |  
`timezone` | Timezone to use when processing temporal rules | `string()` |   | `false` |  



