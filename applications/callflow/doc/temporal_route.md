## Temporal Route

### About Temporal Route

Branch the callflow based on the current date and time when compared to the temporal rule(s) associated with the action.

#### Schema

Validator for the temporal_route callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | What action to perform | `string('menu' | 'enable' | 'disable' | 'reset')` |   | `false` |  
`interdigit_timeout` | How long, in milliseconds, to wait for the next keypress | `integer()` |   | `false` |  
`rule_set` | ID of the rule set | `string()` |   | `false` |  
`rules.[]` |   | `string()` |   | `false` |  
`rules` | List of rule IDs to use | `array(string())` | `[]` | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  
`timezone` | Timezone to use when processing temporal rules | `string()` |   | `false` |  






### Rule Set

To use  a Rule Set in a callflow:

1. Create your `temporal_route` module and add the `rule_set` field to the data payload. The `rule_set` should be the `ID` of the document.
2. Set your first children to be the catch all (using `_` for the key).
3. Set the second one using `rule_set`.

If one (or more) of the rule in the rule set is satisfied it will got to the children using the `rule_set` key. If not to the catch all.
