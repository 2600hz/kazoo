## Intercom

### About Intercom

Determines if the caller should barge in on the callee

#### Schema

Validator for the intercom callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`barge_calls` | Whether to barge in on the callee | `boolean()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



