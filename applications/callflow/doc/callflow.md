## Callflow

### About Callflow

Branches the current call to another callflow.

#### Schema

Validator for the callflow callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`id` | The Callflow ID to branch to | `string()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



