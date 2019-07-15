## Function

### About Function

Runs the function in the local infrastructure to create a new callflow to branch to.

#### Schema

Validator for the function callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`id` | The Function's doc ID | `string()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



