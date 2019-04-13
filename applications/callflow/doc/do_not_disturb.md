## Do Not Disturb

### About Do Not Disturb

#### Schema

Validator for the do_not_disturb callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`action` | What action to perform | `string('activate' | 'deactivate' | 'toggle')` |   | `false` |  
`id` | Document ID on which to update DND settings | `string()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



