## Faxbox

### About Faxbox

#### Schema

Validator for the faxbox callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`faxbox_id` | ID of the faxbox | `string()` |   | `false` |  
`id` | ID of the faxbox | `string()` |   | `false` |  
`media.fax_option` | Caller flag for T38 settings | `boolean() | string('auto' | 'true' | 'false')` |   | `false` |  
`media` |   | `object()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



