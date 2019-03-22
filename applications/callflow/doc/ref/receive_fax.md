## Receive Fax

### About Receive Fax

#### Schema

Validator for the receive_fax callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`media.fax_option` | Caller flag for T38 settings | `string('auto' | 'true' | 'false') | boolean()` |   | `false` |  
`media` |   | `object()` |   | `false` |  
`owner_id` | User ID to send fax to | `string()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



