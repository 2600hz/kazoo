## Faxbox

### About Faxbox

Puts the caller into a faxbox (expectation is the caller is a fax).

#### Schema

Validator for the faxbox callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`faxbox_id` | ID of the faxbox | `string()` |   | `false` |  
`id` | ID of the faxbox | `string()` |   | `false` |  
`media.fax_option` | Caller flag for T38 settings | `string('auto' | 'true' | 'false') | boolean()` |   | `false` |  
`media` |   | `object()` |   | `false` |  



