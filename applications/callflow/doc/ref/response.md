## Response

### About Response

#### Schema

Validator for the Response callflow action



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`code` | The SIP Hangup code to respond with | `integer()` | `486` | `true` |  
`media` | Optional media file to play before responding with the hangup code/cause | `string(0..2048)` |   | `false` |  
`message` | The SIP Hangup cause to respond with | `string()` |   | `false` |  
`skip_module` | When set to true this callflow action is skipped, advancing to the wildcard branch (if any) | `boolean()` |   | `false` |  



