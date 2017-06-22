## Response

### About Response

Return a custom SIP response to the caller

#### Schema

Validator for the Response callflow action



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`code` | The SIP Hangup code to respond with | `integer()` | `486` | `true`
`media` | Optional media file to play before responding with the hangup code/cause | `string()` |   | `false`
`message` | The SIP Hangup cause to respond with | `string()` |   | `false`



