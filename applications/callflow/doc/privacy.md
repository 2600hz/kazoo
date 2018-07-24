### Callflow Privacy

### About Privacy

The `privacy` callflow enables set caller privacy on calls, restricting the presentation some or full parts of Caller ID.

#### Schema

Validator for the privacy callflow data object



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`endpoint_strategy` | whether cf_privacy should overwrite or merge with the caller_id_options of the endpoint. | `string('overwrite' | 'merge')` | `overwrite` | `false` |  
`mode` | set caller privacy on calls, restricting the presentation some or full parts of Caller ID | `string('full' | 'name' | 'number' | 'yes')` | `full` | `false` |  






#### Privacy Modes

Mode | Description
---- | -----------
`full` | anonymize both CIDName and CIDNumber
`name` | anonymize CIDName only
`number` | anonymize CIDNumber only
`yes` | anonymize both CIDName and CIDNumber
