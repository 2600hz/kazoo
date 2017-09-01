### Callflow Privacy

### About Privacy

The `privacy` callflow enables set caller privacy on calls, restricting the presentation some or full parts of Caller ID.

#### Schema

Validator for the privacy callflow's data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`mode` | set caller privacy on calls, restricting the presentation some or full parts of Caller ID | `string('full' | 'name' | 'number' | 'yes')` | `full` | `false`






#### Privacy Modes

Mode | Description
---- | -----------
`full` | anonymize both CIDName and CIDNumber
`name` | anonymize CIDName only
`number` | anonymize CIDNumber only
`yes` | anonymize both CIDName and CIDNumber
