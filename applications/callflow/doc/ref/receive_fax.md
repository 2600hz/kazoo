## Receive Fax

### About Receive Fax

### Schema

Validator for the receive_fax callflow's data object

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`media` |   | `object` |   | `false`
`media.fax_option` | Caller flag for T38 settings | `string('auto', 'true', 'false'), boolean` |   | `false`
`owner_id` | User ID to send fax to | `string` |   | `false`
