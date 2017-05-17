## Dynamic Cid

### About Dynamic Cid

#### Schema

Validator for the dynamic_cid callflow's data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | What action to perform | `string('manual', 'list')` |   | `false`
`caller_id` | Static Caller ID settings | `object` |   | `false`
`caller_id.name` | Caller ID Name | `string` |   | `false`
`caller_id.number` | Caller ID Number | `string` |   | `false`
`enforce_call_restriction` | Check classification restrictions against endpoint | `boolean` | `true` | `false`
`id` | List ID for caller IDs when 'action' is 'list' | `string` |   | `false`
`idx_name` | Named capture group to use | `string` |   | `false`
`interdigit_timeout` | How long, in seconds, to wait for keypresses | `integer` |   | `false`
`max_digits` | Max number of digits allowed when collecting Caller ID Number | `integer` |   | `false`
`media_id` | Prompt to play to caller to enter Caller ID Number | `string` |   | `false`
`min_digits` | Minimum number of digits that must match the regex to collected DTMF | `integer` |   | `false`
`whitelist_regex` | Regex to match collected Caller ID Number | `string` |   | `false`


