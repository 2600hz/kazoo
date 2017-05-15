## Resources

### About Resources

#### Schema

Validator for the resources callflow's data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`bypass_e164` | Use the original requested number instead of normalizing to E164 | `boolean` |   | `false`
`caller_id_type` | Which configured caller-id to use (key in the 'caller_id' object) | `string` | `external` | `false`
`custom_sip_headers` | Custom SIP Headers to include on the outgoing INVITE | `object` |   | `false`
`do_not_normalize` | Use the original requested number instead of normalizing; otherwise try to apply the endpoint's dialplan to the requested number | `boolean` |   | `false`
`dynamic_flags` | List of function names (or 'zone') that are called on the Call record to populate the 'flags' array sent to the resource(s) for matching | `array(string)` |   | `false`
`dynamic_flags.[]` |   | `string` |   | `false`
`emit_account_id` | Toggles whether to put the account id in the SIP packets | `boolean` |   | `false`
`format_from_uri` | If true, puts the account realm in the From header | `boolean` |   | `false`
`from_uri_realm` | Override the From realm in the SIP packets | `string` |   | `false`
`hunt_account_id` | When using local resources, use this account instead of the account making the call (useful for resellers) | `string` |   | `false`
`ignore_early_media` | Toggle whether to ignore early media | `boolean` | `false` | `false`
`outbound_flags` | List of flags to use when matching resources to route the call | `array(string)` | `[]` | `false`
`outbound_flags.[]` |   | `string` |   | `false`
`ringback` | Tone or file to play while waiting for the leg to be answered | `string` |   | `false`
`timeout` | How long, in seconds, to wait for the call to be answered | `integer` |   | `false`
`to_did` | Statically set the DID to dial | `string` |   | `false`
`use_local_resources` | Toggle whether to use the account's (or hunt_account_id's) resources vs the system resources | `boolean` | `true` | `false`


