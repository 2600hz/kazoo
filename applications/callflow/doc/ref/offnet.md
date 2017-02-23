## Offnet

### About Offnet

### Schema

Validator for the offnet callflow's data object

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`bypass_e164` |  | `boolean` |   | `false`
`caller_id_type` |  | `string` | `external` | `false`
`custom_sip_headers` |  | `object` |   | `false`
`do_not_normalize` |  | `boolean` |   | `false`
`dynamic_flags` |  | `array()` |   | `false`
`emit_account_id` |  | `boolean` |   | `false`
`format_from_uri` |  | `boolean` |   | `false`
`from_uri_realm` |  | `string` |   | `false`
`hunt_account_id` |  | `string` |   | `false`
`ignore_early_media` |  | `boolean` | `false` | `false`
`outbound_flags` |  | `array()` | `[]` | `false`
`ringback` |  | `string` |   | `false`
`timeout` |  | `integer` |   | `false`
`to_did` |  | `string` |   | `false`
`use_local_resources` |  | `boolean` | `true` | `false`
