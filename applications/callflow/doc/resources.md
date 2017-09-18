# Resources

Route the caller to external resources (typically upstream providers).

## Data Schema

Validator for the resources callflow's data object

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`bypass_e164` | Use the original requested number instead of normalizing to E164 | `boolean` |   | `false`
`caller_id_type` | Which configured caller-id to use (key in the 'caller_id' object) | `string` | `external` | `false`
`custom_sip_headers` | Custom SIP Headers to include on the outgoing INVITE | `object` |   | `false`
`do_not_normalize` | Use the original requested number instead of normalizing; otherwise try to apply the endpoint's dialplan to the requested number | `boolean` |   | `false`
`dynamic_flags` | List of function names that are called on the Call record or special dynamic flags populate the 'flags' array sent to the resource(s) for matching | `array()` |   | `false`
`emit_account_id` | Toggles whether to put the account id in the SIP packets | `boolean` |   | `false`
`format_from_uri` | If true, puts the account realm in the From header | `boolean` |   | `false`
`from_uri_realm` | Override the From realm in the SIP packets | `string` |   | `false`
`hunt_account_id` | When using local resources, use this account instead of the account making the call (useful for resellers) | `string` |   | `false`
`ignore_early_media` | Toggle whether to ignore early media | `boolean` | `false` | `false`
`outbound_flags` | List of flags to use when matching resources to route the call | `array()` | `[]` | `false`
`ringback` | Tone or file to play while waiting for the leg to be answered | `string` |   | `false`
`timeout` | How long, in seconds, to wait for the call to be answered | `integer` |   | `false`
`to_did` | Statically set the DID to dial | `string` |   | `false`
`use_local_resources` | Toggle whether to use the account's (or hunt_account_id's) resources vs the system resources | `boolean` | `true` | `false`

## Special Dynamic Flags

### Zone
Adding the value `"zone"` to the `dynamic_flags` array will result in a flag set to the zone name that is executing the call.

### Custom Channel Vars
Adding the value `"custom_channel_vars."` appended with a normalized variable name will add the value (if present) to the flags array.  For example, `"custom_channel_vars.owner_id"`.

## Examples

### Route to the system carriers

```json
{"module":"resources",
 "data":{
   "use_local_resources":false
 }
}
```

### Route to the account's local carriers

```json
{"module":"resources",
 "data":{
   "use_local_resources":true
 }
}
```

### Route to another account's local carriers

```json
{"module":"resources",
 "data":{
   "use_local_resources":true,
   "hunt_account_id":"{OTHER_ACCOUNT_ID}"
 }
}
```

This is great for resellers; they can set their reseller account id as the `hunt_account_id` in callflows for their child accounts (such as in the `no_match` callflow).

### Dynamic Flags

```json
{"module":"resources",
 "data":{
   "dynamic_flags": ["custom_channel_vars.owner_id", "zone"]
 }
}
```

This will add the zone name processing the call and owner ID of the endpoint to the required flags of the resource requests.  In this example only resources for that zone and owner will be selected (the resources would need the corresponding values set as available flags).

### Custom SIP Headers

You can include a couple macros that will be replaced at the time of a call in the SIP headers. For instance, if you need to send the Caller-ID number as an "X-" SIP header:

```json
{"module":"resources",
 "data":{
   "use_local_resources":true,
   "hunt_account_id":"{OTHER_ACCOUNT_ID}",
   "custom_sip_headers":{
     "X-Billing-Number":"{caller_id_number}"
   }
 }
}
```

By using `{caller_id_number}`, Kazoo will replace the header value with the actual Caller-ID number for that call. If the number was `4158867900` then the SIP INVITE would include the header `X-Billing-Number: 4158867900`.

You can find the listing of available macros in the `/v2/schemas/ecallmgr` API call; look for the `expandable_macros` defaults.
