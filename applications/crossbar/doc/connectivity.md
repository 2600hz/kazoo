### Connectivity

#### About Connectivity

#### Schema

Trunkstore configuration document - this is old stuff; do not recommend building off this if possible



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`account.auth_realm` | The realm any device in the account will use to authenticate with | `string(1..)` |   | `false` |  
`account.caller_id.cid_name` |   | `string(0..35)` |   | `false` |  
`account.caller_id.cid_number` |   | `string(0..35)` |   | `false` |  
`account.caller_id` |   | `object()` |   | `false` |  
`account.emergency_caller_id.cid_name` |   | `string(0..35)` |   | `false` |  
`account.emergency_caller_id.cid_number` |   | `string(0..35)` |   | `false` |  
`account.emergency_caller_id` |   | `object()` |   | `false` |  
`account.trunks` | The number of two-way trunks this account has purchased | `integer()` |   | `false` |  
`account` | Information that applies to the account as a whole | `object()` |   | `false` |  
`name` | Human-friendly name of the trunkstore account | `string()` |   | `false` |  
`servers.[].DIDs./^\+?\d*$/.caller_id.cid_name` |   | `string(1..35)` |   | `false` |  
`servers.[].DIDs./^\+?\d*$/.caller_id.cid_number` |   | `string(1..35)` |   | `false` |  
`servers.[].DIDs./^\+?\d*$/.caller_id` |   | `object()` |   | `false` |  
`servers.[].DIDs./^\+?\d*$/.failover.e164` | An E.164 formatted DID to dial for failover | `string()` |   | `false` |  
`servers.[].DIDs./^\+?\d*$/.failover.sip` | A SIP URI (sip:user@host) to call for failover | `string()` |   | `false` |  
`servers.[].DIDs./^\+?\d*$/.failover` | Route inbound call to another destination if this server fails to handle the call | `object()` |   | `false` |  
`servers.[].DIDs./^\+?\d*$/.force_outbound` |   | `boolean()` | `false` | `false` |  
`servers.[].DIDs./^\+?\d*$/.options.[]` |   | `string()` |   | `false` |  
`servers.[].DIDs./^\+?\d*$/.options` |   | `array(string())` |   | `false` |  
`servers.[].DIDs./^\+?\d*$/` |   | `object()` |   | `false` |  
`servers.[].DIDs` |   | `object()` |   | `false` |  
`servers.[].auth.auth_method` | What type of auth mechanism to use | `string('password' | 'Password' | 'IP' | 'ip')` | `password` | `true` |  
`servers.[].auth.auth_password` | Password of the user@auth_realm | `string(1..)` |   | `false` |  
`servers.[].auth.auth_user` | Username for authentication | `string(1..)` |   | `false` |  
`servers.[].auth.ip` | IP (sip) address for this device | `string()` |   | `false` |  
`servers.[].auth.port` | Port to send SIP traffic for the remote device | `integer()` |   | `false` |  
`servers.[].auth` |   | `object()` |   | `true` |  
`servers.[].name` | Human-friendly name of the server | `string(1..)` |   | `false` |  
`servers.[].options.caller_id.cid_name` |   | `string(1..35)` |   | `false` |  
`servers.[].options.caller_id.cid_number` |   | `string(1..35)` |   | `false` |  
`servers.[].options.caller_id` |   | `object()` |   | `false` |  
`servers.[].options.delay` | The time, in seconds, to wait before attempting to call the server | `integer()` | `0` | `false` |  
`servers.[].options.enabled` | Is the server ready for sending and receiving calls | `boolean()` | `true` | `false` |  
`servers.[].options.failover.e164` | An E.164 formatted DID to dial for failover | `string()` |   | `false` |  
`servers.[].options.failover.sip` | A SIP URI (sip:user@host) to call for failover | `string()` |   | `false` |  
`servers.[].options.failover` | Route inbound call to another destination if this server fails to handle the call | `object()` |   | `false` |  
`servers.[].options.force_outbound` | If true, will send the call over configured carriers instead of to the server (as opposed to the 'enabled' flag, which will reject the calls) | `boolean()` | `false` | `false` |  
`servers.[].options.ignore_early_media` |   | `boolean()` |   | `false` |  
`servers.[].options.inbound_format` | Determines how the INVITE is sent to the server | `string('e164' | 'npan' | '1npan' | 'username')` | `npan` | `false` |  
`servers.[].options.ip` | IP (sip) address for this device | `string()` |   | `false` |  
`servers.[].options.media_handling` | Determine whether the switch should be in the media path or not | `string('process' | 'bypass')` | `bypass` | `false` |  
`servers.[].options.port` | Port to send SIP traffic for the remote device | `integer()` |   | `false` |  
`servers.[].options.progress_timeout` | The time, in seconds, to wait for the server to progress in the call, before trying an optionally defined failover route or terminating the call | `integer()` |   | `false` |  
`servers.[].options.sip_headers` | List of arbitrary SIP headers to add to the INVITE | `array(object())` |   | `false` |  
`servers.[].options.timeout` | The time, in seconds, to wait for an answer from the server | `integer()` |   | `false` |  
`servers.[].options` |   | `object()` |   | `false` |  
`servers` | What servers will be allowed to make/receive calls via this account | `array(object())` | `[]` | `false` |  



#### Fetch

> GET /v2/accounts/{ACCOUNT_ID}/connectivity

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/connectivity
```
