
# System configuration options in ecallmgr

Schema for ecallmgr system_config



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`acl_request_timeout_fudge_ms` | ecallmgr acl_request_timeout_fudge_ms | `integer()` | `100` | `false` |
`acl_request_timeout_ms` | ecallmgr acl_request_timeout_ms | `integer()` | `2000` | `false` |
`acls` | ecallmgr acls | `object()` | `{}` | `false` |
`allow_endless_recording` | ecallmgr allow_endless_recording | `boolean()` | `false` | `false` |
`authz_default_action` | ecallmgr authz default action | `string()` | `deny` | `false` |
`authz_dry_run` | ecallmgr authz dry run | `boolean()` | `false` | `false` |
`authz_enabled` | ecallmgr authz enabled | `boolean()` | `false` | `false` |
`authz_local_resources` | ecallmgr authz local resources | `boolean()` | `false` | `false` |
`balance_crawler_cycle_ms` | ecallmgr balance crawler cycle in milliseconds | `integer()` | `60000` | `false` |
`balance_crawler_enabled` | ecallmgr balance crawler enabled | `boolean()` | `false` | `false` |
`balance_crawler_fetch_timeout_ms` | ecallmgr balance crawler fetch timeout in milliseconds | `integer()` | `10000` | `false` |
`balance_crawler_interaccount_delay_ms` | ecallmgr balance crawler interaccount delay in milliseconds | `integer()` | `10` | `false` |
`call_routing_bindings.[]` |   | `string()` |   | `false` |
`call_routing_bindings` | ecallmgr call routing bindings | `array(string())` | `["context_2"]` | `false` |
`capabilities` | ecallmgr capabilities | `array(object())` | `["{"capability":"conference","is_loaded":false,"module":"mod_conference"}", "{"capability":"channel_move","is_loaded":false,"module":"mod_channel_move"}", "{"capability":"http_cache","is_loaded":false,"module":"mod_http_cache"}", "{"capability":"dialplan","is_loaded":false,"module":"mod_dptools"}", "{"capability":"sip","is_loaded":false,"module":"mod_sofia"}", "{"capability":"fax","is_loaded":false,"module":"mod_spandsp"}", "{"capability":"tts","is_loaded":false,"module":"mod_flite"}", "{"capability":"freetdm","is_loaded":false,"module":"mod_freetdm"}", "{"capability":"skype","is_loaded":false,"module":"mod_skypopen"}", "{"capability":"xmpp","is_loaded":false,"module":"mod_dingaling"}", "{"capability":"skinny","is_loaded":false,"module":"mod_skinny"}", "{"capability":"sms","is_loaded":false,"module":"mod_sms"}"]` | `false` |
`debug_channel` | ecallmgr debug channel | `boolean()` | `false` | `false` |
`default_fax_extension` | ecallmgr default fax extension | `string()` | `.tiff` | `false` |
`default_realm` | ecallmgr default realm | `string()` | `nodomain.com` | `false` |
`default_recording_extension` | ecallmgr default recording extension | `string()` | `.mp3` | `false` |
`default_ringback` | ecallmgr default ringback | `string()` | `%(2000,4000,440,480)` | `false` |
`event_stream_idle_alert` | ecallmgr event stream idle alert | `integer()` | `0` | `false` |
`expandable_macros` | macros that will be expanded at call-time, for use in custom SIP headers | `object()` | `{"{reseller_id}":"${ecallmgr_Reseller-ID}","{caller_id_number}":"${caller_id_number}","{caller_id_name}":"${caller_id_name}","{billing_id}":"${ecallmgr_Billing-ID}","{account_id}":"${ecallmgr_Account-ID}"}` | `false` |
`expires_deviation_time` | ecallmgr expires deviation time | `integer()` | `180` | `false` |
`failover_when_all_unreg` | failover only when all devices are offline | `boolean()` | `false` | `false` |
`fax_file_path` | ecallmgr fax file path | `string()` | `/tmp/` | `false` |
`fetch_timeout` | ecallmgr fetch timeout | `integer()` | `2600` | `false` |
`freeswitch_context` | ecallmgr freeswitch context | `string()` | `context_2` | `false` |
`fs_cmds` | ecallmgr fs cmds | `array(object())` | `["{"load":"mod_sofia"}", "{"reloadacl":""}"]` | `false` |
`fs_cmds_wait_ms` | ecallmgr fs cmds wait in milliseconds | `integer()` | `5000` | `false` |
`fs_node_uptime_s` | ecallmgr fs node uptime in seconds | `integer()` | `600` | `false` |
`fs_nodes.[]` |   | `string()` |   | `false` |
`fs_nodes` | ecallmgr fs nodes | `array(string())` | `[]` | `false` |
`fs_profiles` | ecallmgr fs profiles | `object()` | `{}` | `false` |
`fs_reconnect_cmds` | ecallmgr fs reconnect cmds | `array(object())` |   | `false` |
`gateways` | ecallmgr gateways | `object()` | `{}` | `false` |
`insert_fetched_registration_locally` | ecallmgr insert fetched registration locally | `boolean()` | `false` | `false` |
`max_channel_cleanup_timeout_ms` | ecallmgr maximum channel cleanup timeout in milliseconds | `integer()` | `60000` | `false` |
`max_channel_uptime_s` | ecallmgr maximum channel uptime in seconds | `integer()` | `0` | `false` |
`max_timeout_for_node_restart` | ecallmgr maximum timeout for node restart | `integer()` | `10000` | `false` |
`multivar_separator` | ecallmgr multivar_separator | `string()` | `~` | `false` |
`network_map` | ecallmgr network map | `object()` | `{}` | `false` |
`node_commands` |   |   |   | `false` |
`node_down_grace_period` | ecallmgr node down grace period | `integer()` | `10000` | `false` |
`process_gateways` | ecallmgr process gateways | `boolean()` | `false` | `false` |
`publish_channel_reconnect` | ecallmgr publish channel reconnect | `boolean()` | `false` | `false` |
`publish_channel_state` | ecallmgr publish channel state | `boolean()` | `true` | `false` |
`publish_conference_event.[]` |   | `string()` |   | `false` |
`publish_conference_event` | ecallmgr publish conference event | `array(string())` | `["conference-create", "conference-destroy", "lock", "unlock", "add-member", "del-member", "stop-talking", "start-talking", "mute-member", "unmute-member", "deaf-member", "undeaf-member"]` | `false` |
`record_sample_rate` | ecallmgr record sample rate | `integer()` | `8000` | `false` |
`record_stereo_sample_rate` | ecallmgr record stereo sample rate | `integer()` | `16000` | `false` |
`record_waste_resources` | ecallmgr record waste resources | `boolean()` | `false` | `false` |
`recording_file_path` | ecallmgr recording file path | `string()` | `/tmp/` | `false` |
`recording_software_name` | ecallmgr recording software name | `string()` | `2600Hz, Inc.'s Kazoo` | `false` |
`redirect_via_proxy` | ecallmgr redirect via proxy | `boolean()` | `true` | `false` |
`restrict_channel_state_publisher` | ecallmgr restrict channel state publisher | `boolean()` | `false` | `false` |
`sanitize_fs_value_regex` | ecallmgr sanitize_fs_value_regex | `string()` | `[^0-9\w\s-]` | `false` |
`should_detect_inband_dtmf` | ecallmgr should detect inband dtmf | `boolean()` | `false` | `false` |
`sofia_conf` | ecallmgr sofia conf | `boolean()` |   | `false` |
`tcp_packet_type` | ecallmgr tcp packet type | `integer()` | `2` | `false` |
`text_routing_bindings.[]` |   | `string()` |   | `false` |
`text_routing_bindings` | ecallmgr text routing bindings | `array(string())` | `["context_2"]` | `false` |
`use_bypass_media_after_bridge` | ecallmgr use bypass media after bridge | `boolean()` | `false` | `false` |
`use_http_cache` | ecallmgr use http cache | `boolean()` | `true` | `false` |
`use_shout` | ecallmgr use shout | `boolean()` | `false` | `false` |
`use_vlc` | ecallmgr use vlc | `boolean()` | `false` | `false` |
`user_cache_time_in_ms` | ecallmgr user cache time in in milliseconds | `integer()` | `3600000` | `false` |


## `failover_when_all_unreg`

When a list of endpoints is passed to ecallmgr for bridging (say via a ring group or a user's owned devices), the listing can optionally include failover endpoints to be dialed if a user's normal devices are offline.

When this flag is enabled, the provided SIP endpoints are checked for active registrations; if no SIP endpoints remain after filtering those endpoints missing registrations, the failover endpoint(s) are used instead.

### User callflow

When a user has one or more SIP endpoints and has also configured failover (by setting `"call_forward.failover":true` plus the call forwarding details), a bridge command will be sent to ecallmgr with the SIP device(s) and the failover endpoint. When ecallmgr goes to build the bridge string for FreeSWITCH, the SIP endpoint statuses will be checked; if all SIP endpoints are missing registrations, the failover endpoint will be used. If any SIP endpoint is registered, the failover endpoint is omitted from the bridge string.

### Ring Group callflow

When KAZOO builds a ring group endpoint list, it resolves all group and user IDs into their owned device IDs. That list of endpoints is then sent to ecallmgr for bridging.

This means that if any SIP device from the resulting endpoint list has an active registration, *all* failover endpoints will be filtered out and not dialed. So if a user has no online SIP endpoints and has failover configured, their failover endpoint will *not* ring if any other SIP endpoint in the ring group is actively registered.

### "Normal" call forwarded endpoints

If a user or ring group has "normal" call-forwarded endpoints included, since these endpoints don't maintain a registration (since they typically are forwarded to another DID), then they aren't ever "offline" from KAZOO's perspective. Bear this in mind when failover endpoints aren't ringing when SIP endpoints are unregistered.
