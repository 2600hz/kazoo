
# System configuration options in ecallmgr

|Key | Type | Default | Description
|--- | ---- | ------- | -----------
|`authz_enabled`|boolean()|`false`|Should ecallmgr authorize the caller
|`authz_local_resources`|boolean()|`false`|Should ecallmgr perform authorization against channels using local account resources
|`authz_dry_run`|boolean()|`false`|If `true`, don't stop the channel from progressing
|`authz_default_action`|`deny`,`allow`|`deny`|If authz lookup fails or isn't configured to run, what should be the default action
|`inbound_rate_resp_timeout`|integer()|10000|Time, in milliseconds, to wait for a rate response for inbound channels
|`outbound_rate_resp_timeout`|integer()|10000|Time, in milliseconds, to wait for a rate response for outbound channels
|`inbound_rate_required`|boolean()|`false`|Is a rate required for the inbound call to proceed
|`outbound_rate_required`|boolean()|`false`|Is a rate required for the outbound call to proceed
|`node_down_grace_period`|integer()|10000|Time, in milliseconds, ecallmgr will flush the internal channels cache of channels from the down FreeSWITCH node
|`fs_nodes`|[string()]|\[]|The list of FreeSWITCH servers to connect to when ecallmgr starts
|`acls`|json\_object()|`{}`|The list carrier/SBC/endpoint ACLs to load into FreeSWITCH
|`send_registrar_notifications`|boolean()|`true`|Should ecallmgr figure out which registrar is the oldest and send a register update if the local ecallmgr is the oldest
|`default_fax_extension`|string()|`.tiff`|What extension fax files should be stored with on the local FreeSWITCH disk
|`default_recording_extension`|string()|`.mp3`|What extension recording files should be stored with on the local FreeSWITCH disk
|`fax_file_path`|string()|`/tmp/`|Where on the local FreeSWITCH disk should faxes be temporarily stored
|`recording_file_path`|string()|`/tmp/`|Where on the local FreeSWITCH disk should recordings be temporarily stored
|`use_vlc`|boolean()|`false`|Toggle experimental usage of mod_vlc for media streaming
|`use_shout`|boolean()|`false`|Toggle usage of mod_shout for media streaming
|`use_http_cache`|boolean()|`true`|Toggle usage of mod_http_cache for media streaming
|`expires_deviation_time`|integer()|180|Time, in seconds, to "fudge" registration expiration with
|`user_cache_time_in_ms`|integer()|3600000 (one hour)|Time, in milliseconds, to cache user XML
|`record_waste_resources`|boolean()|`false`|Should recording waste resources by sending RTP (see [FreeSWITCH wiki](http://wiki.freeswitch.org/wiki/Variable_record_waste_resources)
|`sofia_conf`|boolean()|`false`|Should ecallmgr fetch Sofia's XML config from Kazoo
|`fs_profiles`|json\_object()|`{}`|The FreeSWITCH profiles to load
|`process_gateways`|boolean()|`false`|Should ecallmgr fetch SIP gateways
|`gateways`|json\_object()|`{}`|Sofia Gateway definitions
|`fs_node_uptime_s`|integer()|600|How much uptime, in seconds, before a FreeSWITCH server is considered "up"
|`fetch_timeout`|integer()|2600|How long, in milliseconds, to wait for fetch API requests to respond. Should be less than mod_kazoo waits for ecallmgr.
|`fs_cmds_wait_ms`|integer()|5000|How long, in milliseconds, to wait for start commands to be executed on a connecting FreeSWITCH node
|`fs_cmds`|json\_object()|`[{"load":"mod_sofia"},{"reloadacl":""}]`|What commands to run on a FreeSWITCH server when connecting for the first time
|`fs_reconnect_cmds`|json\_objects()|`{}`|What commands to run on a FreeSWITCH server when reconnecting (falls back to `fs_cmds` if none exist
|`capabilities`|json\_object()|`{}`|Read-only, result of probing FreeSWITCH for what capabilities are loaded
|`restrict_channel_state_publisher`|boolean()|`false`|If `true` only the "handling" ecallmgr will publish channel events for a given call
|`tcp_packet_type`|integer()|2|TCP packet type for how data is encoded between ecallmgr and mod\_kazoo
|`event_stream_idle_alert`|integer()|0|Time, in seconds, to consider an event stream idle and restart it (30s or less is considered "infinity")
|`redirect_via_proxy`|boolean()|`true`|When moving a channel to another FreeSWITCH server, do so via an SBC/proxy
|`freeswitch_context`|string()|`context_2`|The default FreeSWITCH context
|`default_realm`|string()|`nodomain.com`|The default realm when the realm is missing
|`max_timeout_for_node_restart`|integer()|10000|Time, in milliseconds, to consider a node down (allows nodes to come up after momentary blip)
|`max_channel_cleanup_timeout_ms`|integer()|60000|Time, in milliseconds, to check for too-old channels and remove them
|`publish_channel_reconnect`|boolean()|`false`|Whether to publish a call event related to a node reconnecting (apps may need to check for their channel if the CHANNEL_DESTROY was missed)
|`max_channel_update_s`|integer()|0|Limit, in seconds, to channel duration (0 for no limit). Channels exceeding this limit will be killed.
|`should_detect_inband_dtmf`|boolean()|`false`|Whether to start the DTMF detection if telephone-event 101 isn't offered in the SDP.
