
Here's a run down of the available SUP commands for manipulating ecallmgr!

All commands should be prefixed by `sup -necallmgr ecallmgr_maintenance`. So if the command is listed as `list_fs_nodes`, the full command you would actually run from the command line is `sup -necallmgr ecallmgr_maintenance list_fs_nodes`.

## Managing FreeSWITCH servers

* `add_fs_node freeswitch@some.host.com [AsDefault]`: Instructs ecallmgr to connect to the provided FreeSWITCH server. Set `AsDefault` to `false` (`true` if not specified) to only add the FreeSWITCH node to the local ecallmgr's `system_config` entry. This command is will update the list of FreeSWITCH servers to connect to on ecallmgr startup.
* `remove_fs_node freeswitch@some.host.com [AsDefault]`: disconnect ecallmgr from the provided FreeSWITCH server. Set `AsDefault` to `true` to persist the change to the default list of FreeSWITCH servers; otherwise only the local ecallmgr's config is updated.
* `list_fs_nodes`: Lists the FreeSWITCH servers ecallmgr is currently connected to and interacting with.
* `get_fs_nodes`: Fetches the configured list of FreeSWITCH servers from the AMQP bus.

## Manging ACLs

### Managing Carrier IPs

* `carrier_acls [AsDefault]`: List the known carrier ACLs for the current ecallmgr (or set `AsDefault` to `true` for the default list of ACLs).
* `test_carrier_ip I.P.Add.Ress`: Tests an IP address against all connected FreeSWITCH servers and report if it would be allowed or denied.
* `allow_carrier Name I.P.Add.Ress [AsDefault]`: Allows traffic from the IP address (or [CIDR](https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing)) in the carrier ACL list. `Name` is a human-friendly label to associate with the IP Address.
* `deny_carrier Name I.P.Add.Ress [AsDefault]`: Denies traffic from the IP address (or [CIDR](https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing)) in the carrier ACL list. `Name` is a human-friendly label to associate with the IP Address.

### Managing SBC IPs

These will be your Kamailio IP addresses most of the time

* `sbc_acls [AsDefault]`: List the known SBC ACLs for the current ecallmgr (or set `AsDefault` to `true` for the default list of ACLs).
* `test_sbc_ip I.P.Add.Ress`: Tests an IP address against all connected FreeSWITCH servers and report if it would be allowed or denied.
* `allow_sbc Name I.P.Add.Ress [AsDefault]`: Allows traffic from the IP address (or [CIDR](https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing)) in the SBC ACL list. `Name` is a human-friendly label to associate with the IP Address.
* `deny_sbc Name I.P.Add.Ress [AsDefault]`: Denies traffic from the IP address (or [CIDR](https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing)) in the SBC ACL list. `Name` is a human-friendly label to associate with the IP Address.

### General Commands

* `acl_summary [AsDefault]`: list the ACLs that are currently being fetched from the config
* `remove_acl Name [AsDefault]`: removes the ACL-related information associated with the `Name` label used when allowying/denying an IP address.
* `reload_acls`: Issues a `reloadacl` command to all connected FreeSWITCH servers
* `flush_acls`: Flushes the cached ACLs from the ecallmgr node

## Informational

* `node_summary`: Get a summary of all connected FreeSWITCH nodes.
* `node_details [NodeName]`: Get a detailed report of all connected FreeSWITCH nodes (or just for `NodeName` where `NodeName` is `freeswitch@some.host.com`).
* `channel_summary [NodeName]`: List a summary of known channels, across all connected FreeSWITCH servers (or just for `NodeName`)
* `channel_details [NodeName]`: List details of known channels, across all connected FreeSWITCH servers (or just for `NodeName`)
* `conference_summary [NodeName]`: List a summary report of all conferences on all connected FreeSWITCH servers (or just `NodeName`).
* `conference_details [NodeName]`: List a detailed report of all conferences on all connected FreeSWITCH servers (or just `NodeName`).
* `registrar_summary [SipRealm]`: List known registration summary for all known registrations or just for registrations for `SipRealm`.
* `registrar_details [SipRealm]`: List known registration details for all known registrations or just for registrations for `SipRealm`.
* `registrar_details SipUser SipRealm`: List registration details for `SipUser@SipRealm`


## Sanity Checks

* `sync_channels [NodeName]`: Reconcile ecallmgr's list of channels against known channels on the FreeSWITCH servers. Useful during disconnect/reconnect scenarios if the list isn't reflecting the actual channels up.
* `sync_conferences [NodeName]`: Reconcile ecallmgr's list of conferences against known conferences on the FreeSWITCH servers. Useful during disconnect/reconnect scenarios if the list isn't reflecting the actual conferences up.
* `hangup_long_running_channels [MaxAge]`: Search known channels and hang any up that have been on longer than `MaxAge` seconds.
    * `limit_channel_uptime MaxAge [AsDefault]`: Limit the max age of channels. Set to 0 to disable the check (default).

## Flushing Things

* `flush_node_channels NodeName`: Flush the node's tracked channels in ecallmgr
* `flush_node_conferences NodeName`: Flush the node's tracked conferences in ecallmgr
* `flush_registrar [SipRealm [SipUser]]`: Flushes ecallmgr's registration cache for all registrations, any registration under the `SipRealm`, or a specific registration for `SipUser@SipRealm`
* `flush_authn`: Flushes SIP credentials (used to speed up responses to INVITE/REGISTER attempts)
* `flush_util`: Flushes the util cache, used by authz, origination, and other parts of the code

## Authorization (authz)

* `enable_authz`: Turns on authorization for all ecallmgrs (or at least, those that don't override it in their personal configs).
* `disable_authz`: Turns off authorization for all ecallmgrs
* `enable_local_resource_authz`: Turns on authorization for calls to local resources - Kazoo will still track the channel as if it was using a global resource.
* `disable_local_resource_authz`: Turns off authorization for calls to local resources - Kazoo is not responsible for limiting calls to these local resources.
