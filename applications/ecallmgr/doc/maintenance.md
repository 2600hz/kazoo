
Here's a run down of the available SUP commands for manipulating ecallmgr!

All commands should be prefixed by `sup -n ecallmgr ecallmgr_maintenance`. So if the command is listed as `list_fs_nodes`, the full command you would actually run from the command line is `sup -n ecallmgr ecallmgr_maintenance list_fs_nodes`.

## Managing FreeSWITCH servers

* `add_fs_node freeswitch@some.host.com [{AS_DEFAULT}]`: Instructs ecallmgr to connect to the provided FreeSWITCH server. Set `{AS_DEFAULT}` to `false` (`true` if not specified) to only add the FreeSWITCH node to the local ecallmgr's `system_config` entry. This command is will update the list of FreeSWITCH servers to connect to on ecallmgr startup.
* `remove_fs_node freeswitch@some.host.com [{AS_DEFAULT}]`: disconnect ecallmgr from the provided FreeSWITCH server. Set `{AS_DEFAULT}` to `true` to persist the change to the default list of FreeSWITCH servers; otherwise only the local ecallmgr's config is updated.
* `list_fs_nodes`: Lists the FreeSWITCH servers ecallmgr is currently connected to and interacting with.
* `get_fs_nodes`: Fetches the configured list of FreeSWITCH servers from the AMQP bus.

## Managing ACLs

### Managing Carrier IPs

* `carrier_acls [{AS_DEFAULT}]`: List the known carrier ACLs for the current ecallmgr (or set `{AS_DEFAULT}` to `true` for the default list of ACLs).
* `test_carrier_ip I.P.Add.Ress`: Tests an IP address against all connected FreeSWITCH servers and report if it would be allowed or denied.
* `allow_carrier Name I.P.Add.Ress [{AS_DEFAULT}]`: Allows traffic from the IP address (or [CIDR](https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing)) in the carrier ACL list. `Name` is a human-friendly label to associate with the IP Address.
* `deny_carrier Name I.P.Add.Ress [{AS_DEFAULT}]`: Denies traffic from the IP address (or [CIDR](https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing)) in the carrier ACL list. `Name` is a human-friendly label to associate with the IP Address.

### Managing SBC IPs

These will be your Kamailio IP addresses most of the time

* `sbc_acls [{AS_DEFAULT}]`: List the known SBC ACLs for the current ecallmgr (or set `{AS_DEFAULT}` to `true` for the default list of ACLs).
* `test_sbc_ip I.P.Add.Ress`: Tests an IP address against all connected FreeSWITCH servers and report if it would be allowed or denied.
* `allow_sbc Name I.P.Add.Ress [{AS_DEFAULT}]`: Allows traffic from the IP address (or [CIDR](https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing)) in the SBC ACL list. `Name` is a human-friendly label to associate with the IP Address.
* `deny_sbc Name I.P.Add.Ress [{AS_DEFAULT}]`: Denies traffic from the IP address (or [CIDR](https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing)) in the SBC ACL list. `Name` is a human-friendly label to associate with the IP Address.

### General Commands

* `acl_summary [{AS_DEFAULT}]`: list the ACLs that are currently being fetched from the config
* `remove_acl Name [{AS_DEFAULT}]`: removes the ACL-related information associated with the `Name` label used when allowing/denying an IP address.
* `reload_acls`: Issues a `reloadacl` command to all connected FreeSWITCH servers
* `flush_acls`: Flushes the cached ACLs from the ecallmgr node

## Informational

* `node_summary`: Get a summary of all connected FreeSWITCH nodes.
* `node_details [{NODE_NAME}]`: Get a detailed report of all connected FreeSWITCH nodes (or just for `{NODE_NAME}` where `{NODE_NAME}` is `freeswitch@some.host.com`).
* `channel_summary [{NODE_NAME}]`: List a summary of known channels, across all connected FreeSWITCH servers (or just for `{NODE_NAME}`)
* `channel_details [{NODE_NAME}]`: List details of known channels, across all connected FreeSWITCH servers (or just for `{NODE_NAME}`)
* `conference_summary [{NODE_NAME}]`: List a summary report of all conferences on all connected FreeSWITCH servers (or just `{NODE_NAME}`).
* `conference_details [{NODE_NAME}]`: List a detailed report of all conferences on all connected FreeSWITCH servers (or just `{NODE_NAME}`).
* `registrar_summary [{SIP_REALM}]`: List known registration summary for all known registrations or just for registrations for `{SIP_REALM}`.
* `registrar_details [{SIP_REALM}]`: List known registration details for all known registrations or just for registrations for `{SIP_REALM}`.
* `registrar_details {SIP_USER} {SIP_REALM}`: List registration details for `SipUser@SipRealm`


## Sanity Checks

* `sync_channels [{NODE_NAME}]`: Reconcile ecallmgr's list of channels against known channels on the FreeSWITCH servers. Useful during disconnect/reconnect scenarios if the list isn't reflecting the actual channels up.
* `sync_conferences [{NODE_NAME}]`: Reconcile ecallmgr's list of conferences against known conferences on the FreeSWITCH servers. Useful during disconnect/reconnect scenarios if the list isn't reflecting the actual conferences up.
* `hangup_long_running_channels [{MAX_AGE}]`: Search known channels and hang any up that have been on longer than `{MAX_AGE}` seconds.
    * `limit_channel_uptime {MAX_AGE} [{AS_DEFAULT}]`: Limit the max age of channels. Set to 0 to disable the check (default).

## Flushing Things

* `flush_node_channels {NODE_NAME}`: Flush the node's tracked channels in ecallmgr
* `flush_node_conferences {NODE_NAME}`: Flush the node's tracked conferences in ecallmgr
* `flush_registrar [{SIP_REALM} [{SIP_USER}]]`: Flushes ecallmgr's registration cache for all registrations, any registration under the `{SIP_REALM}`, or a specific registration for `SipUser@SipRealm`
* `flush_authn`: Flushes SIP credentials (used to speed up responses to INVITE/REGISTER attempts)
* `flush_util`: Flushes the util cache, used by authz, origination, and other parts of the code

## Authorization (authz)

* `enable_authz`: Turns on authorization for all ecallmgrs (or at least, those that don't override it in their personal configs).
* `disable_authz`: Turns off authorization for all ecallmgrs
* `enable_local_resource_authz`: Turns on authorization for calls to local resources - Kazoo will still track the channel as if it was using a global resource.
* `disable_local_resource_authz`: Turns off authorization for calls to local resources - Kazoo is not responsible for limiting calls to these local resources.
