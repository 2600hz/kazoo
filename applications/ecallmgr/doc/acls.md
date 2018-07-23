
ACLs control whether to request username/password authentication from a source IP address or not. Kazoo maintains two lists of ACLs, one for the SBCs (typically Kamailio) and one for upstream carriers to send inbound traffic to Kazoo.

## SBCs

It is important to add your SBC IPs to the proper ACL list so that FreeSWITCH doesn't challenge incoming traffic from those SBCs.

The following SUP commands will allow you to manipulate the SBC ACL list:

* `sup -n ecallmgr ecallmgr_maintenance allow_sbc {SBC_NAME} {SBC_IP}`

    This will accept traffic from {SBC\_IP} without requiring authentication

* `sup -n ecallmgr ecallmgr_maintenance remove_acl {SBC_NAME}`

    This will remove the ACL from the list

* `sup -n ecallmgr ecallmgr_maintenance deny_sbc {SBC_NAME} {SBC_IP}`

    This will deny traffic from {SBC\_IP}, if you need that for some reason

## Carriers

It is important to add your Carrier IPs to the proper ACL list so that FreeSWITCH doesn't challenge incoming traffic from those Carriers. Most carriers do not expect to be challenged.

The following SUP commands will allow you to manipulate the Carrier ACL list:

* `sup -n ecallmgr ecallmgr_maintenance allow_carrier {CARRIER_NAME} {CARRIER_IP}`

    This will accept traffic from {CARRIER\_IP} without requiring authentication

* `sup -n ecallmgr ecallmgr_maintenance remove_acl {CARRIER_NAME}`

    This will remove the ACL from the list

* `sup -n ecallmgr ecallmgr_maintenance deny_carrier {CARRIER_NAME} {CARRIER_IP}`

    This will deny traffic from {CARRIER\_IP}, if you need that for some reason


Note: {CARRIER\_IP} can also be a [CIDR](https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing#CIDR_notation)-formatted entry.

## Update running ACLs

To update all the FreeSWITCH servers' ACL lists:

* `sup -n ecallmgr ecallmgr_maintenance reload_acls`

## Test IP against ACLs

You can test whether an IP would be accepted by the Carrier or SBC ACLs:

* `sup -n ecallmgr ecallmgr_maintenance test_carrier_ip {CARRIER_IP} [{FREESWITCH_NODE}]`
* `sup -n ecallmgr ecallmgr_maintenance test_sbc_ip {SBC_IP} [{FREESWITCH_NODE}]`

If you don't provide a FreeSWITCH node, all connected nodes will be queried. `{FREESWITCH_NODE}` should be formatted as the mod_kazoo node name (`freeswitch@fs.server.com`).
