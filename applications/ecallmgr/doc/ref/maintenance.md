## SUP-able functions

| Function | Arguments | Description |
| -------- | --------- | ----------- |
| `acl_summary/0` |  | |
| `acl_summary/1` | `(false) | (true) | (AsDefault)` | |
| `add_fs_node/1` | `(FSNode)` | |
| `add_fs_node/2` | `(FSNode,false) | (FSNode,true) | (FSNode,AsDefault)` | |
| `allow_carrier/1` | `(Name)` | |
| `allow_carrier/2` | `(Name,IP)` | |
| `allow_carrier/3` | `(Name,IP,false) | (Name,IP,true) | (Name,IP,AsDefault)` | |
| `allow_sbc/1` | `(Name)` | |
| `allow_sbc/2` | `(Name,IP)` | |
| `allow_sbc/3` | `(Name,IP,false) | (Name,IP,true) | (Name,IP,AsDefault)` | |
| `carrier_acls/0` |  | |
| `carrier_acls/1` | `(false) | (true) | (AsDefault)` | |
| `channel_details/0` |  | |
| `channel_details/1` | `(UUID)` | |
| `channel_summary/0` |  | |
| `channel_summary/1` | `(Node)` | |
| `check_sync/2` | `(Username,Realm)` | |
| `conference_details/0` |  | |
| `conference_details/1` | `(UUID)` | |
| `conference_summary/0` |  | |
| `conference_summary/1` | `(Node)` | |
| `deny_carrier/1` | `(Name)` | |
| `deny_carrier/2` | `(Name,IP)` | |
| `deny_carrier/3` | `(Name,IP,false) | (Name,IP,true) | (Name,IP,AsDefault)` | |
| `deny_sbc/1` | `(Name)` | |
| `deny_sbc/2` | `(Name,IP)` | |
| `deny_sbc/3` | `(Name,IP,false) | (Name,IP,true) | (Name,IP,AsDefault)` | |
| `disable_authz/0` |  | |
| `disable_local_resource_authz/0` |  | |
| `enable_authz/0` |  | |
| `enable_local_resource_authz/0` |  | |
| `flush_acls/0` |  | |
| `flush_authn/0` |  | |
| `flush_node_channels/1` | `(Node)` | |
| `flush_node_conferences/1` | `(Node)` | |
| `flush_registrar/0` |  | |
| `flush_registrar/1` | `(Realm)` | |
| `flush_registrar/2` | `(Username,Realm)` | |
| `flush_util/0` |  | |
| `get_fs_nodes/0` |  | |
| `hangup/1` | `(UUID)` | |
| `hangup_long_running_channels/0` |  | |
| `hangup_long_running_channels/1` | `(MaxAge)` | |
| `limit_channel_uptime/1` | `(MaxAge)` | |
| `limit_channel_uptime/2` | `(MaxAge,AsDefault)` | |
| `list_fs_nodes/0` |  | |
| `node_details/0` |  | |
| `node_details/1` | `(NodeName)` | |
| `node_summary/0` |  | |
| `registrar_details/0` |  | |
| `registrar_details/1` | `(Realm)` | |
| `registrar_details/2` | `(Username,Realm)` | |
| `registrar_summary/0` |  | |
| `registrar_summary/1` | `(Realm)` | |
| `registrar_sync/0` |  | |
| `reload_acls/0` |  | |
| `remove_acl/1` | `(Name)` | |
| `remove_acl/2` | `(Name,false) | (Name,true) | (Name,AsDefault)` | |
| `remove_fs_node/1` | `(FSNode)` | |
| `remove_fs_node/2` | `(FSNode,false) | (FSNode,true) | (FSNode,AsDefault)` | |
| `sbc_acls/0` |  | |
| `sbc_acls/1` | `(false) | (true) | (AsDefault)` | |
| `show_calls/0` |  | |
| `show_channels/0` |  | |
| `sync_channels/0` |  | |
| `sync_channels/1` | `(Node)` | |
| `sync_conferences/0` |  | |
| `sync_conferences/1` | `(Node)` | |
| `test_carrier_ip/1` | `(IP)` | |
| `test_carrier_ip/2` | `(IP,Node) | (IP,_) | (_,[])` | |
| `test_sbc_ip/1` | `(IP)` | |
| `test_sbc_ip/2` | `(IP,Node) | (IP,_) | (_,[])` | |
