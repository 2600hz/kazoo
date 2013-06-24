{application, ecallmgr
 ,[
   {description, "Manage resource requests and interface with FreeSWITCH"}
   ,{id, "59d9f698-62d6-4be6-937c-e8a2647c9a07"}
   ,{vsn, "0.8.4"}
   ,{modules, [ecallmgr_app, ecallmgr_auxiliary_sup, ecallmgr_call_command, ecallmgr_call_control, ecallmgr_call_control_sup, ecallmgr_call_events, ecallmgr_call_event_sup, ecallmgr_call_sup, ecallmgr_config, ecallmgr, ecallmgr_fs_authn, ecallmgr_fs_authz, ecallmgr_fs_cdr, ecallmgr_fs_channel, ecallmgr_fs_conference, ecallmgr_fs_conferences, ecallmgr_fs_config, ecallmgr_fs_flite, ecallmgr_fs_node, ecallmgr_fs_nodes, ecallmgr_fs_node_sup, ecallmgr_fs_notify, ecallmgr_fs_pinger, ecallmgr_fs_pinger_sup, ecallmgr_fs_resource, ecallmgr_fs_route, ecallmgr_fs_sup, ecallmgr_fs_xml, ecallmgr_init, ecallmgr_maintenance, ecallmgr_originate, ecallmgr_originate_sup, ecallmgr_query, ecallmgr_registrar, ecallmgr_sup, ecallmgr_util, freeswitch, fs_xml]}
   ,{registered, []}
   ,{applications, [
                    kernel
                    ,stdlib
                   ]}
   ,{included_applications, [
                             crypto
                             ,sasl
                             ,lager
                             ,whistle_amqp
                            ]}
   ,{mod, {ecallmgr_app, []}}
   ,{env, [{reloader, false}]}
  ]}.
