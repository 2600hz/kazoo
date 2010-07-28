{application, fs_toolkit,
  [{description, "FreeSWITCH Distributed Toolkit"},
  {vsn, "1.0"},
  {modules, [fs_app, fs_toolkit_sup, fs_toolkit_cfg, amqp_server, amqp_event_manager, fs_reload_client]},
  {registered, [fs_toolkit_sup, amqp_server, amqp_event_manager]},
  {applications, [kernel, stdlib]},
  {mod, {fs_app, []}}
]}. 
