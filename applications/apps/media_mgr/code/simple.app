{application, 'simple',
 [{description, "A simple application"},
  {vsn,         "1.0"},
  {modules,      [simple,kv,packet_assembler,
                  simple_sup,simple_logger]},
  {maxT,         infinity},
  {registered,   [kv, my_simple_event_logger,
		  my_simple_packet_assembler]},
  {applications, []},
  {included_applications, []},
  {env,          []},
  {mod,          {simple, go}}]}.

