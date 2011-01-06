{application, log_roller, [
    {description, "log_roller"},
    {vsn, "0.4"},
    {modules, [
		erltl,
		expanded_logging,
		log_roller,
		log_roller_h,
		log_roller_logger,
		log_roller_server,
		log_roller_utils,
		lrb,
		lr_cache,
		lr_config,
		lr_filter,
		lr_hooks,
		lr_read_from_disk,
		lr_tail,
		lr_web_server,
		lr_write_to_disk
    ]},
    {registered, []},
    {mod, {log_roller, []}},
    {applications, [kernel, stdlib, dynamic_compile]},
	{start_phases, [{world, []}]}
]}.
