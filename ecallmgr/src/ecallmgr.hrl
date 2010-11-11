-record(handler_stats, {lookups_success = 0 :: integer()
			,lookups_failed = 0 :: integer()
                        ,lookups_timeout = 0 :: integer()
                        ,lookups_requested = 0 :: integer()
			,started = {0,0,0} :: tuple(integer(), integer(), integer())
		       }).

-record(node_stats, {started = {0,0,0} :: tuple(integer(), integer(), integer())
		     ,last_heartbeat = {0,0,0} :: tuple(integer(), integer(), integer())
                     ,created_channels = 0 :: integer()
		     ,destroyed_channels = 0 :: integer()
		    }).

-define(DEFAULT_DOMAIN, <<"trunks.2600hz.com">>).
