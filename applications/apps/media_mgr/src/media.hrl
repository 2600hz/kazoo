-define(CHUNKSIZE, 24576).

-define(APP_NAME, <<"media_srv">>).
-define(APP_VERSION, <<"0.2.0">>).

-define(MEDIA_DB, <<"media_files">>).
-define(PORT_RANGE, 0). % use 0 to have OS assign port #, {Low, Hi} for range of ports to try
-define(PORT_OPTIONS, [binary, {packet,0}, {active,false}, {reuseaddr, true}]).
-define(MAX_RESERVED_PORTS, 10).
-define(MAX_WAIT_FOR_LISTENERS, 600000). %% 600 secs = 10 minutes

-record(media_file, {
	  stream_url = <<>> :: binary()
	 ,contents = <<>> :: binary()
         ,content_type = <<>> :: binary()
         ,media_name = <<>> :: binary()
	 ,chunk_size = ?CHUNKSIZE :: integer()
         ,shout_response = "" :: string()
         ,shout_header = <<>> :: binary()
         ,continuous = false :: boolean()
	 }).
