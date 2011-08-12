-include_lib("whistle/include/wh_types.hrl"). % get the whistle types
-include_lib("couchbeam/include/couchbeam.hrl").
-include_lib("whistle/include/wh_log.hrl").

-record(design_data, {
	  db_name = <<>> :: binary() %% the actual DB name, encoded (/ -> %2f)
	 ,design_name = <<>> :: binary()
	 ,node = undefined :: atom()
         ,shards = [] :: list(binary()) | []
	 ,disk_size = 0 :: non_neg_integer()
         ,data_size = 0 :: non_neg_integer()
	 ,conn = #server{} :: #server{}
	 ,admin_conn = #server{} :: #server{}
	 ,do_compaction = false :: boolean()
	 }).
-record(db_data, {
	  db_name = <<>> :: binary() %% the shard name
	 ,node = undefined :: atom()
	 ,disk_size = 0 :: non_neg_integer()
	 ,data_size = 0 :: non_neg_integer()
	 ,conn = #server{} :: #server{}
	 ,admin_conn = #server{} :: #server{}
	 ,do_compaction = false :: boolean()
	 }).

-define(TIMEOUT, 1000 * 60 * 60). %% check every hour

-define(COMPACT_THRESHOLD, 500). 
-define(MIN_DISK_SIZE, 131072).
-define(DEFAULT_PORT, 5984).
-define(DEFAULT_ADMIN_PORT, 5986).
-define(IBROWSE_OPTS, [{max_sessions, 1024}, {max_pipeline_size, 10}]).
