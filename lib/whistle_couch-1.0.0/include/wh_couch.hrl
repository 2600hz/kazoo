-record(design_data, {
	  db_name = <<>> :: binary()
	 ,design_name = <<>> :: binary()
         ,shards = [] :: list(binary()) | []
	 ,disk_size = 0 :: non_neg_integer()
         ,data_size = 0 :: non_neg_integer()
	 }).
-record(db_data, {
	  db_name = <<>> :: binary()
	 ,disk_size = 0 :: non_neg_integer()
	 ,data_size = 0 :: non_neg_integer()
	 }).

-define(TIMEOUT, 1000 * 60 * 60). %% check every hour

-define(COMPACT_THRESHOLD, 500). 
-define(MIN_DISK_SIZE, 131072).
