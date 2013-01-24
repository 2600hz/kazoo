-ifndef(WH_COUCH_HRL).
-include_lib("whistle/include/wh_types.hrl"). % get the whistle types
-include_lib("couchbeam/include/couchbeam.hrl").
-include_lib("whistle/include/wh_log.hrl").

-define(TIMEOUT, 1000 * 60 * 60). %% check every hour

-define(COMPACT_THRESHOLD, 500). 
-define(MIN_DISK_SIZE, 131072).
-define(DEFAULT_PORT, 5984).
-define(DEFAULT_ADMIN_PORT, 5986).
-define(IBROWSE_OPTS, [{max_sessions, 512}, {max_pipeline_size, 10}, {connect_timeout, 100}]).

-define(CONFIG_FILE_PATH, [code:priv_dir(whistle_couch), "/startup.config"]).

-define(WH_COUCH_CACHE, whistle_couch_cache).

-define(FIXTURES_FOLDER, "fixtures").

-type couchbeam_errors() :: 'not_found' | 'conflict' | 'failed' |
                            'precondition_failed' | 'db_not_reachable' |
                            'db_not_found' | 'worker_is_dead' | 'invalid_db_name' |
                            'unknown_req_id' |
                            'req_timedout' | 'sel_conn_closed' |
                            'invalid_view_name' |
                            {integer(), term()} | % {error_code, body}
                            integer() |
                            {'EXIT', term()} |
                            {'url_parsing_failed', term()} |
                            {'ok', string(), _, _}.
-type couchbeam_error() :: {'error', couchbeam_errors()}.

-record(design_data, {
          db_name = <<>> :: binary() %% the actual DB name, encoded (/ -> %2f)
         ,design_name = <<>> :: binary()
         ,node = undefined :: atom()
         ,shards = [] :: list(binary()) | []
         ,disk_size = 0 :: non_neg_integer()
         ,data_size = 0 :: non_neg_integer()
         ,conn = #server{} :: server()
         ,admin_conn = #server{} :: server()
         ,do_compaction = false :: boolean()
         }).
-record(db_data, {
          db_name = <<>> :: binary() %% the shard name
         ,node = undefined :: atom()
         ,disk_size = 0 :: non_neg_integer()
         ,data_size = 0 :: non_neg_integer()
         ,conn = #server{} :: server()
         ,admin_conn = #server{} :: server()
         ,do_compaction = false :: boolean()
         }).

-record(wh_couch_connection, {id = wh_util:current_tstamp()
                              ,host = "localhost"
                              ,port = ?DEFAULT_PORT
                              ,username = ""
                              ,password = ""
                              ,ready = false
                              ,admin = false
                              ,connection = undefined
                              ,change_handlers = dict:new()
                             }).

-define(WH_COUCH_HRL, true).
-endif.
