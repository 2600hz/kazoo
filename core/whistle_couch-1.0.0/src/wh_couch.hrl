-ifndef(WH_COUCH_HRL).
-include_lib("whistle/include/wh_types.hrl"). % get the whistle types
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/kz_system_config.hrl").
-include_lib("couchbeam/include/couchbeam.hrl").

-define(TIMEOUT, ?MILLISECONDS_IN_HOUR). %% check every hour

-define(COMPACT_THRESHOLD, 500).
-define(MIN_DISK_SIZE, 131072).
-define(DEFAULT_PORT, 5984).
-define(DEFAULT_ADMIN_PORT, 5986).
-define(IBROWSE_OPTS, [{'max_sessions', 512}
                       ,{'max_pipeline_size', 10}
                       ,{'connect_timeout', 500}
                      ]).

-define(WH_COUCH_CACHE, 'whistle_couch_cache').

-define(FIXTURES_FOLDER, "fixtures").

-define(CONFIG_CAT, ?SYSCONFIG_COUCH).

-type couchbeam_errors() :: 'not_found' | 'conflict' | 'failed' |
                            'precondition_failed' | 'db_not_reachable' |
                            'db_not_found' | 'worker_is_dead' | 'invalid_db_name' |
                            'unknown_req_id' | 'retry_later' |
                            'req_timedout' | 'sel_conn_closed' |
                            'invalid_view_name' | 'gateway_timeout' |
                            {integer(), _} | % {error_code, body}
                            integer() |
                            {'EXIT', _} |
                            {'url_parsing_failed', _} |
                            {'conn_failed', _} |
                            {'ok', string(), _, _}.
-type couchbeam_error() :: {'error', couchbeam_errors()}.

-record(design_data, {
          db_name = <<>> :: binary() %% the actual DB name, encoded (/ -> %2f)
         ,design_name = <<>> :: binary()
         ,node :: atom()
         ,shards = [] :: ne_binaries()
         ,disk_size = 0 :: non_neg_integer()
         ,data_size = 0 :: non_neg_integer()
         ,conn = #server{} :: server()
         ,admin_conn = #server{} :: server()
         ,do_compaction = 'false' :: boolean()
         }).
-record(db_data, {
          db_name = <<>> :: binary() %% the shard name
         ,node :: atom()
         ,disk_size = 0 :: non_neg_integer()
         ,data_size = 0 :: non_neg_integer()
         ,conn = #server{} :: server()
         ,admin_conn = #server{} :: server()
         ,do_compaction = 'false' :: boolean()
         }).

-record(wh_couch_connection, {id = wh_util:current_tstamp()
                              ,host = "localhost"
                              ,port = ?DEFAULT_PORT
                              ,username = ""
                              ,password = ""
                              ,connected = 'false'
                              ,ready = 'false'
                              ,admin = 'false'
                              ,server = #server{}
                              ,tag = 'local'
                             }).
-type couch_connection() :: #wh_couch_connection{}.
-type couch_connections() :: [couch_connection()].

-type couchbeam_db() :: #db{}.

-record(wh_copy_doc, {source_dbname  :: ne_binary()
                      ,source_doc_id  :: ne_binary()
                      ,dest_dbname = 'undefined' :: api_binary()
                      ,dest_doc_id = 'undefined' :: api_binary()
                     }).
-type copy_doc() :: #wh_copy_doc{}.

-define(WH_COUCH_HRL, 'true').
-endif.
