-ifndef(KZ_DATA_HRL).
-include_lib("kazoo/include/kz_types.hrl"). % get the kazoo types
-include_lib("kazoo/include/kz_log.hrl").
-include_lib("kazoo/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").

-define(CACHE_NAME, 'kazoo_data_cache').
-define(KAZOO_DATA_PLAN_CACHE, 'kazoo_data_plan_cache').

-define(CONFIG_CAT, <<"datamgr">>).
-define(APP_NAME, <<"datamgr">>).
-define(APP_VERSION, <<"4.0.0">>).

-record(data_connection, {id = kz_util:current_tstamp()
                          ,app :: atom() | '$1'
                          ,props = #{} :: #{} | '_'
                          ,server :: any() | '$2'
                          ,connected = 'false' :: boolean() | '_'
                          ,ready = 'false' :: boolean()
                          ,tag :: atom()
                         }).

-record(db, {app :: atom()
            ,server :: any()
            ,db :: any()
            ,name :: ne_binary()
            }).

-type data_connection() :: #data_connection{}.
-type data_connections() :: [data_connection()].
-type server() :: api({atom(), any()}).
-type db() :: #db{}.

-record(copy_doc, {source_dbname  :: ne_binary()
                   ,source_doc_id  :: ne_binary()
                   ,dest_dbname = 'undefined' :: api(binary())
                   ,dest_doc_id = 'undefined' :: api(binary())
                  }).
-type copy_doc() :: #copy_doc{}.

-type data_errors() :: 'conflict' |
                       'db_not_found' |
                       'db_not_reachable' |
                       'failed' |
                       'gateway_timeout' |
                       'invalid_db_name' |
                       'invalid_view_name' |
                       'not_found' |
                       'precondition_failed' |
                       'req_timedout' |
                       'retry_later' |
                       'sel_conn_closed' |
                       'unknown_req_id' |
                       'worker_is_dead' |
                       'resource_not_available' |
                       integer() |
                       {'EXIT', _} |
                       {'conn_failed', _} |
                       {'ok', string(), _, _} |
                       {'url_parsing_failed', _} |
                       {integer(), _}. % {error_code, body}

-type data_error() :: {'error', data_errors()}.

-type stale() :: 'ok' | 'update_after'.

-type key_range() :: binary() | [binary() | kz_json:object()].

-type view_option() :: 'conflicts' |
                       'descending' |
                       'group' |
                       'include_docs' |
                       'inclusive_end' |
                       'reduce' |
                       {'end_docid', binary()} |
                       {'endkey', key_range()} |
                       {'group_level', 'exact' | integer()} |
                       {'key', binary()} |
                       {'keys', [binary()]} |
                       {'limit', integer()} |
                       {'list', binary()} |
                       {'reduce', boolean()} |
                       {'skip', integer()} |
                       {'stale', stale()} |
                       {'start_docid', binary()} |
                       {'startkey', key_range()}.

-type view_options() :: list(view_option()).

-type db_classifications() :: api('account' |
                                  'modb' |
                                  'acdc' |
                                  'numbers' |
                                  'aggregate' |
                                  'system' |
                                  'deprecated'
                                 ).

-type db_create_options() :: [{'q',integer()} | {'n',integer()}].

-type ddoc() :: ne_binary() | 'all_docs' | 'design_docs'.

-type docid() :: ne_binary() | {ne_binary(), ne_binary()}.
-type docids() :: ne_binary() | {ne_binary(), ne_binaries()}.

-type get_results_return() :: {'ok', kz_json:objects() | kz_json:keys()} |
                              data_error().

-define(DEFAULT_DATA_SECTION, [{local, bigcouch}]).
-define(MERGE_PROPS, [{driver, kazoo_couch}
                      ,{tag, local}
                     ]).
-define(MERGE_MAP, maps:from_list(?MERGE_PROPS)).


-define(FIXTURES_FOLDER, "fixtures").

-define(PUBLISH_FIELDS, [<<"_deleted">>
                         ,<<"pvt_account_id">>
                         ,<<"pvt_created">>
                         ,<<"pvt_deleted">>
                         ,<<"pvt_modified">>
                         ,<<"pvt_type">>
                        ]).

-define(DELETE_KEYS, [<<"_rev">>, <<"id">>, <<"_attachments">>]).

-define(MAX_BULK_INSERT, 2000).

-define(VALID_DBNAME, is_binary(DbName) andalso byte_size(DbName) > 0).

-define(KZ_DATA_HRL, 'true').
-endif.
