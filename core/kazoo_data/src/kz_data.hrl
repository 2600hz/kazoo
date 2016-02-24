-ifndef(KZ_DATA_HRL).
-include_lib("whistle/include/wh_types.hrl"). % get the whistle types
-include_lib("whistle/include/wh_log.hrl").
-include_lib("whistle/include/wh_databases.hrl").
-include_lib("whistle/include/kz_system_config.hrl").

-define(KZ_DATA_CACHE, 'kazoo_data_cache').
-define(CONFIG_CAT, <<"datamgr">>).

-record(data_connection, {id = wh_util:current_tstamp()
                          ,app :: atom()
                          ,props :: #{}
                          ,server
                          ,connected = 'false' :: boolean()
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
-type server() :: {atom(), any()} | 'undefined'.
-type db() :: #db{}.

-record(copy_doc, {source_dbname  :: ne_binary()
                   ,source_doc_id  :: ne_binary()
                   ,dest_dbname = 'undefined' :: api_binary()
                   ,dest_doc_id = 'undefined' :: api_binary()
                  }).
-type copy_doc() :: #copy_doc{}.

-type data_errors() :: 'not_found' | 'conflict' | 'failed' |
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
-type data_error() :: {'error', data_errors()}.


-type stale() :: ok | update_after.

-type view_option() :: {key, binary()} | {start_docid, binary()}
    | {end_docid, binary()} | {start_key, binary()}
    | {end_key, binary()} | {limit, integer()}
    | {stale, stale()}
    | descending
    | {skip, integer()}
    | group | {group_level, exact | integer()}
    | reduce | {reduce, boolean()}
    | inclusive_end | include_docs | conflicts
    | {list, binary()}
    | {keys, list(binary())}.

-type view_options() :: list(view_option()).

-type db_classifications() :: 'account' | 'modb' | 'acdc' |
                              'numbers' | 'aggregate' | 'system' |
                              'deprecated' | 'undefined'.

-type db_create_options() :: [{'q',integer()} | {'n',integer()}].

-type ddoc() :: ne_binary() | 'all_docs' | 'design_docs'.

-type get_results_return() :: {'ok', wh_json:objects() | wh_json:keys()} |
                              data_error().

-define(DEFAULT_DATA_SECTION, [{local, bigcouch}]).
-define(MERGE_PROPS, [{driver, kazoo_couch}
                      ,{tag, local}
                     ]).
-define(MERGE_MAP, maps:from_list(?MERGE_PROPS)).


-define(FIXTURES_FOLDER, "fixtures").

-define(PUBLISH_FIELDS, [<<"pvt_type">>
                         ,<<"pvt_account_id">>
                         ,<<"pvt_created">>
                         ,<<"pvt_modified">>
                        ]).

-define(DELETE_KEYS, [<<"_rev">>, <<"id">>, <<"_attachments">>]).

-define(MAX_BULK_INSERT, 2000).

-define(VALID_DBNAME, is_binary(DbName) andalso byte_size(DbName) > 0).

-define(KZ_DATA_HRL, 'true').
-endif.
