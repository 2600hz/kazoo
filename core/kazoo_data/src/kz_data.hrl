-ifndef(KZ_DATA_HRL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo/include/kz_system_config.hrl").

-define(CACHE_NAME, 'kazoo_data_cache').
-define(KAZOO_DATA_PLAN_CACHE, 'kazoo_data_plan_cache').

-define(APP, 'kazoo_data').
-define(APP_NAME, <<"datamgr">>).
-define(APP_VERSION, <<"4.0.0">>).
-define(CONFIG_CAT, ?APP_NAME).

-record(data_connection, {id = {kz_time:now_s(), kz_binary:rand_hex(4)}
                         ,app :: atom() | '$1'
                         ,props = #{} :: map() | '_'
                         ,server :: any() | '$2'
                         ,connected = 'false' :: boolean() | '_'
                         ,ready = 'false' :: boolean()
                         ,tag :: atom()
                         }).

-record(db, {app :: atom()
            ,server :: any()
            ,db :: any()
            ,name :: kz_term:ne_binary()
            }).

-type data_connection() :: #data_connection{}.
-type data_connections() :: [data_connection()].
-type server() :: {atom(), any()} | 'undefined'.
-type db() :: #db{}.

-record(copy_doc, {source_dbname  :: kz_term:ne_binary()
                  ,source_doc_id  :: kz_term:ne_binary()
                  ,dest_dbname :: kz_term:api_binary()
                  ,dest_doc_id :: kz_term:api_binary()
                  }).
-type copy_doc() :: #copy_doc{}.

-type error_verbosity() :: 'default' | 'verbose'.
-type data_errors() :: 'conflict' |
                       'db_not_found' |
                       'db_not_reachable' |
                       'failed' |
                       'gateway_timeout' |
                       'invalid_db_name' |
                       'invalid_view_name' |
                       'not_found' |
                       'no_results' |
                       'precondition_failed' |
                       'req_timedout' |
                       'retry_later' |
                       'sel_conn_closed' |
                       'unknown_req_id' |
                       'worker_is_dead' |
                       'resource_not_available' |
                       kz_term:ne_binary() |
                       integer() |
                       {'EXIT', _} |
                       {'conn_failed', _} |
                       {'ok', string(), _, _} |
                       {'url_parsing_failed', _} |
                       {integer(), _}. % {error_code, body}

-type data_error() :: {'error', data_errors()}.

-type stale() :: 'ok' | 'update_after'.

-type key_range() :: integer() | binary() | [integer() | binary() | kz_json:object()].

-type transform_fun() :: fun((kz_json:object(), kz_json:object()) -> kz_json:object()).

-type view_option() :: 'conflicts' |
                       'descending' |
                       'first_when_multiple' |
                       'group' |
                       'include_docs' |
                       'inclusive_end' |
                       'reduce' |
                       'override_existing_document' |
                       {max_bulk_read, pos_integer()} |
                       {'transform',transform_fun()} |
                       {'end_docid', binary()} |
                       {'endkey', key_range()} |
                       {'group_level', 'exact' | integer()} |
                       {'key', binary()} |
                       {'keys', [binary()] | [[binary()]]} |
                       {'limit', integer()} |
                       {'list', binary()} |
                       {'reduce', boolean()} |
                       {'skip', integer()} |
                       {'stale', stale()} |
                       {'start_docid', binary()} |
                       {'startkey', key_range()}.

-type view_options() :: [view_option()].

-type view_listing() :: {kz_term:ne_binary(), kz_json:object()}.
-type views_listing() :: [view_listing()].

-type db_classification() :: 'account' |
                             'acdc' |
                             'aggregate' |
                             'deprecated' |
                             'external' |
                             'modb' |
                             'numbers' |
                             'provisioner' |
                             'ratedeck' |
                             'resource_selectors' |
                             'system' |
                             'undefined'.

-type db_create_option() :: {'q', non_neg_integer()} |
                            {'n', non_neg_integer()} |
                            'ensure_other_dbs' |
                            {'ensure_other_dbs', boolean()}.
-type db_create_options() :: [db_create_option()].

-type db_delete_option() :: 'ensure_other_dbs' |
                            {'ensure_other_dbs', boolean()} .
-type db_delete_options() :: [db_delete_options()].

-type ddoc() :: kz_term:ne_binary() | 'all_docs' | 'design_docs'.

-type docid() :: kz_term:ne_binary() | {kz_term:ne_binary(), kz_term:ne_binary()}.
-type docids() :: [docid()].

-type get_results_return() :: {'ok', kz_json:json_terms()} |
                              data_error().

-define(DEFAULT_DATA_SECTION, [{'local', 'bigcouch'}]).
-define(MERGE_PROPS, [{'driver', 'kazoo_couch'}
                     ,{'tag', 'local'}
                     ]).
-define(MERGE_MAP, maps:from_list(?MERGE_PROPS)).

-define(FIXTURES_FOLDER, "fixtures").

-define(DEFAULT_PUBLISH_EXCLUDE_TYPES
       ,[<<"cdr">>
        ,<<"ledger">>
        ,<<"audit_log">>
        ,<<"login_attempt">>
        ,<<"pivot_debug">>
        ,<<"notify_smtp_log">>
        ]
       ).

-define(PUBLISH_FIELDS, [<<"_deleted">>
                        ,<<"pvt_account_id">>
                        ,<<"pvt_created">>
                        ,<<"pvt_deleted">>
                        ,<<"pvt_modified">>
                        ,<<"pvt_type">>
                        ]).

-define(DELETE_KEYS, [<<"_rev">>, <<"id">>, <<"_attachments">>, <<"pvt_attachments">>]).

-define(KZ_DATA_HRL, 'true').
-endif.
