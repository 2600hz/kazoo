-ifndef(ACDC_STATS_HRL).

-define(ARCHIVE_MSG, 'time_to_archive').
-define(CLEANUP_MSG, 'time_to_cleanup').

-define(VALID_STATUSES, [<<"waiting">>, <<"handled">>, <<"abandoned">>, <<"processed">>]).

-define(STATS_QUERY_LIMITS_ENABLED, kapps_config:get_is_true(?CONFIG_CAT, <<"stats_query_limits_enabled">>, 'true')).
-define(MAX_RESULT_SET, kapps_config:get_integer(?CONFIG_CAT, <<"max_result_set">>, 25)).

-record(agent_miss, {agent_id :: kz_term:api_binary()
                    ,miss_reason :: kz_term:api_binary()
                    ,miss_timestamp = kz_time:now_s() :: pos_integer()
                    }).
-type agent_miss() :: #agent_miss{}.
-type agent_misses() :: [agent_miss()].

-record(call_stat, {id :: kz_term:api_binary() | '_' %% call_id::queue_id
                   ,call_id :: kz_term:api_binary() | '_'
                   ,account_id :: kz_term:api_binary() | '$1' | '_'
                   ,queue_id :: kz_term:api_binary() | '$2' | '_'

                   ,agent_id :: kz_term:api_binary() | '$3' | '_' % the handling agent

                   ,entered_timestamp = kz_time:now_s() :: pos_integer() | '$1' | '$5' | '_'
                   ,abandoned_timestamp :: kz_term:api_integer() | '$2' | '_'
                   ,handled_timestamp :: kz_term:api_integer() | '$3' | '_'
                   ,processed_timestamp :: kz_term:api_integer() | '_'

                   ,hung_up_by :: kz_term:api_binary() | '_'

                   ,abandoned_reason :: kz_term:api_binary() | '_'

                   ,misses = [] :: agent_misses() | '_'

                   ,status :: kz_term:api_binary() | '$1' | '$2' | '$4' | '_'
                   ,caller_id_name :: kz_term:api_binary() | '_'
                   ,caller_id_number :: kz_term:api_binary() | '_'
                   ,caller_priority :: kz_term:api_integer() | '_'
                   ,is_archived = 'false' :: boolean() | '$2' | '$3' | '_'
                   }).
-type call_stat() :: #call_stat{}.
-type call_stats() :: [call_stat()].


-define(STATUS_STATUSES, [<<"logged_in">>, <<"logged_out">>, <<"ready">>
                         ,<<"connecting">>, <<"connected">>
                         ,<<"wrapup">>, <<"paused">>, <<"outbound">>
                         ]).

%% This key optimizes lookups in the ordered_set ETS table
-record(status_stat_key, {account_id = '_' :: kz_term:ne_binary() | '$1' | '_'
                         ,agent_id = '_' :: kz_term:ne_binary() | '$2' | '_'
                         ,timestamp = '_' :: pos_integer() | '$1' | '$3' | '_'
                         }).
-type status_stat_key() :: #status_stat_key{}.
-record(status_stat, {key = '_' :: status_stat_key() | '_'
                     ,id :: kz_term:api_binary() | '_'
                     ,status :: kz_term:api_binary() | '$4' | '_'

                     ,wait_time :: kz_term:api_integer() | '_'
                     ,pause_time :: kz_term:api_integer() | '_'
                     ,callid :: kz_term:api_binary() | '_'
                     ,caller_id_name :: kz_term:api_binary() | '_'
                     ,caller_id_number :: kz_term:api_binary() | '_'
                     ,queue_id :: kz_term:api_binary() | '_'
                     ,is_archived = 'false' :: boolean() | '$1' | '$2' | '_'
                     }).
-type status_stat() :: #status_stat{}.


-define(ACDC_STATS_HRL, 'true').
-endif.
