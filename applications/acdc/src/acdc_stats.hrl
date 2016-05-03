-ifndef(ACDC_STATS_HRL).

-define(ARCHIVE_MSG, 'time_to_archive').
-define(CLEANUP_MSG, 'time_to_cleanup').

-define(VALID_STATUSES, [<<"waiting">>, <<"handled">>, <<"abandoned">>, <<"processed">>]).

-define(MAX_RESULT_SET, kapps_config:get_integer(?CONFIG_CAT, <<"max_result_set">>, 25)).

-record(agent_miss, {
          agent_id :: api(binary())
          ,miss_reason :: api(binary())
          ,miss_timestamp = kz_util:current_tstamp() :: pos_integer()
         }).
-type agent_miss() :: #agent_miss{}.
-type agent_misses() :: [agent_miss()].

-record(call_stat, {
          id :: api(binary()) | '_' %% call_id::queue_id
          ,call_id :: api(binary()) | '_'
          ,account_id :: api(binary()) | '$1' | '_'
          ,queue_id :: api(binary()) | '$2' | '_'

          ,agent_id :: api(binary()) | '$3' | '_' % the handling agent

          ,entered_timestamp = kz_util:current_tstamp() :: pos_integer() | '$1' | '$5' | '_'
          ,abandoned_timestamp :: api(integer()) | '_'
          ,handled_timestamp :: api(integer()) | '_'
          ,processed_timestamp :: api(integer()) | '_'

          ,hung_up_by :: api(binary()) | '_'

          ,abandoned_reason :: api(binary()) | '_'

          ,misses = [] :: agent_misses() | '_'

          ,status :: api(binary()) | '$1' | '$2' | '$4' | '_'
          ,caller_id_name :: api(binary()) | '_'
          ,caller_id_number :: api(binary()) | '_'
          ,caller_priority :: api(integer()) | '_'
          ,is_archived = 'false' :: boolean() | '$2' | '$3' | '_'
         }).
-type call_stat() :: #call_stat{}.


-define(STATUS_STATUSES, [<<"logged_in">>, <<"logged_out">>, <<"ready">>
                          ,<<"connecting">>, <<"connected">>
                          ,<<"wrapup">>, <<"paused">>, <<"outbound">>
                         ]).
-record(status_stat, {
          id :: api(binary()) | '_'
          ,agent_id :: api(binary()) | '$2' | '_'
          ,account_id :: api(binary()) | '$1' | '_'
          ,status :: api(binary()) | '$4' | '_'
          ,timestamp :: api_pos_integer() | '$1' | '$3' | '$5' | '_'

          ,wait_time :: api(integer()) | '_'
          ,pause_time :: api(integer()) | '_'
          ,callid :: api(binary()) | '_'
          ,caller_id_name :: api(binary()) | '_'
          ,caller_id_number :: api(binary()) | '_'
          ,is_archived = 'false' :: boolean() | '$1' | '$2' | '_'
         }).
-type status_stat() :: #status_stat{}.


-define(ACDC_STATS_HRL, 'true').
-endif.
