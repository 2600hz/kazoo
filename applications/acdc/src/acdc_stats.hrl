-ifndef(ACDC_STATS_HRL).

-define(ARCHIVE_MSG, 'time_to_archive').
-define(CLEANUP_MSG, 'time_to_cleanup').

-define(VALID_STATUSES, [<<"waiting">>, <<"handled">>, <<"abandoned">>, <<"processed">>]).

-define(MAX_RESULT_SET, kapps_config:get_integer(?CONFIG_CAT, <<"max_result_set">>, 25)).

-record(agent_miss, {
          agent_id :: maybe(binary())
          ,miss_reason :: maybe(binary())
          ,miss_timestamp = kz_util:current_tstamp() :: pos_integer()
         }).
-type agent_miss() :: #agent_miss{}.
-type agent_misses() :: [agent_miss()].

-record(call_stat, {
          id :: maybe(binary()) | '_' %% call_id::queue_id
          ,call_id :: maybe(binary()) | '_'
          ,account_id :: maybe(binary()) | '$1' | '_'
          ,queue_id :: maybe(binary()) | '$2' | '_'

          ,agent_id :: maybe(binary()) | '$3' | '_' % the handling agent

          ,entered_timestamp = kz_util:current_tstamp() :: pos_integer() | '$1' | '$5' | '_'
          ,abandoned_timestamp :: maybe(integer()) | '_'
          ,handled_timestamp :: maybe(integer()) | '_'
          ,processed_timestamp :: maybe(integer()) | '_'

          ,hung_up_by :: maybe(binary()) | '_'

          ,abandoned_reason :: maybe(binary()) | '_'

          ,misses = [] :: agent_misses() | '_'

          ,status :: maybe(binary()) | '$1' | '$2' | '$4' | '_'
          ,caller_id_name :: maybe(binary()) | '_'
          ,caller_id_number :: maybe(binary()) | '_'
          ,caller_priority :: maybe(integer()) | '_'
          ,is_archived = 'false' :: boolean() | '$2' | '$3' | '_'
         }).
-type call_stat() :: #call_stat{}.


-define(STATUS_STATUSES, [<<"logged_in">>, <<"logged_out">>, <<"ready">>
                          ,<<"connecting">>, <<"connected">>
                          ,<<"wrapup">>, <<"paused">>, <<"outbound">>
                         ]).
-record(status_stat, {
          id :: maybe(binary()) | '_'
          ,agent_id :: maybe(binary()) | '$2' | '_'
          ,account_id :: maybe(binary()) | '$1' | '_'
          ,status :: maybe(binary()) | '$4' | '_'
          ,timestamp :: maybe(pos_integer()) | '$1' | '$3' | '$5' | '_'

          ,wait_time :: maybe(integer()) | '_'
          ,pause_time :: maybe(integer()) | '_'
          ,callid :: maybe(binary()) | '_'
          ,caller_id_name :: maybe(binary()) | '_'
          ,caller_id_number :: maybe(binary()) | '_'
          ,is_archived = 'false' :: boolean() | '$1' | '$2' | '_'
         }).
-type status_stat() :: #status_stat{}.


-define(ACDC_STATS_HRL, 'true').
-endif.
