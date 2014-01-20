-ifndef(ACDC_STATS_HRL).

-define(ARCHIVE_MSG, 'time_to_archive').
-define(CLEANUP_MSG, 'time_to_cleanup').

-define(VALID_STATUSES, [<<"waiting">>, <<"handled">>, <<"abandoned">>, <<"processed">>]).

-define(MAX_RESULT_SET, whapps_config:get_integer(?CONFIG_CAT, <<"max_result_set">>, 25)).

-record(agent_miss, {
          agent_id :: api_binary()
          ,miss_reason :: api_binary()
          ,miss_timestamp = wh_util:current_tstamp() :: pos_integer()
         }).
-type agent_miss() :: #agent_miss{}.
-type agent_misses() :: [agent_miss(),...] | [].

-record(call_stat, {
          id :: api_binary() | '_' %% call_id::queue_id
          ,call_id :: api_binary() | '_'
          ,acct_id :: api_binary() | '$1' | '_'
          ,queue_id :: api_binary() | '$2' | '_'

          ,agent_id :: api_binary() | '$3' | '_' % the handling agent

          ,entered_timestamp = wh_util:current_tstamp() :: pos_integer() | '$1' | '$5' | '_'
          ,abandoned_timestamp :: api_integer() | '_'
          ,handled_timestamp :: api_integer() | '_'
          ,processed_timestamp :: api_integer() | '_'

          ,abandoned_reason :: api_binary() | '_'

          ,misses = [] :: agent_misses() | '_'

          ,status :: api_binary() | '$2' | '$4' | '_'
          ,caller_id_name :: api_binary() | '_'
          ,caller_id_number :: api_binary() | '_'
          ,is_archived = 'false' :: boolean() | '$3' | '_'
         }).
-type call_stat() :: #call_stat{}.


-define(STATUS_STATUSES, [<<"logged_in">>, <<"logged_out">>, <<"ready">>
                          ,<<"connecting">>, <<"connected">>
                          ,<<"wrapup">>, <<"paused">>, <<"outbound">>
                         ]).
-record(status_stat, {
          id :: api_binary() | '_'
          ,agent_id :: api_binary() | '$2' | '_'
          ,acct_id :: api_binary() | '$1' | '_'
          ,status :: api_binary() | '$4' | '_'
          ,timestamp :: api_pos_integer() | '$1' | '$3' | '$5' | '_'

          ,wait_time :: api_integer() | '_'
          ,pause_time :: api_integer() | '_'
          ,callid :: api_binary() | '_'
          ,caller_id_name :: api_binary() | '_'
          ,caller_id_number :: api_binary() | '_'
          ,is_archived = 'false' :: boolean() | '$1' | '$2' | '_'
         }).
-type status_stat() :: #status_stat{}.


-define(ACDC_STATS_HRL, 'true').
-endif.
