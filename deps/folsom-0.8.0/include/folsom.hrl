-define(FOLSOM_TABLE, folsom).
-define(COUNTER_TABLE, folsom_counters).
-define(GAUGE_TABLE, folsom_gauges).
-define(HISTOGRAM_TABLE, folsom_histograms).
-define(METER_TABLE, folsom_meters).
-define(METER_READER_TABLE, folsom_meter_readers).
-define(HISTORY_TABLE, folsom_histories).
-define(DURATION_TABLE, folsom_durations).
-define(SPIRAL_TABLE, folsom_spirals).

-define(DEFAULT_LIMIT, 5).
-define(DEFAULT_SIZE, 1028). % mimic codahale's metrics
-define(DEFAULT_SLIDING_WINDOW, 60). % sixty second sliding window
-define(DEFAULT_ALPHA, 0.015). % mimic codahale's metrics
-define(DEFAULT_INTERVAL, 5000).
-define(DEFAULT_SAMPLE_TYPE, uniform).

-record(spiral, {
          tid = folsom_metrics_histogram_ets:new(folsom_spiral,
                                                 [set,
                                                  {write_concurrency, true},
                                                  public]),
          server
         }).

-record(slide, {
          window = ?DEFAULT_SLIDING_WINDOW,
          reservoir = folsom_metrics_histogram_ets:new(folsom_slide,
                                                       [duplicate_bag, {write_concurrency, true}, public]),
          server
         }).

-record(slide_uniform, {
          window = ?DEFAULT_SLIDING_WINDOW,
          size = ?DEFAULT_SIZE,
          reservoir = folsom_metrics_histogram_ets:new(folsom_slide_uniform,[set, {write_concurrency, true}, public]),
          seed = os:timestamp(),
          server
         }).

-record(uniform, {
          size = ?DEFAULT_SIZE,
          n = 1,
          reservoir = folsom_metrics_histogram_ets:new(folsom_uniform,[set, {write_concurrency, true}, public]),
          seed = os:timestamp()
         }).

-record(exdec, {
          start = 0,
          next = 0,
          alpha = ?DEFAULT_ALPHA,
          size = ?DEFAULT_SIZE,
          seed = os:timestamp(),
          n = 1,
          reservoir = folsom_metrics_histogram_ets:new(folsom_exdec,[ordered_set, {write_concurrency, true}, public])
         }).

-record(none, {
          size = ?DEFAULT_SIZE,
          n = 1,
          reservoir = folsom_metrics_histogram_ets:new(folsom_none,[ordered_set, {write_concurrency, true}, public])
         }).

-record(slide_sorted, {
          size = ?DEFAULT_SIZE,
          n = 0,
          reservoir = folsom_metrics_histogram_ets:new(folsom_slide_sorted,[ordered_set, {write_concurrency, true}, public])
         }).

-record(histogram, {
          type = uniform,
          sample = #uniform{}
         }).

-record(history, {
          tid
          }).

-define(SYSTEM_INFO, [
                      allocated_areas,
                      allocator,
                      alloc_util_allocators,
                      build_type,
                      c_compiler_used,
                      check_io,
                      compat_rel,
                      cpu_topology,
                      creation,
                      debug_compiled,
                      dist,
                      dist_ctrl,
                      driver_version,
                      elib_malloc,
                      dist_buf_busy_limit,
                      %fullsweep_after, % included in garbage_collection
                      garbage_collection,
                      %global_heaps_size, % deprecated
                      heap_sizes,
                      heap_type,
                      info,
                      kernel_poll,
                      loaded,
                      logical_processors,
                      logical_processors_available,
                      logical_processors_online,
                      machine,
                      %min_heap_size, % included in garbage_collection
                      %min_bin_vheap_size, % included in garbage_collection
                      modified_timing_level,
                      multi_scheduling,
                      multi_scheduling_blockers,
                      otp_release,
                      process_count,
                      process_limit,
                      scheduler_bind_type,
                      scheduler_bindings,
                      scheduler_id,
                      schedulers,
                      schedulers_online,
                      smp_support,
                      system_version,
                      system_architecture,
                      threads,
                      thread_pool_size,
                      trace_control_word,
                      update_cpu_info,
                      version,
                      wordsize
                  ]).

-define(STATISTICS, [
                     context_switches,
                     %exact_reductions, % use reductions instead
                     garbage_collection,
                     io,
                     reductions,
                     run_queue,
                     runtime,
                     wall_clock
                    ]).

-define(PROCESS_INFO, [
                       backtrace,
                       binary,
                       catchlevel,
                       current_function,
                       %dictionary,
                       error_handler,
                       garbage_collection,
                       group_leader,
                       heap_size,
                       initial_call,
                       links,
                       last_calls,
                       memory,
                       %message_binary,
                       message_queue_len,
                       messages,
                       min_heap_size,
                       min_bin_vheap_size,
                       monitored_by,
                       monitors,
                       priority,
                       reductions,
                       registered_name,
                       sequential_trace_token,
                       stack_size,
                       status,
                       suspending,
                       total_heap_size,
                       trace,
                       trap_exit
                      ]).

-define(SOCKET_OPTS, [
                      active,
                      broadcast,
                      delay_send,
                      dontroute,
                      exit_on_close,
                      header,
                      keepalive,
                      nodelay,
                      packet,
                      packet_size,
                      read_packets,
                      recbuf,
                      reuseaddr,
                      send_timeout,
                      send_timeout_close,
                      sndbuf,
                      priority,
                      tos
                     ]).
