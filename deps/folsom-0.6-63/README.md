### folsom

Folsom is an Erlang based metrics system inspired by Coda Hale's metrics (https://github.com/codahale/metrics/). The metrics API's purpose is to collect realtime metrics from your Erlang applications and publish them via Erlang APIs and output plugins. folsom is *not* a persistent store. There are 6 types of metrics: counters, gauges, histograms (and timers), histories, meter_readers and meters. Metrics can be created, read and updated via the `folsom_metrics` module.

#### Building and running

First, regarding using folsom and folsom_webmachine together. To make sure you have compatible versions of each, make sure you use code from the same version tags, ie 0.5 of folsom is known to work with 0.5 folsom_webmachine. HEAD on each repo may have broken API compatibility.

You need a (preferably recent) version of Erlang installed but that should be it.

       ./rebar compile

folsom can be run standalone or embedded in an Erlang application.

       $ erl -pa ebin

       > folsom_sup:start_link(). % this creates the needed ETS tables and starts a gen_server

#### Metrics API

folsom_metrics.erl is the API module you will need to use most of the time.

Retrieve a list of current installed metrics:

      > folsom_metrics:get_metrics().

Query a specific metric:

      > folsom_metrics:get_metric_value(Name).

Generally names of metrics are atoms or binaries.

##### Counters

Counter metrics provide increment and decrement capabilities for a single scalar value.

      > folsom_metrics:new_counter(Name).
      > folsom_metrics:notify({Name, {inc, Value}}).
      > folsom_metrics:notify({Name, {dec, Value}}).

##### Gauges

Gauges are point-in-time single value metrics.

      > folsom_metrics:new_gauge(Name).
      > folsom_metrics:notify({Name, Value}).

##### Histograms (and Timers)

Histograms are collections of values that have statistical analysis done to them, such as mean, min, max, kurtosis and percentile. They can be used like "timers" as well with the timed update functions.

      > folsom_metrics:new_histogram(Name).
      > folsom_metrics:histogram_timed_update(Name, Mod, Fun, Args).
      > folsom_metrics:histogram_timed_update(Name, Fun, Args).
      > folsom_metrics:histogram_timed_update(Name, Fun).
      > folsom_metrics:notify({Name, Value}).

##### Histories

Histories are a collection of past events, such as errors or log messages.

      > folsom_metrics:new_history(Name).
      > folsom_metrics:get_history_values(Name, Count). % get more than the default number of history items back
      > folsom_metrics:notify({Name, Value}).

##### Meters

Meters are increment only counters with mean rates and exponentially weighted moving averages applied to them, similar to a unix load average.

      > folsom_metrics:new_meter(Name).
      > folsom_metrics:notify({Name, Value}).

##### Meter Reader

Meter readers are like a meter except that the values passed to it are monotonically increasing, e.g., reading from a water or gas meter, CPU jiffies, or I/O operation count.

      > folsom_metrics:new_meter_reader(Name).
      > folsom_metrics:notify({Name, Value}).

##### Erlang VM

folsom also produces Erlang VM statistics.

The result of `erlang:memory/0`:

       > folsom_vm_metrics:get_memory().

The result of `erlang:system_info/1`:

       > folsom_vm_metrics:get_system_info().

The result of `erlang:statistics/1`:

       > folsom_vm_metrics:get_statistics().

The result of `erlang:process_info/1`:

       > folsom_vm_metrics:get_process_info(). %% use with caution

The result of `inet:getstat/1`, `prim_inet:getstatus/1`, `erlang:port_info/1`, `prim_inet:gettype/1`, `inet:getopts/1`, `inet:sockname/1`:

       > folsom_vm_metrics:get_port_info(). %% use with caution

The result from `ets:info/1` and `dets:info/1` across all tables

       > folsom_vm_metrics:get_ets_info().
       > folsom_vm_metrics:get_dets_info().
