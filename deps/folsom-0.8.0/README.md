### folsom

Folsom is an Erlang based metrics system inspired by Coda Hale's metrics (https://github.com/codahale/metrics/). The metrics API's purpose is to collect realtime metrics from your Erlang applications and publish them via Erlang APIs and output plugins. folsom is *not* a persistent store. There are 6 types of metrics: counters, gauges, histograms (and timers), histories, meter_readers and meters. Metrics can be created, read and updated via the `folsom_metrics` module.

#### Building and running

First, regarding using folsom and folsom_webmachine together. To make sure you have compatible versions of each, make sure you use code from the same version tags, ie 0.5 of folsom is known to work with 0.5 folsom_webmachine. HEAD on each repo may have broken API compatibility.

You need a (preferably recent) version of Erlang installed but that should be it.

       ./rebar get-deps compile

folsom can be run standalone or embedded in an Erlang application.

       $ erl -pa ebin deps/*/ebin

       > folsom:start(). % this creates the needed ETS tables and starts a gen_server

You can also start it as an application:

       $ erl -pa ebin deps/*/ebin
       > application:start(folsom).

       $ erl -pa ebin deps/*/ebin -s folsom

The application can be configured to create individual or lists of metrics at
startup on the command line or in an application config file:

       $ erl -pa ebin deps/*/ebin -s folsom \
          -folsom history '[hist1,hist2]' \
          -folsom gauge gauge1

       $ echo '[{folsom, [{history, [hist1, hist2]}, {gauge, gauge1}]}].' \
          > myapp.config
       $ erl -pa ebin deps/*/ebin -config myapp.config -s folsom

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

###### Histogram sample types

Each histogram draws its values from a `reservoir` of readings. You can select a `sample type` for a histogram by passing the name of the sample type as an atom when you create a new histogram.
Some sample types have further arguments. The purpose of a sample type is to control the size and charecteristics of the reservoir of readings the histogram performs analysis upon.

Folsom currently provides the following sample types:

######  `uniform`

This is a random uniform sample over the stream of readings. This is the default sample type, bounded in size to 1028 readings. When `size` readings have been taken, new readings replace older readings
in the reservoir at random. You can set the sample size at creation time:

      > folsom_metrics:new_histogram(Name, uniform, Size::integer()).

Be sure you understand _why_ before you do this.

###### `exdec`

This is a  sample that exponentially decays less significant readings over time so as to give greater significance to newer readings. Read more here -
[Forward Decay...](http://www.research.att.com/people/Cormode_Graham/library/publications/CormodeShkapenyukSrivastavaXu09.pdf).
Again you can change defaults at creation time, if you think you need to:

    > folsom_metrics:new_histogram(Name, exdec, Size::integer(), Alpha::float()).

###### `slide`

This is a sliding window in time over a stream of readings. The default window size is 60 seconds. Every reading that occurs in a sliding sixty second window is stored,
with older readings being discarded. If you have a lot of readings per
minute the `reservoir` may get pretty big and so it will take more time to calculate statistics. You can set the `window` size by providing a number of seconds.

    > folsom_metrics:new_histogram(Name, slide, Seconds::integer()).

###### `slide_uniform`

This is a sliding window in time over a stream of readings with a random uniform sample per second, to bound the size of the total number of readings. The maximum size of the reservoir will be
 `window size * sample size`. Default is a window of 60 seconds and a sample size of 1028. Again, you can change these at creation time:

    > folsom_metrics:new_histogram(Name, slide_uniform, {Secs::interger(), Size::integer()).

##### Histories

Histories are a collection of past events, such as errors or log messages.

      > folsom_metrics:new_history(Name).
      > folsom_metrics:get_history_values(Name, Count). % get more than the default number of history items back
      > folsom_metrics:notify({Name, Value}).

##### Meters

Meters are increment only counters with mean rates and exponentially weighted moving averages applied to them, similar to a unix load average.

      > folsom_metrics:new_meter(Name).
      > folsom_metrics:notify({Name, Value}).

###### `Spiral` meter

A `spiral` is a type of meter that has a one minute sliding window count. The meter tracks an increment only counter and a total for the last minute. This is a sliding count with older readings dropping off per second.

    > folsom_metrics:new_spiral(Name).
    > folsom_metrics:notify({Name, Count}).

##### Meter Reader

Meter readers are like a meter except that the values passed to it are monotonically increasing, e.g., reading from a water or gas meter, CPU jiffies, or I/O operation count.

      > folsom_metrics:new_meter_reader(Name).
      > folsom_metrics:notify({Name, Value}).

##### Metrics groups/tags

Certain users might want to group and query metrics monitoring a common task. In order to do so, they can
tag metrics:

    > folsom_metrics:tag_metric(Name, Tag).

and untag metrics:

    > folsom_metrics:untag_metric(Name, Tag).

Users can query a list of tuples `[{Name, Value}]` of all metrics with a given tag:

    > folsom_metrics:get_metrics_value(Tag).

If only a certain type of metrics from a given group is desired, one can specify so:

    > folsom_metrics:get_metrics_value(Tag, Type).

where Type is one of `counter`, `gauge`, `histogram`, `history`, `meter`, `meter_reader`, `duration` or `spiral`.

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
