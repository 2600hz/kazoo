- [Kazoo System Tracing](#orgheadline7)
  - [Start a trace](#orgheadline5)
    - [Filters](#orgheadline1)
    - [Filename](#orgheadline2)
    - [Format](#orgheadline3)
    - [Defaults](#orgheadline4)
  - [Stopping a trace](#orgheadline6)


# Kazoo System Tracing<a id="orgheadline7"></a>

It is possible to create Erlang traces and write them to file(s) so you can get an idea about what is happening in the VM. Lager provides this [tracing](https://github.com/basho/lager#tracing) functionality and kz\_data\_tracing exposes the wrapper.

## Start a trace<a id="orgheadline5"></a>

Creating a system trace needs to be surgical; too many events could overwhelm the VM and impact performance. Especially if tracing a production system, use filters to limit the scope of events traced.

Starting a trace is straight-forward:

    1> {'ok', Trace1} = kz_data_tracing:trace_file().
    {'ok', Ref1}
    2> {'ok', Trace2} = kz_data_tracing:trace_file(Filters).
    {'ok', Ref2}
    3> {'ok', Trace3} = kz_data_tracing:trace_file(Filters, Filename).
    {'ok', Ref3}
    4> {'ok', Trace4} = kz_data_tracing:trace_file(Filters, Filename, Format).
    {'ok', Ref4}

SUP-provided examples:

    sup kazoo_data_maintenance trace_module Module
    sup kazoo_data_maintenance trace_function Function
    sup kazoo_data_maintenance trace_function Module Function
    sup kazoo_data_maintenance trace_pid 'X.Y.Z'

### Filters<a id="orgheadline1"></a>

Filters are 2-tuples that contain the patterns used on events to see if they meet the necessary criteria.

-   \`{'module', Module}\`: Filter for calls to \`Module\`
-   \`{'function', Function}\`: Filter for calls to \`Function\` When combined with \`{'module', Module}\`, just trace that Module:Function combo
-   \`{'pid', Pid}\`: Filter for calls by \`Pid\`
-   Custom metadata
    -   \`lager:warning([{request, RequestId}], "the passwords didn't match for ~s", [UserId])\` would allow a filter of \`{'request', RequestId}\`
    -   You can also associate metadata with a \`pid()\`: \`lager:md([{'zone', 'west'}])\`

You can also use \`'\*'\` in the second element to match all of that class of filter.

For example, \`[{'module', 'kz\_json'}, {'function', '\*'}]\` would filter all calls to the \`kz\_json\` module.

### Filename<a id="orgheadline2"></a>

Specify the filename for where to write the trace results.

### Format<a id="orgheadline3"></a>

You can also supply a [custom formatter](https://github.com/basho/lager#custom-formatting) to be used when writing the log line (uses the Kazoo standard format otherwise).

### Defaults<a id="orgheadline4"></a>

-   Filters: \`[{'function', '\*'}]\`
-   Filename: \`/tmp/{32\_char\_hex}.log\`
-   Format: -define(DEFAULT\_TRACE\_OUTPUT\_FORMAT, ['time'," [",'severity',"] |", 'from\_app', "|", {'callid', <a id="orgtarget1"></a>}, "|", 'mod', ":" , 'func', ":", 'line', " (",'pid', ") ", 'message', "\n"]).

## Stopping a trace<a id="orgheadline6"></a>

Assuming you bound the second element in the resulting tuple:

    1> {'ok', Ref} = kz_data_tracing:trace_file().
    {'ok', Ref}
    2> ...Do some things...or not
    'maybe'
    3> kz_data_tracing:stop_trace(Ref).
    'ok'
