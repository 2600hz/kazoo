### Flame graphs

It can be nice to visualize where time is spent when executing code.

We have added Scott Fritchie's fork of [eflame](https://github.com/slfritchie/eflame) to help trace and generate flame graphs.

**Important**: as far as I can tell, you can only have one trace going on at any given time. Be aware!

#### kz_tracers

The `kz_tracers` module (in `core/whistle_apps`) starts the trace. Currently it just traces the supplied `pid()` with an optional time to trace (default 100s but this is way too long for most requests).

#### Tracing Crossbar requests

You can enable a trace for a crossbar request by adding a header `x-trace-request: true`.

You will also need to enable tracing as a system config: `sup kapps_config set_default crossbar allow_tracing true` This has performance implications so I wouldn't leave this enabled on a production environment.

Now try your cURL request:

```shell
curl -v -X PUT \
    -H "content-type:application/json" \
    -H "x-trace-request:true" \
    -d '{"data":{"credentials":"{HASH}","account_name":"master"}}' \
    http://localhost:8000/v2/user_auth
```

In your Erlang VM (if you're running in interactive mode), you should see some logs:

```
started trace for <0.10699.2> in <0.10700.2>
Tracer <0.10667.2>
self() <0.10700.2>
Starting tracer results: {v_global_and_local_calls,
                             {ok,[{matched,
                                      'whistle_apps@localhost.localdomain',
                                      24456},
                                  {saved,1}]}}
trace for <0.10699.2> done
trace formatted to /tmp/eflame.trace.out
<0.10699.2> Hello, world, I'm <0.10713.2> and I'm running....
<0.10706.2>

Writing to /tmp/eflame.trace.out for 3 processes... finished!
```

Once its finished, you need to run a script to create the svg file:

```shell
kazoo/deps/eflame$ cat /tmp/eflame.trace.out | ./flamegraph.riak-color.pl > /tmp/eflame.svg
```

Now load that into your browser and click around!

You can also remove the SLEEP time:

```shell
kazoo/deps/eflame$ cat /tmp/eflame.trace.out | grep -v 'SLEEP ' | ./flamegraph.riak-color.pl > /tmp/eflame.svg
```

#### Issues

Sometimes nothing gets traced and no svg can be generated. You might see this in your VM:

```
started trace for <0.10645.2> in <0.10646.2>
Tracer <0.4131.0>
self() <0.10646.2>
Starting tracer results: {v_global_and_local_calls,
                             {ok,[{matched,
                                      'whistle_apps@localhost.localdomain',
                                      24456},
                                  {saved,1}]}}
trace for <0.10645.2> done
trace formatted to /tmp/eflame.trace.out


Writing to /tmp/eflame.trace.out for 0 processes... finished!
```

I haven't bothered looking into this yet, as rerunning the request to do another trace usually gets some output. I think it depends on how fast the request is processed?
