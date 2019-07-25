This is port of [leader cron](https://github.com/jeraymond/leader_cron) to use `amqp_leader` instead of `gen_leader`.

# amqp_cron

amqp_cron provides a distributed task scheduler for executing task
periodically in an Erlang cluster

Participating members of the cluster elect a leader node. The leader
node schedules and executes given tasks as per their schedules. Should
the leader node become unavailable a new leader is elected who resumes
task execution responsibilities.

Tasks are defined by specifying a function in a particular module with
given arguments to run according to a schedule. The schedule types
are:

* sleeper - sleep a specified number of milliseconds between task
  executions
* one shot - execute task once at a given date and time or after a
number of milliseconds
* cron - define a schedule very similar to Unix cron

## Usage

Startup amqp_cron on each participating node (do this on all nodes):

```erlang
amqp_cron:start_link().
```

Schedule tasks from any node. Here a cron style schedule is defined.

```erlang
amqp_cron:schedule_task({cron, {[5], all, all, all, all}},
                          {io, format, [user, "It is 5 past the hour", []]}).
```

That's it. In this example the task prints, "It is 5 past the hour" on the
leader node at 5 minutes past every hour.

You can also schedule anonymous functions:
```erlang
F = fun(Device, Format, Args) -> io:format(Device, Format, Args) end,
amqp_cron:schedule_task({cron, {[5], all, all, all, all}},
                          {F, [user, "It is 5 past the hour", []]}).
```

See the `amqp_cron_task` module for full scheduling details (or `make doc`).

## Building

Run `make` or include as a [rebar](https://github.com/basho/rebar) dependency
in your project.

## Testing

To run all tests run `make check`. This runs both the eunit tests as well as the
common tests. These can also be run individually via `make eunit` and `make ct`.

## Static Analysis


### Dialyzer

To perform the [Dialyzer](http://www.erlang.org/doc/man/dialyzer.html)
static analysis of the code run `make dialyzer`. Run `make build_plt`
once before performing the static analysis to build the plt file required
by dialyzer.

### Xref

To run the [Xref](http://www.erlang.org/doc/apps/tools/xref_chapter.html)
cross reference tool run `make xref`.
