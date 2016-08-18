/*
Section: Kazoo Couch
Title: Compaction
Language: en-US
*/

# Database Compaction

Because CouchDb/BigCouch uses an append-only file to write changes to, you must monitor the size of those files on disk and periodically *compact* them. Compaction is the process of taking all the current revisions in the file and writing a new file with just the *active* documents, effectively deleting all old revisions. Compaction can have dramatic results in recovered disk space and performance (as seek times are reduced on smaller files).

## Automatic Compaction

Kazoo allows the cluster operator to enable automatic compaction of all databases. A process will walk the list of known databases (updating each time the process starts again) and check each database against a heuristic to decide whether to compact or not.

### Check if auto-compaction is enabled

`sup couch_compactor_fsm compact_automatically`

This will return `true` or `false`, indicating whether the auto-compactor process is running on this node.

### Enable/Disable auto-compaction

`sup couch_compactor_fsm compact_automatically [boolean]`

`[boolean]` should be either `true` or `false`. This will only configure the compactor. The actual compaction process will not immediately start.

To immediately start auto-compaction (set the config up and start the compaction process):

`sup couch_compactor_fsm start_auto_compaction`

To stop auto-compaction:

`sup couch_compactor_fsm stop_auto_compaction`

## Compacting

### Compact Everything!

To compact all databases across all nodes:

`sup couch_compactor_fsm compact`

This will essentially run one pass of the auto-compactor.

### Compact A Node

To compact all databases on a single node:

`sup couch_compactor_fsm compact_node bigcouch@node.host.com`

### Compact a Database

To compact a database across all nodes:

`sup couch_compactor_fsm compact_db database`

`database` would be the un-encoded version of the database name. For insance, if you have an account with ID `bd119d242987c986443f3cf67eae4926`, to compact the db your SUP command would be:

`sup couch_compactor_fsm compact_db account/bd/11/9d242987c986443f3cf67eae4926`

To compact the `accounts` DB:

`sup couch_compactor_fsm compact_db accounts`

To compact a numbers DB (for prefix `+1415` for instance):

`sup couch_compactor_fsm compact_db numbers/+1415`

#### Compact on a specific node

If you only want to compact a specific database on a specifc node:

`sup couch_compactor_fsm compact_db bigcouch@node.host.com database`

The format of `database` follows the same rules as the all-nodes version.

## Status

You can query the status of the compactor:

    sup couch_compactor_fsm status
    {ok,[{node,<<"bigcoucn@bc1.2600hz.com">>},
         {db,<<"numbers/+1415">>},
         {wait_left,31644},
         {queued_jobs,none},
         {nodes_left,1},
         {dbs_left,1},
         {start_time,{{2014,7,28},{18,45,45}}},
         {elapsed_s,166}
        ]}

* `node`: The current node being compacted
* `db`: The current database being compacted
* `wait_left`: The compactor will pause between compaction jobs, to allow the node to recover a bit (compaction can be disk-intensive). `wait_left` is the time left, in seconds, until the next compaction command occurs.
* `queued_jobs`: A listing of jobs that are queued up behind the currently executing job
* `nodes_left`: How many nodes are left to compact as part of this job
* `dbs_left`: How many databases are left to compact as part of this job
* `start_time`: Datetime (in UTC) of when the current job started
* `elapsed_s`: How many seconds have elapsed since the current job started

You can also inspect for these specific values:

    sup couch_compactor_fsm nodes_left
    [<<"bigcouch@bc2.2600hz.com">>, <<"bigcouch@bc3.2600hz.com">>, <<"bigcouch@bc4.2600hz.com">>]

You can also query for `dbs_left`, `current_node`, `current_db`, and `current` which will return both the node and database.

## Cancelling Jobs

### Cancel all jobs (including queued)

`sup couch_compactor_fsm cancel_all_jobs`

Do note, this will cancel the currently running auto-compaction job; however, when the auto-compaction check occurs again, a new job will be started.

### Cancel the current job

`sup couch_compactor_fsm cancel_current_job`

The next job in the queue will execute.

### Cancel the current shard

You can also cancel a specific shard being compacted:

`sup couch_compactor_fsm cancel_current_shard`

## Configuration

The compactor's configuration can be found in the `system_config/kazoo_couch` doc. The following fields are available for tweaking:

* `autocompaction_check`: Defaults to 60000 ms (60 seconds); how often to check if auto-compaction is enabled, and if so and the compactor isn't running a job, start a compact job.
* `bigcouch_cookie`: The cookie value of all the BigCouch nodes.
* `min_data_size`: Defaults to 131072 bytes; the minimum size of a database's data to consider the database for compaction. If the database's data size is under the threshold, the database is skipped.
* `min_ratio`: Defaults to 1.2; the ratio of disk_size to data_size of a database. The larger the ratio, the more "junk" that is in the database file. Compaction will be skipped for any database under the ratio.
* `compact_automatically`: Whether to compact automatically.
* `max_wait_for_compaction_pids`: Defaults to 360000ms (360 seconds); some compaction jobs take a long time. This timer will stop waiting for the shard to finish and move on to the next shard.
* `max_compacting_shards`: Defaults to 2; the number of shards to compact in parallel. This will increase disk activity. Tread carefully increasing it.
* `max_compacting_views`: Defaults to 2; the number of views to compact in parallel. Same caveat as `max_compacting_shard`.
* `sleep_between_poll`: Defaults to 3000ms (3 seconds); Kazoo must poll the BigCouch node to know when a database has finished compacting. This setting is how long to wait between polling.
* `sleep_between_compaction`: Defaults to 60000ms (60 seconds); how long to sleep between compaction jobs. Increase if BigCouch nodes are being impacted by compaction (will allow them to catch up).
* `sleep_between_views`: Defaults to 2000ms; typically lower impact, so no need to wait as long. How long to wait between view compactions.

## Troubleshooting

### Failure to connect

If the Kazoo node can't talk to the BigCouch node directly (using Distributed Erlang), this typically means the BigCouch cookie isn't set properly in the system_config.

Check the cookie:

`sup kapps_config get kazoo_couch bigcouch_cookie`

If it is returning an improper value, set it to the proper cookie:

`sup kapps_config set kazoo_couch bigcouch_cookie [cookie_value]`

### Warning while compacting

When you manually compact a db, especially if the Kazoo node has recently started up, you may see warnings like:

    5> couch_compactor_fsm:compact_db(<<"bigcouch@db01.somehost.com">>,<<"some_db">>).
    =ERROR REPORT==== 16-May-2012::23:39:09 ===
    global: 'kazoo_con_1337211446@kapps.somehost.com' failed to connect to 'bigcouch@db01.somehost.com'
    =ERROR REPORT==== 16-May-2012::23:39:09 ===
    global: 'kazoo_con_1337211446@kapps.somehost.com' failed to connect to 'bigcouch@db02.somehost.com'
    =ERROR REPORT==== 16-May-2012::23:39:10 ===
    global: 'kazoo_con_1337211446@kapps.somehost.com' failed to connect to 'bigcouch@db03.somehost.com'
    =ERROR REPORT==== 16-May-2012::23:39:10 ===
    global: 'kazoo_con_1337211446@kapps.somehost.com' failed to connect to 'bigcouch@db04.somehost.com'
    done

These are ignorable and are part of the initial connection between a Kazoo and BigCouch node.
