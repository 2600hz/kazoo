# Database Compaction

Because CouchDB/BigCouch uses an append-only file to write changes to, you must monitor the size of those files on disk and periodically *compact* them. Compaction is the process of taking all the current revisions in the file and writing a new file with just the *active* documents, effectively deleting all old revisions. Compaction can have dramatic results in recovered disk space and performance (as seek times are reduced on smaller files).

## Automatic Compaction

Kazoo allows the cluster operator to enable automatic compaction of all databases. A process will walk the list of known databases (updating each time the process starts again) and check each database against a heuristic to decide whether to compact or not.

## System Configuration Schema

#### Schema

Schema for kazoo_couch system_config



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`admin_port` | The CouchDB API port, typically 5986 | `integer()` | `5986` | `false` |
`allow_maintenance_db_delete` | kazoo_couch allow maintenance db delete | `boolean()` | `false` | `false` |
`api_port` | The CouchDB API port, typically 5984 | `integer()` | `5984` | `false` |
`compact_automatically` | kazoo_couch compact automatically | `boolean()` | `false` | `false` |
`default_chunk_size` | kazoo_couch default chunk size | `integer()` | `1000` | `false` |
`max_compacting_shards` | kazoo_couch maximum compacting shards | `integer()` | `2` | `false` |
`max_compacting_views` | kazoo_couch maximum compacting views | `integer()` | `2` | `false` |
`max_concurrent_docs_to_archive` | kazoo_couch maximum concurrent docs to archive | `integer()` | `500` | `false` |
`max_wait_for_compaction_pids` | kazoo_couch maximum wait for compaction pids | `integer()` | `360000` | `false` |
`min_data_size` | kazoo_couch minimum data size | `integer()` | `131072` | `false` |
`min_ratio` | kazoo_couch minimum ratio | `number()` | `1.2` | `false` |
`sleep_between_poll` | kazoo_couch sleep between poll | `integer()` | `3000` | `false` |
`use_bigcouch_direct` | kazoo_couch use bigcouch direct | `boolean()` | `true` | `false` |



## Operations

The compaction module [kt_compactor](https://github.com/2600hz/kazoo/blob/master/applications/tasks/src/modules/kt_compactor.erl) binds into the triggers for all databases. Based on the `tasks.browse_dbs_interval_s` in `system_config`, each database will be processed each time the timer expires. The default timeout is daily, but the next timer won't start until all the databases have been processed by **all** task modules bound for them. On large systems, there could be so many databases and operations could take a long time on them, that the next timer won't start for more than a day.

Typically there's nothing wrong with that, just know that the timer isn't a daily timer but instead is 84600 seconds after the last database is processed.

### Per-node settings

Its important to set per-couch-node settings if your setup varies from the default ports (5984 and 5986).

```bash
sup kapps_config set_node kazoo_couch admin_port 35986 couchdb@db001.host.com
```

Repeat for `admin_port` and `api_port` for each database node in your setup (`curl localhost:5984/_membership` if you need your list of nodes).

## API-initiated compaction

In addition to the automatic crawling of databases available, the [tasks API](../../crossbar/doc/tasks.md) contains methods for initiating compaction of the cluster, per-node, or per-database.

### Compact Everything

This is an input-less compaction job. At the end of the run, you should have a CSV with output about the database shards on each node with their disk/data usage before and after compaction.

1. Create the task:
    ```shell
    curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    'http://{SERVER}:8000/v2/tasks?category=compaction&action=compact_all'
    ```

2. Start the task:
    ```shell
    curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    'http://{SERVER}:8000/v2/tasks/{TASK_ID}'
    ```

3. Query the task's status:
    ```shell
    curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    'http://{SERVER}:8000/v2/tasks/{TASK_ID}'
    ```

4. Export the results
    ```shell
    curl -v -X GET \
    -o compaction_results.csv
    -H "Accept: text/csv"
    -H "X-Auth-Token: $AUTH_TOKEN"
    'http://{SERVER}:8000/v2/tasks/{TASK_ID}?csv_name=out.csv'
    ```
    ```csv
    node,database,before_disk,before_data,after_disk,after_data
    node3@127.0.0.1,faxes,804116,1544,804116,1544
    node2@127.0.0.1,faxes,554260,1544,1230100,1544
    node1@127.0.0.1,faxes,132372,1544,132372,1544
    ```

### Compact a node

If you prefer, you can specify a CSV with nodes to compact, including a flag to force compaction or use the heuristics to do it.

#### CSV columns

| Column | Description |
| `node` | Which node(s) to run compaction on |
| `force` | Whether to force compaction on each database or apply the ratio heuristic |

For instance, compact two nodes, forcing all databases to be compacted on the first node, and only those meeting the ratio criteria on the second node:

```csv
"node","force"
"bigcouch@db001.zone1.host.com","true"
"bigcouch@db004.zone3.host.co.uk","false"
```

#### API commands

Create the task:
    ```shell
    curl -v -X PUT \
    -H "X-Auth-Token: $AUTH_TOKEN" \
    -H "Content-Type: text/csv" \
    'http://localhost:8000/v2/tasks?category=compaction&action=compact_node' \
     --data-binary $'"node","force"\n"node1@127.0.0.1","false"\n"node2@127.0.0.1","true"'
    ```

Note: if you want to specify the CSV content in the args to cURL, you need to use `$'...'` to allow the `\n` to be passed to Crossbar properly. Otherwise create a CSV file and use `@file.csv` instead.

Start, query, and export the results as before.

### Compact a database

Compacting a database will ensure all shards on relevant nodes are compacted.

#### CSV columns

| Column | Description |
| `database` | Which database to run compaction on |
| `force` | Whether to force compaction on the database or apply the ratio heuristic |

For instance, compact two databases, forcing compaction on the first database, and apply the ratio heuristic to see if the second database should be compacted.

```csv
"database","force"
"system_config","true"
"accounts","false"
```

#### API commands

1. Create the task:
    ```shell
    curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -H "Content-Type: application/json" \
    --data-binary @/path/to/node.csv
    'http://{SERVER}:8000/v2/tasks?category=compaction&action=compact_db'
    ```

Start, query and export as before.

### Via SUP

You can compact all databases on a node:

```shell
> sup kt_compactor compact_node {BIGCOUCH@SERVER1.COM}
node, database, before_disk, before_data, after_disk, after_data
{BIGCOUCH@SERVER1.COM}, webhooks, 41056, 8273, 24672, 8273
{BIGCOUCH@SERVER1.COM}, token_auth, 24672, 283, 8288, 283
{BIGCOUCH@SERVER1.COM}, tasks, 28768, 2724, 12384, 2724
...
```

You can compact a database across all nodes:

```shell
> shell kt_compactor compact_db {DATABASE}
node, database, before_disk, before_data, after_disk, after_data
{BIGCOUCH@SERVER1.COM}, {DATABASE}, 24672, 4192, 16480, 4192
{BIGCOUCH@SERVER2.COM}, {DATABASE}, 24672, 4192, 16480, 4192
{BIGCOUCH@SERVER3.COM}, {DATABASE}, 24672, 4192, 16480, 4192
```

You can compact account databases by using the account id; compact MODBs with `{ACCOUNT_ID}-{YYYY}{MM}`.
