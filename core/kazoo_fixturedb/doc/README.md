# Kazoo FixtureDB

What can you say about a data application? That it loves to be distributed and fault tolerant? That it listens to data streams and not understand a bit?

## Overview

General speaking, FixtureDB removes dependency of Kazoo Data Manager to CouchDB and RabbitMQ for running EUnit tests and removes the burden of isolating source codes with `-ifdef(TEST)` when it comes to reading from database, which in part result in a tiny little bit more coverage (if any) by running through all Kazoo Data Manager code.

FixtureDB acts as a central repository for all fixtures which we simply use during tests.

When writing your EUnit test you can use [_fixture_ test representation](http://erlang.org/doc/apps/eunit/chapter.html#Fixtures) to setup a your test environment by starting a dummy connection to FixtureDB, and cleanup the connection later.

Below is an example with `setup` and `cleanup` method. In `setup()` function we're starting `kazoo_config` applications to read the default [`config-test.ini`](../../../rel/config-test.init) and then starts the Kazoo data link supervisor. After these steps most of the calls to `kz_datamgr` functions will use FixtureDB (few operations, like view maintenance cleanup, are not implemented).

##### Example EUnit Test Using Setup/Cleanup Method

```erlang
-spec render_test_() -> any().
render_test_() ->
    {setup
    ,fun setup/0
    ,fun cleanup/1
    ,fun(_ReturnOfSetup) ->
             [{"your awesome test"
              ,?_assertEqual(good, evil)
              }
             ]
     end
    }.

setup() ->
    ?LOG_DEBUG(":: Setting up Kazoo FixtureDB"),

    {ok, _} = application:ensure_all_started(kazoo_config),
    {ok, LinkPid} = kazoo_data_link_sup:start_link(),

    LinkPid.

cleanup(LinkPid) ->
    _ = erlang:exit(LinkPid, normal),
    _ = application:stop(kazoo_config),
    ?LOG_DEBUG(":: Stopped Kazoo FixtureDB").
```

### Database File Structure

All of the fixtures are reside in [core/kazoo_fixturedb/priv/dbs](../priv/dbs). In `dbs` directory, each sub-directory is represented a database. For example `dbs/accounts` is represented `accounts` database. Inside these database directories, there are sub-directories to store the JSON document for this database (in `docs` directory) and CouchDB view result (in `views` directory):

```shell
$ tree core/kazoo_fixturedb/priv/dbs/

core/kazoo_fixturedb/priv/dbs/
...
├── accounts
│   └── docs
│       ├── account0000000000000000000000001.json
│       ├── account0000000000000000000000002.json
│       └── account0000000000000000000000003.json
└── services
    ├── docs
    │   ├── account0000000000000000000000001.json
    │   ├── account0000000000000000000000002.json
    │   └── account0000000000000000000000003.json
    ├── view-index.csv
    └── views
        └── services+cascade_quantities-dffd4eaab58007e68e839f620b80d77c.json
...
```

#### Attachments reside in `docs` directory

Document attachments are stored in `docs` directory. Their file names are encoded by the document ID and attachment name, (see [Document Attachments](#document-attachments) section). Attachments are usually audio files, PDFs or pictures. FixtureDB has some default files in `priv/media_files`. You can simply use them by creating a symbolic link to them.

#### View Results Directory

View results are exactly the same result sets which `kz_datamgr:get_results/2,3` returns. It is your responsibility to write the correct view result structure and in the proper sort order. Complex queries have their own unique filename which is explained in the [View Results](#view-results) section.

## Using FixtureDB

FixtureDB acts as database driver for `kazoo_data`, so you need to start `kazoo_config` first to read the database configuration, then start `kazoo_data` connection link by running `kazoo_data_link_sup:start_link()` to bring up Kazoo data connection ETS table. `kazoo_data_link_sup` is a lite version of `kazoo_data_sup` which does not depend on `kazoo_amqp` and tracing capability to be available; its just making a new connection to a database (which in this case is FixtureDB).

OS environment variable `KAZOO_CONFIG` is necessary for `kazoo_config` to read the correct FixtureDB database configuration and by default is set to [`$(ROOT)/rel/config-test.ini`](../../../rel/config-test.ini) in [`kz.mk`](../../../make/kz.mk) file for `test`, `eunit`, `proper` and `fixture_db` targets.

In tests which access the database, you have to bring up `kazoo_config` and `kazoo_data_link_sup`:

```erlang
{'ok', _} = application:ensure_all_started('kazoo_config').
{'ok', _Pid} = kazoo_data_link_sup:start_link().
```

> **NOTE:** Don't forget to stop the `kazoo_config` and `kazoo_data_link_sup` after your test is finished (see [EUnit test example](#example-unit-test-using-setupcleanup-method) above), otherwise your test will fail because the `kazoo_config` and `kazoo_data_link_sup` are not shut down normally!

### Database

Each database has a directory of their own inside `core/kazoo_fixturedb/priv/dbs/`.

> **NOTE:** Database name in URL encoded, e.g. `account/xxxx` becomes `account%2Fac%2Fco%2Funtxxxx`.

In each database there are two directories: `docs` for storing regular JSON documents inside the database and `views` for storing view results.

To make it easy to map each attachment or complex view to their corresponding files in the `docs` and `views` directories, there are two CSV index files at the root of the database directory.

### Fixture JSON Documents

Documents are the exact copy of a real CouchDB documents. Document's filename is the `_id` of the document with `.json` extension in the end.

> **NOTE:** The `_id` in the filename is URL encoded, e.g. `_design/services` becomes `_design%2Fservices.json`.

You can use `kz_fixturedb_util:get_doc_path/2,3` to get document's file path.

Saving a document is not really saving the JObj in a file, instead it just returned an updated JObj with bumped `_rev` and updated `pvt_document_hash`. If database (the database folder) does not exists it returns error `{'error', 'not_found'}`. If you try to save a document which exists in the `docs` without correct `_rev` then the result is `{'error', 'conflict'}` as expected.

Delete document(s) is result in a bulk save in Couchbeam, so it returns a bulk save result:

```erlang
(fixturedb@hes.2600hz.com)12> kz_datamgr:del_docs(<<"accounts">>, [<<"file_not_exists">>, <<"account0000000000000000000000002">>]).

{ok,[{[{<<"ok">>,true},
       {<<"id">>,<<"file_not_exists">>},
       {<<"rev">>,<<"1-a91148dc2361d2ebba6278fc1352bedd">>},
       {<<"accepted">>,true}]},
     {[{<<"ok">>,true},
       {<<"id">>,<<"account0000000000000000000000002">>},
       {<<"rev">>,<<"744-81cd18e2bd386c5b3fcd6df4e731d596">>},
       {<<"accepted">>,true}]}]}
```

#### Utility Functions

If you change an existing JSON file manually and need to update `pvt_document_hash` you can use `kz_fixturedb_util:update_pvt_doc_hash(Path)`. This is necessary for example in Teletype notification templates in `system_config` database since during initializing a template, teletype checks if document hash is changed or not so it does not update template which is customized directly in database.

To update all JSON files document hash in FixtureDB use `kz_fixturedb_util:update_pvt_doc_hash/0`.

### Document Attachments

Each document's attachment is saved in database `docs` directory in a separate file. Attachment's filename is the MD5 hash of `{DOC_ID}` and `{ATTACHMENT_NAME}` ending in `.att`. Use `kz_fixturedb_util:get_att_path/3,4` to get the file path:

```erlang
(fixturedb@hes.2600hz.com)13> kz_fixturedb_util:get_att_path(<<"account%2Fac%2Fco%2Funt0000000000000000000000003-201710">>, <<"201710-vm_message0000000000000000000001">>, <<"vm-new_message.mp3">>).

"/opt/kazoo/core/kazoo_fixturedb/priv/dbs/account%2Fac%2Fco%2Funt0000000000000000000000003-201710/docs/5cef38cd0260632cf119c6fcf0c44f58.att"
```

For easily understand which file is referring which `doc_id+attachment_name` hash, there is an index file (`attachment-index.csv`) in the root of each database directory:

```shell
$ cat priv/dbs/account%2Fac%2Fco%2Funt0000000000000000000000003-201710/attachment-index.csv

doc_id, attachment_name, attachment_file_name
201710-vm_message0000000000000000000001, vm-new_message.wav, 5cef38cd0260632cf119c6fcf0c44f58.att
```

Putting an attachment is just returning the updated document JObj back. Deleting an attachment is returning tuple `{'ok', JObj}` which the JObj just have the `id` and `rev`. Both put and delete will return error tuple if the JSON document file does not exists in database.

For large attachments like voicemail or a fax, use the files from `core/kazoo_fixturedb/priv/media_files` (or if it does not exists add one). And then soft link it in the destination database. For example in Unix shell if you're already at `code/kazoo_fixturedb` directory:

```shell
kazoo_fixturedb $ ls -l priv/dbs/account%2Fac%2Fco%2Funt0000000000000000000000003-201710/docs/
lrwxrwxrwx 1 hehe hehe   39 Oct 28 17:45 5cef38cd0260632cf119c6fcf0c44f58.att -> ../../../media_files/vm-new_message.mp3
```

#### Utility Functions

* `kz_fixturedb_util:get_att_path(DbName, DocId, AName)`: you can use this to get the attachment file path and inspect the hashed filename
* `kz_fixturedb_util:add_att_to_index(DbName, DocId, AName)`: will create `doc_id+attachment_name` hash and add it to the `attachment-index.csv` file and make a symbolic link to a file in `core/kazoo_fixturedb/priv/media_files` if the `attachment_name` matches want of the file in that directory.

**Advanced Usage:** To use a customized data plan you can use arity 4 of above function (as first argument).

### View Results

FixtureDB uses a new cutting edge advanced high performance technique called Human® to compute the CouchDB view's result. Same like [attachments](#document-attachments), there is an index file for views at `view-index.csv` in each database root directory to help you keep track of filename and query.

All view result are in the `views` database's directory. Since views are tricky when it comes to `reduce`, `group_level`, `startkey` and etc..., every time FixtureDB sees these view options, it hashes the view options and will add it to the file name. For example:

```erlang
(fixturedb@hes.2600hz.com)17> kz_fixturedb_util:get_view_path(<<"account%2Fac%2Fco%2Funt0000000000000000000000001">>, <<"users/crossbar_listing">>, []).

"/opt/kazoo/core/kazoo_fixturedb/priv/dbs/account%2Fac%2Fco%2Funt0000000000000000000000001/views/users+crossbar_listing.json"
```

As you can see above when there is no complex view options (in this example an empty options `[]`) the filename is simply reflecting the view name. Here is an example with a complex view options:

```erlang
(fixturedb@hes.2600hz.com)18> kz_fixturedb_util:get_view_path(<<"account%2Fac%2Fco%2Funt0000000000000000000000001">>, <<"users/crossbar_listing">>, [{key,<<"user">>},include_docs]).

"/opt/kazoo/core/kazoo_fixturedb/priv/dbs/account%2Fac%2Fco%2Funt0000000000000000000000001/views/users+crossbar_listing-ba9ee0020a95e519332f16eacf0a3324.json"
```

The list of view options which will result in creating a hashed file name are defined in [`kz_fixturedb.hrl`](../src/kz_fixturedb.hrl). Current options are:

```erlang
-define(DANGEROUS_VIEW_OPTS, [startkey, endkey, key
                             ,keys, group, group_level
                             ,reduce, list, skip
                             ]).
```

#### Utility Functions

* `kz_fixturedb_util:get_view_path(DbName, Design, Options)`: get file path for your view query. After adding a view result to `views` directory use
* `kz_fixturedb_util:add_view_to_index(DbName, Design, Options)`: adds the view with options to the index file

**Advanced Usage:** To use a customized data plan you can use arity 4 of above function (as first argument).

#### Note about `include_docs`

If `include_docs` is set, you don't need to include docs in your view result, `kz_fixturedb_view` will add each document to the result set based on each result keys. So you can instead add the document in database `doc` directory.

#### Note about `limit` and `descending`

As a rule write your view result in the `ascending` order. Again `kz_fixturedb_view` is reversing the the result if `descending` is set.

This only works if your query does not have `reduce`, `group` or `group_level` view options.

This includes `limit` too, if your test case is not have a complex view options (it doesn't have any of `?DANGEROUS_VIEW_OPTS`) you have to include all view results in the file `kz_fixturedb_view` keep taking care of splitting the result set.

#### Note about `?DANGEROUS_VIEW_OPTS`

You as a test designer must have a complete scenario of your queries that your test needs. If your query is using `startkey`, `endkey`, `keys`, `key` `reduce`, `group*` or etc... make a rough plan of what each query is and `kz_fixturedb_util:get_view_path/3,4` then create the view result file for them.

### How to Get the Data Plan

It's simple:

```erlang
SystemPlan = kzs_plan:plan().
PlanForThisDb = kzs_plan:plan(SomeDb).
```

### Have Database in Other Application Directory (Experimental)

> **Note:** currently there is no way to have update the `kz_dataconnection` to use this feature.

FixtureDB has an experimental way to read database from other path than the default `core/kazoo_fixturedb/priv/dbs` path. It could be useful if you want to test an applications which maybe requires a lot of document and you want to put them in the default path.

For this to work you have to setup a new connection with these options set

```erlang
#{test_app => atom() %% required, is the name app of which you're writing test
 ,test_db => ne_binary() %% required, the name of your database which you want fixturedb read from
 ,test_db_subdir => atom() %% optional, the path to where is the root dbs directory, default is priv/ folder of your app
}
```

The exact way how to make a new connection this is remain un-discovered :) But it works for every database (including system_data, system_config and services).

Better option is use data plan (although you may loose the ability to use those three databases in this way). This way is un-discovered too.

## FixtureDB Shell Target

There is a target in the root [`Makefile`](../../../Makefile) and [`kz.mk`](../../../make/kz.mk) for start a shell with all core, applications and deps in path and has `KAZOO_CONFIG` set, so you can easily run `kz_fixturedb_util` functions where ever you're in Kazoo project path.

```shell
kazoo $ make fixture_shell
```

```erlang
Erlang/OTP 19 [erts-8.3] [source] [64-bit] [smp:4:4] [async-threads:10] [hipe] [kernel-poll:false]

Eshell V8.3  (abort with ^G)
(fixturedb@hes.2600hz.com)1> application:ensure_all_started(kazoo_config)
16:36:34.542 [info] Application lager started on node 'fixturedb@hes.2600hz.com'
16:36:34.542 [info] Application kazoo started on node 'fixturedb@hes.2600hz.com'
16:36:34.544 [info] Application zucchini started on node 'fixturedb@hes.2600hz.com'
16:36:34.587 [info] loaded configs from file /home/hesaam/work/2600hz/kazoo/rel/config-test.ini
16:36:34.588 [notice] loaded settings : [{amqp,[{uri,"amqp://guest:guest@127.0.0.1:5672"}]},{data,[{config,kazoo_fixturedb}]},{kazoo_fixturedb,[{driver,kazoo_fixturedb},{tag,local}]},{kazoo_apps,[{cookie,change_me}]},{ecallmgr,[{cookie,change_me}]},{log,[{syslog,none},{console,none},{file,none}]}]
16:36:34.615 [notice] setting zone to local
16:36:34.616 [info] Application kazoo_config started on node 'fixturedb@hes.2600hz.com'
{ok,[crypto,inet_cidr,observer,syntax_tools,compiler,
     goldrush,lager,kazoo,zucchini,kazoo_config]}
(fixturedb@hes.2600hz.com)2>
(fixturedb@hes.2600hz.com)2> kazoo_data_link_sup:start_link().
16:36:46.073 [info] Application asn1 started on node 'fixturedb@hes.2600hz.com'
16:36:46.073 [info] Application public_key started on node 'fixturedb@hes.2600hz.com'
16:36:46.089 [info] Application kazoo_fixturedb started on node 'fixturedb@hes.2600hz.com'
16:36:46.126 [info] waiting for first connection...
16:36:46.126 [info] adding connection
16:36:46.142 [info] start connection
16:36:46.142 [info] trying to connect kazoo_fixturedb
16:36:46.365 [info] connected to local: {"kazoo":"Willkommen","version":"0.0.0.0.0.0.0.1","features":["cool"],"vendor":{"name":"the Great FixtureDB Committee"}}
{ok,<0.117.0>}
(fixturedb@hes.2600hz.com)3>
```
