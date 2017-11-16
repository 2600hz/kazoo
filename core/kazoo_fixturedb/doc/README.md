# Kazoo FixtureDB

What can you say about a data application? That it loves to be distributed and fault tolerant? That it listens to data streams and not understand a bit? yeah, that.

## Overview

General speaking, FixtureDB removes dependency of Kazoo Data Manager for AMQP and Kazoo Cache and removes the burden of isolating source codes with `-ifdef(TEST)` when it comes to reading from database, which in parts result in a tiny little bit more coverage (if any).

FixtureDB acts as central repository for all fixtures which we simply use during tests.

When writing your EUnit test you can use [_fixture_ test representation](http://erlang.org/doc/apps/eunit/chapter.html#Fixtures) to setup a dummy connection to FixtureDB, and cleanup the connection later.

Below is an example with `setup` and `cleanup` method, in `setup()` we're starting `kazoo_config` applications to read the default [`config-test.ini`](../../../rel/config-test.init and then starts the Kazoo Data connection manager. After these steps most of the calls to `kz_datamgr` functions will use FixtureDB (few operations, like view maintenance cleanup, are not implemented, also save document/put attachments simply return what you submitted without actually writing anything).

* Sample test using setup/cleanup method

```erlang
-spec render_test_() -> any().
render_test_() ->
    {setup
    ,fun setup/0
    ,fun cleanup/1
    ,fun(_ReturnOfSetup) ->
             [{"you're awesome test"
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
    _DataLink = erlang:exit(LinkPid, normal),
    Ref = monitor(process, LinkPid),
    receive
        {'DOWN', Ref, process, LinkPid, _Reason} ->
            _KConfig = application:stop(kazoo_config),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: ~p kazoo_config: ~p", [_DataLink, _KConfig])
    after 1000 ->
            _KConfig = application:stop(kazoo_config),
            ?LOG_DEBUG(":: Stopped Kazoo FixtureDB, data_link: timeout kazoo_config: ~p", [_KConfig])
    end.
```

## Using FixtureDB

FixtureDB acts as database driver for `kazoo_data`, so basically it needs `kazoo_config` reads the database configuration. You need to start `kazoo_config`, then to make `kazoo_data` know about FixtureDB, `kazoo_data_link_sup:start_link()` should be called to bring up Kazoo data connection ETS table. After that `kzs_plan` can find the connection and plan for database operation on FixtureDB.

OS environment `KAZOO_CONFIG` is necessary for `kazoo_config` to read the database configuration and by default is set to [`$(ROOT)/rel/config-test.ini`](../../../rel/config-test.ini) in [`kz.mk`](../../../make/kz.mk) file for `test`, `eunit` and `proper` target.

In the test which need accessing to database you have to bring up `kazoo_config` and `kazoo_data_link_sup`:

```erlang
{'ok', _} = application:ensure_all_started('kazoo_config').
{'ok', _Pid} = kazoo_data_link_sup:start_link().
```

> **NOTE:** Don't forget to stop the `kazoo_config` and `kazoo_data_link_sup` after you test!

All of the fixtures are in [core/kazoo_fixturedb/priv/dbs](../priv/dbs). In `dbs` directory each sub-directory is represented a database. For example `dbs/accounts` is represented `accounts` database. Inside these database directories, there are sub-directories to store the JSON document for this database (in `docs` dir) and CouchDB view result (in `views` dir):

```shell
$ tree core/kazoo_fixturedb/priv/dbs/

core/kazoo_fixturedb/priv/dbs/
...
├── accounts
│   └── docs
│       ├── 009afc511c97b2ae693c6cc4920988e8.json
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

### Fixture JSON Documents

Documents are the exact copy of a real CouchDB documents. Document's filename is the `_id` of the document with `.json` extension.

> **NOTE:** The `_id` in the filename is URL encoded, e.g. `_design/services` becomes `_design%2Fservices.json`.

You can use `kz_fixturedb_util:get_doc_path/2,3` to get document's file path.

Saving a document is not really saving the JObj in a file, instead it just returned an updated JObj with bumped `_rev` and updated `pvt_document_hash`. If database (the database folder) does not exists it returns error `{'error', 'not_found'}`. If you try to save a document which exists in the `docs` without correct `_rev` then the result is `{'error', 'conflict'}` as expected.

Delete document(s) is result in a bulk save in CouchBeam, so to mimic this behavior it returns a bulk save result:

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

If you changed an already exists JSON file manually with a text editor and need to update `pvt_document_hash` you can use `kz_fixturedb_util:update_pvt_doc_hash(Path)`. This is necessary for example in Teletype notification templates in `system_config` database since during initializing a template teletype checks if document hash is changed or not so it does not updates template which is customized directly in database.

To update all JSON files document hash in FixtureDB use `kz_fixturedb_util:update_pvt_doc_hash/0`.

### Attachments

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

When you're adding an attachment use `kz_fixturedb_util:get_att_path/3,4` to get the file path and save it there then use `kz_fixturedb_util:add_att_path_to_index/3,4` to add the attachment file name to the `attachment-index.csv` file.

Putting an attachment is just returning the updated document JObj back. Deleting an attachment is returning tuple `{'ok', JObj}` whic JObj just have `id` and `rev`. Both put and delete will return error tuple if the JSON document file does not exists in database.

For large attachments like voicemail or a fax, use the files from `core/kazoo_fixturedb/priv/media_files` (or if it does not exists add one). And then soft link it in the destination database. For example in Unix shell if you're already at `code/kazoo_fixturedb` directory:

```shell
kazoo_fixturedb $ ln -sf ../../../media_files/vm-new_message.mp3 priv/dbs/account%2Fac%2Fco%2Funt0000000000000000000000003-201710/docs/5cef38cd0260632cf119c6fcf0c44f58.att
kazoo_fixturedb $
kazoo_fixturedb $ ls -l priv/dbs/account%2Fac%2Fco%2Funt0000000000000000000000003-201710/docs/
lrwxrwxrwx 1 hehe hehe   39 Oct 28 17:45 5cef38cd0260632cf119c6fcf0c44f58.att -> ../../../media_files/vm-new_message.mp3
```

### View Result

FixtureDB uses a new advanced high performance technique called Human® to compute the CouchDB view's result.

All view result are `views` database's directory. Since views are tricky when it comes to `reduce`, `group_level`, `startkey` and etc..., when FixtureDB sees these view options, it hashes the view options and will add it to the file name. For example:

```erlang
(fixturedb@hes.2600hz.com)17> kz_fixturedb_util:get_view_path(<<"account%2Fac%2Fco%2Funt0000000000000000000000001">>, <<"users/crossbar_listing">>, []).

"/opt/kazoo/core/kazoo_fixturedb/priv/dbs/account%2Fac%2Fco%2Funt0000000000000000000000001/views/users+crossbar_listing.json"
```

As you can see above when there is no complex view options (in this example an empty options `[]`) the filename is simply reflect the view name. Here is an example with a complex view options:

```erlang
(fixturedb@hes.2600hz.com)18> kz_fixturedb_util:get_view_path(<<"account%2Fac%2Fco%2Funt0000000000000000000000001">>, <<"users/crossbar_listing">>, [{key,<<"user">>},include_docs]).

"/opt/kazoo/core/kazoo_fixturedb/priv/dbs/account%2Fac%2Fco%2Funt0000000000000000000000001/views/users+crossbar_listing-ba9ee0020a95e519332f16eacf0a3324.json"
```

As you can see to get file path for your view query use `kz_fixturedb_util:get_view_path/3,4`. There is an index file for view's file (`view-index.csv`) in each database directory to help you keep track of filename and query. After adding a view result to `views` directory use `kz_fixturedb_util:add_view_path_to_index/3,4` to update the index file.

The list of view options which are result in creating a hashed file name are defined in [`kz_fixturedb.hrl`](../src/kz_fixturedb.hrl) and currently are:

```erlang
-define(DANGEROUS_VIEW_OPTS, [startkey, endkey, key
                             ,keys, group, group_level
                             ,reduce, list
                             ]).
```

#### Note about `include_docs`

If `include_docs` is set, you don't need to include docs in your view result, `kz_fixturedb_view` will add each document to the result set based on each result keys. So you can instead add the document in database `doc` directory.

#### Note about `limit` and `descending`

As a rule write your view result in the `ascending` order. Again `kz_fixturedb_view` is reversing the the result if `descending` is set.

This includes `limit` too, if your test case is not have a complex view options (it doesn't have any of `?DANGEROUS_VIEW_OPTS`) you have to include all view results in the file `kz_fixturedb_view` keep taking care of splitting the result set.

#### Note about `?DANGEROUS_VIEW_OPTS`

You as a test designer must have a complete scenario of your queries that your test needs. If your query is using `startkey`, `endkey`, `keys`, `key` `reduce`, `group*` or etc... make a rough plan of what each query is and `kz_fixturedb_util:get_view_path/3,4` then create the view result file for them.

### How to Get the Data Plan

It's simple:

```erlang
SystemPlan = kzs_plan:plan().
PlanForThisDb = kzs_plan:plan().
```

### Have Database in Other Application Directory

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
