# CouchDB Design Documents

Kazoo uses views to query CouchDB. Basically views are JavaScript codes which will be put in a document inside the database that they operate on. This special document is called Design document in CouchDB. Each Design document can implement multiple view. Please consult [Official CouchDB Design Documents](http://docs.couchdb.org/en/stable/ddocs/index.html) to learn more about how to write view.

Traditionally Kazoo have all design documents in simple JSON files store in each application's private directory. After a view is updated system administrator required to run a maintenance command (`kapps_maintenance refresh [Database]`) to read the view files and update them in databases. As long as Kazoo is deploy as a single node, there was no issue. Deploying Kazoo in multi-node scenario where each node is just running a subset of applications would potentially result in breaking this refresh maintenance command. For example if you run this command for refreshing views for an account database in a node which doesn't have Crossbar application installed will failed, since it can not find required account's database views.

With Kazoo 4.2, some views (account and MODB for example) are read from the JSON files and put in a specific database (`system_data` database). This process is called registering the views. Subsequence refresh command would read the view definitions from this database and is not required to run on a specific node.

This has been improved in Kazoo 4.3 by putting all views that Kazoo uses into `system_data` database. Upon system startup, each application is now responsible for registering the views that it provides (and creating their specific database if they have one) and optionally refreshing it.

## `kazoo` key

Starting from Kazoo 4.3, when creating design documents for use in CouchDB, Kazoo requires some metadata to assist in making sure the views in the databases are up to date.

At the root level of a design document, add a key `kazoo` with an object that tells Kazoo about which database(s) the design document belongs. This key is required.

First, an example:

```json
{
    "_id":"_design/foo",
    "kazoo": {
        "view_map": [
            {"classification":"account"},
            {"database":"system_config"}
        ]
    },
    "language":"javascript",
    "views": { // impement your views here }
}
```

The `view_map` is an array of objects that tell Kazoo in what databases should the design document be put. There are two options for the object: `classification` and `database`.

### Database Classification

Kazoo has the following standard database classifications:

Classification | Description
-------------- | -----------
`account` | The main account database
`aggregate` | Certain databases used to aggregate documents from across accounts
`deprecated` | Deprecated databases no longer used
`external` | Typically the "internal" CouchDB databases for users, dbs, and nodes
`modb` | Account MODB databases
`numbers` | Number databases
`ratedeck` | Ratedeck databases
`resource_selectors` | Account resource selector databases
`system` | System databases, like `system_config` or `system_media`

Of course, applications can create their own classifications as well; they need to bind in for the database classification binding in `kzs_util` to supply the binding of their database(s).

When using `classification` in `view_map`, this design document will be put on any databases that their classification is matching by this value. For our `_design/foo` example above refresh command to an account's database will put every account's views including `foo` into this database.

### Database

This key can be used to put the design document in specific database. In `_design/foo` example above, `"database":"system_config"` will put the design document in `system_config` database.

## Registering The Views

Each application is responsible for registering their views. Generally speaking, design documents are stored in `{APP}/priv/couchdb/views/` which means the `{APP}_app` (or other initialize routine) would call `kz_datamgr:register_views_from_folder('{APP}', "views")`. Also the application can bind to `register_view` binding, to allow system administrator to update the JSON file and re-register the view in a running node.

If the application is using their own database, they have to check and create it.

To put it all together, the application initialization should include some lines like below. It assumes that your application is using a database defined in `MY_DB_NAME` which defined in your application Erlang header file.

```erlang
my_app_maintenance:register_views(),
kapps_maintenance:bind(?APP, 'my_app_maintenance', 'register_views'),
my_app_maintenance:init_dbs(),
```

And in your `my_app_maintenance` module:

```erlang
-spec register_views() -> 'ok'.
register_views() ->
    _ = kz_datamgr:register_views_from_folder('{APP}', "views"),
    'ok'.

-spec init_dbs() -> 'ok'.
init_dbs() ->
    _ = kz_datamgr:db_create(?MY_DB_NAME),
    _ = kz_datamgr:refresh_views(?MY_DB_NAME)
    'ok'.
```

Once registered, `kapps_maintenance:migrate/0` and `kapps_maintenance:refresh/0` will ensure that what is on disk matches what's registered and will ensure all databases are updated as necessary with the appropriate version of the relevant design docs.

## Multiline views

While the `map` functions in Kazoo's CouchDB views are generally pretty small and simple, there are occasions where writing the `map` function as one long string isn't so great for reading the function.

Kazoo thus allows you to specify the `map` function using an array of strings to allow you to write the JavaScript as you might in a normal `.js` file and Kazoo will take care of flattening it into a string before loading into CouchDB.

Consider the `faxes.json` design doc. The `crossbar_listing` function is defined as:

```json
{
    // ...
    "views": {
        "crossbar_listing": {
            "map": [
                "function(doc) {",
                "  if (doc.pvt_type != 'fax' || doc.pvt_deleted)",
                "    return;",
                "  emit(doc._id, {",
                "    'modified': doc.pvt_modified",
                "  });",
                "}"
            ]
        },
    // ...
}
```

Here we see the function defined over multiple lines which aid in reading what the function is meant to do.

Contrast that with the "flattened" version":

```json
...,"views":{"crossbar_listing":{"map":"function(doc) { if (doc.pvt_type != 'fax' || doc.pvt_deleted) return; emit([doc.pvt_created, doc._id], {'id': doc._id, 'status': doc.pvt_job_status, 'to': doc.to_number, 'from': doc.from_number, 'created': doc.pvt_created}); }"},...
```

And this is a straightforward `map` function!

It is, therefore, recommended to use the multiline definitions for the on-disk design documents.

## Validate CouchDB Views

Script file `scripts/validate-js.sh` is assisting you to make sure the view files are parsable by CouchDB and has the `kazoo` key set properly. This script is depend on `couchjs` from CouchDB package to be installed and in the path.

After making sure `couchjs` is available you can run the script like below:

```bash
./scripts/validate-js.sh `find */*/priv/**/* -name '*.json'`
```

This will make sure the `map` function doesn't have any syntax error and parasble by CouchDB JavaScript engine, the JSON file has correct format and `kazoo` meta data for views are causing the views re-writing each other on the same database or classifications.
