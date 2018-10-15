# Couch Views

When creating design documents for use in CouchDB, Kazoo requires some metadata to assist in making sure the views in the databases are up to date.

## `kazoo` key

At the root level of a design document, add a key `kazoo` with an object that tells Kazoo about which database(s) the design document belongs.

First, an example:

```json
{"_id":"_design/foo"
 ,"kazoo":{
   "view_map":[
     {"classification":"account"}
     ,{"database":"system_config"}
   ]
 }
 ,"language":"javascript"
 ,"views":{...}
}
```

The `view_map` is an array of objects that tell Kazoo in what databases should the design document be put. There are two options for the object: `classification` and `database`.

### Classifications

Kazoo has the following standard classifications:

Classification | Description
-------------- | -----------
account | The main account database
aggregate | Certain databases used to aggregate documents from across accounts
deprecated | Deprecated databases no longer used
external | Typically the "internal" CouchDB databases for users, dbs, and nodes
modb | Account MODB databases
numbers | Number databases
ratedeck | Ratedeck databases
resource_selectors | Account resource selector databases
system | System databases, like `system_config` or `system_media`

Of course, applications can create their own classifications as well; they need to bind in for the database classification binding in `kzs_util` to supply the binding of their database(s).

## Registering the views

Once the design document is updated with the `kazoo` metadata, the application will need to register the views. Generally speaking, design documents are stored in `{APP}/priv/couchdb/views/` which means the `{APP}_app` (or other init routine) would call `kz_datamgr:register_views_from_folder('{APP}', "views")`.

Once registered, `kapps_maintenance:migrate/0` and `kapps_maintenance:refresh/0` will ensure that what is on disk matches what's registered and will ensure all databases are updated as necessary with the appropriate version of the relevant design docs.
