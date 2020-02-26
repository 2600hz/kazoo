## SUP-able functions

| Function | Arguments | Description |
| -------- | --------- | ----------- |
| `db_to_disk/1` | `(Database)` | |
| `db_to_disk/2` | `(Database,FilterFun)` | |
| `dummy_plan/0` |  | |
| `new_connection/0` |  | |
| `new_connection/1` | `(Map)` | |


## `db_to_disk/1,2`

Let's say you need account db `9c2e035c8559b175e58146f6a31a3e67` to persist to FixtureDB. `kz_fixturedb_maintenance:db_to_disk(<<"9c2e035c8559b175e58146f6a31a3e67">>).` will write all the JSON objects in the database to disk. If you want to be selective on which documents to persist, `kz_fixturedb_maintenance:db_to_disk(<<"account%2F9c%2F2e%2F035c8559b175e58146f6a31a3e67">>, FilterFun).` will do the job. `FilterFun` is a function of arity-1 that takes the document as an argument and returns a `boolean()` for whether to persist the document.

## `dummy_plan/0` and `new_connection/0,1`

Sometimes you're in `fixture_shell` (using `make fixture_shell`) and you don't want care about starting kazoo_data and kazoo_fixturedb and want just to grab some docs using kazoo_fixturedb directly.
In this case instead you can use these functions to get dummy plan and connection.
