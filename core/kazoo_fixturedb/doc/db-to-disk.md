# How to take a database in KAZOO to disk for FixtureDB

It is helpful to do testing in KAZOO when developing a feature or testing a fix. However, to put all that work into a repeatable test when it involves the database is challenging with FixtureDB when the dataset is large.

This guide will show the steps to take an existing database and populate the FixtureDB filesystem with the appropriate files and view indexes.

## Database to disk

Let's say you need account db `9c2e035c8559b175e58146f6a31a3e67` to persist to FixtureDB. `kz_fixturedb_maintenance:db_to_disk(<<"9c2e035c8559b175e58146f6a31a3e67">>).` will write all the JSON objects in the database to disk. If you want to be selective on which documents to persist, `kz_fixturedb_maintenance:db_to_disk(<<"account%2F9c%2F2e%2F035c8559b175e58146f6a31a3e67">>, FilterFun).` will do the job. `FilterFun` is a function of arity-1 that takes the document as an argument and returns a `boolean()` for whether to persist the document.

## View index to disk

FixtureDb requires the view index of a query to be statically configured into a file. Accomplish this by passing the relevant options into the following function:

   kz_fixturedb_util:view_index_to_disk(<<"account%2F9c%2F2e%2F035c8559b175e58146f6a31a3e67">>, <<"design/view">>, [include_docs]).

This will create the appropriately named view index `design+view-{options_hash}.json` and populate it with the results.
