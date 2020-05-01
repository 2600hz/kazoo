# Kazoo Number Manager maintenance


## carrier_module_usage Prefix

* `Prefix`: the first few characters of an E164 number

> sup kazoo_numbers_maintenance carrier_module_usage +1555

```
numbers%2F%2B1555:
knm_local: 7
knm_managed: 14
Totals:
knm_local: 7
knm_managed: 14
```


## carrier_module_usage

> sup kazoo_numbers_maintenance carrier_module_usage

```
...snip...
Totals:
knm_local: 174
knm_managed: 34
knm_bandwidth2: 1
knm_telnyx: 10
knm_bandwidth: 3
knm_mdn: 1
```


## convert_carrier_module_number {NUM} Target

* `{NUM}`: a phone number convertible to E164 format
* `Target`: a valid carrier module name

> sup kazoo_numbers_maintenance convert_carrier_module_number 4152266659 knm_bandwid

```
Bad carrier module: knm_bandwid
```

> sup kazoo_numbers_maintenance convert_carrier_module_number 4152266659 knm_bandwidth2

```
updated carrier module to knm_bandwidth2 for 1:
+14152266659
updating carrier module failed for 0:
```


## convert_carrier_module Source Target Prefix

* `Source`: the carrier to match with (does not need to be valid)
* `Target`: a valid carrier module name
* `Prefix`: the first few characters of an E164 number

> sup kazoo_numbers_maintenance convert_carrier_module knm_band knm_bandwidth2 +1415

```
attempt to convert numbers with carrier module knm_band to knm_bandwidth2 in database numbers%2F%2B1415
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
```


## convert_carrier_module Source Target

* `Source`: the carrier to match with (does not need to be valid)
* `Target`: a valid carrier module name

> sup kazoo_numbers_maintenance convert_carrier_module knm_bandwidth knm_bandwidth2

```
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1202
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1236
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1248
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1312
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1315
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1318
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1414
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1415
updated carrier module to knm_bandwidth2 for 2:
+14152338397
+14152338421
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1424
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1425
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1434
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1454
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1464
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1497
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1498
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1499
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1504
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1555
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1665
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1666
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1800
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B1937
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B3980
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4242
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4252
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4262
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4411
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4412
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4413
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4414
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4415
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4416
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4417
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4418
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4419
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4420
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4423
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4424
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4428
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4429
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4433
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4470
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4480
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4484
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B4487
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
attempt to convert numbers with carrier module knm_bandwidth to knm_bandwidth2 in database numbers%2F%2B8835
updated carrier module to knm_bandwidth2 for 0:
updating carrier module failed for 0:
```


## reset_allowed_features_to_defaults_on_system_config

> sup kazoo_numbers_maintenance reset_allowed_features_to_defaults_on_system_config

```
Features allowed on system config document:
	carrier_name cnam e911 failover force_outbound inbound_cnam outbound_cnam port prepend ringback
```


## Maintenance commands to bulk copying number documents

These are sets of function operating on all number documents in each database(s) to copy the docs from number databases to account databases or vice versa.

You usually want to run these commands when you're importing account database from another to cluster into the current system and want to sync the numbers in account database to their corresponding number databases (or vice versa  you want to enforce numbers from number databases to their assigned account databases).


### Copy all numbers from account database(s) to their corresponding number database(s)

Commands to sync/import numbers from account databases to number databases.

`sup kazoo_numbers_maintenance copy_accounts_to_number_dbs`
: Copy all number docs from all account dbs to their corresponding number dbs.

`kazoo_numbers_maintenance:copy_accounts_to_number_dbs(AccountDbsList)`
: Copy all number docs from a list of account dbs to their corresponding number dbs _(Can only be called from Erlang Shell)_.

`sup kazoo_numbers_maintenance copy_single_account_to_number_dbs {ACCOUNT_DB}`
: Copy all number docs from a single account db to their corresponding number dbs.


### Copy all numbers from number database(s) to their assigned account database(s)

Commands to sync/enforce numbers from number databases to their assigned account databases.

`sup kazoo_numbers_maintenance copy_number_dbs_to_accounts`
: Copy all numbers from all number databases to their assigned account databases.

`kazoo_numbers_maintenance:copy_number_dbs_to_accounts(NumberDbsList)`
: Copy all numbers from a list of number databases to their assigned account databases _(Can only be called from Erlang Shell)_.

`sup kazoo_numbers_maintenance copy_single_number_db_to_accounts {NUMBERDB}`
: Copy all numbers from a single number database to their assigned account databases.


### Copy all numbers assigned to a specific account from number database(s) to their assigned account database

Commands to sync/enforce numbers assigned to a specific account.

`sup kazoo_numbers_maintenance copy_assigned_number_dbs_to_account {ACCOUNT}`
: Copy all number documents assigned to `{ACCOUNT}` from all number databases to account database.

`sup kazoo_numbers_maintenance copy_single_assigned_number_db_to_account {ACCOUNT} {NUMBERDB}`
: Copy all number documents assigned to `{ACCOUNT}` from a single number database to account database.


### Copy documents for a list of numbers

These commands are their counterpart commands above, but instead of operating on whole documents from database(s) they just documents for specified list of numbers.

`sup kazoo_numbers_maintenance copy_number_from_accountdb_to_numbdb {ACCOUNT} {NUMBER}`
: Copy the document for a single `{NUMBER}` from `{ACCOUNT}` database to its corresponding number database.

`kazoo_numbers_maintenance:copy_numbers_from_accountdb_to_numbdbs(Account, NumbersList)`
: Copy documents for `NumbersList` from `Account` database to their corresponding number databases _(Can only be called from Erlang Shell)_.

`sup kazoo_numbers_maintenance copy_number_from_db_to_assigned_account {NUMBER}`
: Copy the document for a single `{NUMBER}` from its number database to its assigned account database.

`kazoo_numbers_maintenance:copy_numbers_from_dbs_to_assigned_accounts(NumbersList)`
: Copy documents for a list of numbers `NumbersList` from their corresponding number databases to their assigned account databases  _(Can only be called from Erlang Shell)_.

`sup kazoo_numbers_maintenance copy_single_number_to_account_db {Number} {Account}`
: **DANGER:** Copy a single number from its number database to the `{Account}` database without checking if the number is really assigned to that account.


### Fix/Sync issues with number(s)

These commands are attempting to fix mosy common issues with out of sync numbers by doing a series fixes:

* Try to fix numbers assigned to the account in number databases which have wrong applications usage set (wrong `used_by` field), by looking for assigned numbers in account's callflow and trunkstore documents and check `used_by` field of number docs and fix the disparity.
* After fixing app usage, copy all numbers assigned to the account from number databases to the account's database.
* Remove numbers from account daatasbe that are not belong to the account (have wrong `pvt_assigned_to` field).
* Reconsile the services (to sync service doc if any number is removed from account database in previous step).

`sup kazoo_numbers_maintenance fix_account_db_numbers {ACCOUNT}`
: Try to fix all numbers assigned `{ACCOUNT}`.

`sup kazoo_numbers_maintenance fix_number {ACCOUNT} {NUMBER}`
: Try to fix a single number assigned to account.

`sup kazoo_numbers_maintenance fix_numbers(Account, NumbersList)`
: Try to fix a list of numbers assigned to account _(Can only be called from Erlang Shell)_.


### Fix numbers application usage

These commands try to fix numbers in databases which have wrong applications usage set (wrong `used_by` field), by looking for assigned numbers in assigned account's callflow and trunkstore documents and check `used_by` field of number docs and fix the disparity.

`sup kazoo_numbers_maintenance fix_apps_for_account_dbs`
: Fix apps for all numbers in all account databases.

`kazoo_numbers_maintenance:fix_apps_for_account_dbs(AccountsList)`
: Fix apps for all numbers for a list of account databases _(Can only be called from Erlang Shell)_.

`sup kazoo_numbers_maintenance fix_apps_for_single_account_db {ACCOUNT}`
: Fix apps for all numbers in a single account database.

`sup kazoo_numbers_maintenance fix_apps_for_number_dbs`
: Fix apps for all assigned numbers in all number databases.

`kazoo_numbers_maintenance:fix_apps_for_number_dbs(NumberDbsList)`
: Fix apps for all assigned numbers in a list of number databases _(Can only be called from Erlang Shell)_.

`kazoo_numbers_maintenance:fix_apps_in_number_dbs_for_accounts(AccountsList)`
: Fix apps for all numbers in number databases assigned to corsspanding account in `AccountsList` _(Can only be called from Erlang Shell)_.

`sup kazoo_numbers_maintenance fix_apps_in_number_dbs_for_single_account {ACCOUNT}`
: Fix apps for all numbers in number databases assigned to account `ACCOUNT`.

`sup kazoo_numbers_maintenance fix_apps_for_single_number_db {NUMBER_DB}`
: Fix apps for all numbers in a single number databases.


`kazoo_numbers_maintenance:fix_numbers_apps_in_accountdb(Account, NumbersList)`
: Fix apps for a list of numbers in a single account database _(Can only be called from Erlang Shell)_.

`kazoo_numbers_maintenance:fix_numbers_apps_in_numbdb(NumbersList)`
: Fix apps for a list of numbers in their corsspanding number databases _(Can only be called from Erlang Shell)_.


### Remove wrong assigned numbers from account database(s)

This check `pvt_assigned_to` field of all numbers in account database(s) and if its is not equal to the account ID, will remove the ndocument(s).

`sup kazoo_numbers_maintenance remove_wrong_assigned_from_accounts`
: Remove numbers with wrong assignment from all account databases.

`sup kazoo_numbers_maintenance remove_wrong_assigned_from_single_accountdb {Account}`
: Remove numbers with wrong assignment from a single account database.


### Commands to remove numbers if their won't exists in their corsspanding databases

These are handy commands which removing number documents if their are missing from their corsspanding databases. These are intended to cleanup numbers after in your test environment or dev environment. **DO NOT USE THESE COMMANDS IN PRODUCTION SERVERS**.

!!! Caution
    Use these commands if you really understand what are you doing.

`sup kazoo_numbers_maintenance destructively_remove_from_account_db_if_not_in_numdb/0`
: Remove numbers from all account databases if they're not exists in any number database.

`sup kazoo_numbers_maintenance destructively_remove_from_number_dbs_if_not_in_accountdb/0`
: Remove numbers from all number databases if they're not exists in any account database.

`sup kazoo_numbers_maintenance destructively_remove_from_single_account_db_if_not_in_numdb/1`
: Remove numbers from a single account database if they're not exists in any number database.

`sup kazoo_numbers_maintenance destructively_remove_from_single_numdb_if_not_in_accountdb/1`
: Remove numbers from all a single number database if they're not exists in any account database.
