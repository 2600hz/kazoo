# Ledger Rollover Task

The ledger rollover task, at the start of the month, walks the accounts and rolls over their ledger amount from the previous month to the new MODB.

There is also a task to refresh the primary ledgers view, `totals_by_source`, to make sure the index isn't too far behind when the monthly rollover task occurs.

## Rollover

Rollover sums up the ledgers of the previous month's MODB and creates a new ledger in the now-current MODB for that amount.

Set `tasks.ledger_rollover`'s `rollover_in_parallel` to control how many accounts to roll over at a time.

## Refresh

Refresh the view index for the `ledgers/totals_by_source` view to process any docs.

For reference, running the rollover on an unindexed MODB with 12,000 docs (318 of which are ledger docs) took ~6 seconds. Running the same view on an up-to-date index took 70 milliseconds. Similar to CDRs, this is a good one to enable.

Set `tasks.ledger_rollover`'s `refresh_view_enabled` to `true` to enable the job (performed daily).

Set `tasks.ledger_rollover`'s `refresh_in_parallel` to control how many MODBs to refresh at a time.
