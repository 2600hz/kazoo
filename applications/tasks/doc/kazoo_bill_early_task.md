# Kazoo Early Bill Tasks

This tasks is send or bill the accounts a configurable days before first of the next billing cycle(first day of the month). This task is triggered everyday.

By default this only sends the reminder, but if bill early is enabled globally or for a specific account instead of sending the reminder it will make charges the due amount.

After the task is checking an account, and the current day is within the the early days amount:

1. If the account or system is configured to bill early, it ignores reminder and goes to processing the account for billing
1. If account or system is configured to send bill reminder notification, it goes to processing the account for sending the notification.
1. The value of `notifications.low_balance.bill_early_task_timestamp` from account definition will be read. This is the timestamp of the next due date. If current time lower than this value, then the is the first time we are visiting this account for this billing cycle an we should proceed, otherwise we're already processed this account and should continue to next account.
1. If the account has service plans assigned, the action (bill or sending reminder) will be performed.
1. If the action was successful we save the due date in `notifications.low_balance.bill_early_task_timestamp` of account's document to indicate the account is processed for this billing cycle.

## Parameters configurable at account level

Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`bill_early_enabled` | Controls whether we should bill early this account | `boolean()` | `false` | `false` |
`reminder_enabled` | Controls whether we should send bill reminder for this account a few days before due date | `boolean()` | `false` | `false` |


## Parameters Configurable at system level

Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`bill_early_enabled` | Controls whether we should bill early the accounts | `boolean()` | `false` | `false` |
`how_many_early_days` | How many days before first of the next month this task should start processing accounts | `integer()` | `5` | `false` |
`reminder_enabled` | Controls whether we should send bill reminder for the accounts a few days before due date | `boolean()` | `false` | `false` |
