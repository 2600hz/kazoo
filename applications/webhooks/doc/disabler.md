# The Webhooks Disabler

A `gen_server` is started to monitor the webhook attempts that fail and to auto-disable any webhook that fails "too often" (defined later).

When an HTTP request is made to the configured webhook's `uri`, any non-200 response and any connectivity errors are treated as a failure and added to the `webhooks_cache` cache. It includes the `AccountId`, `HookId`, and Gregorian milliseconds timestamp of when the failure occurred.

## Failure settings

- `attempt_failure_expiry_ms`: Failure cache entries are kept in cache for value of `attempt_failure_expiry_ms`. It can be defined in the account's config doc (`config_webhooks`) or `system_config/webhooks`. It defaults to `60000` or 60 seconds.

- `attempt_failure_count` determines how many failures within the `attempt_failure_expiry_ms` window constitue "too often"; webhooks exceeding `attempt_failure_count` in that time period are automatically disabled. It can be defined in the account's config doc (`config_webhooks`) or `system_config/webhooks`. It defaults to `6`.

Using the defaults, then, means any webhook that fails 6 times within a minute should be auto-disabled.

## Disabler Run

Given the `system_config` value of `attempt_failure_expiry_ms`, the disabler will run a check every `attempt_failure_expiry_ms div 4` (default of 60s means a check every 15s).
