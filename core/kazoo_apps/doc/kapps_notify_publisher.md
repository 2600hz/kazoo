### Notification Publisher

To publish a notification reliably to `teletype` you can use `kapps_notify_publisher`.

`kapps_notify_publisher` is wrapper around `kz_amqp_worker` and `kapi_notifications`, which publish notification using `kz_amqp_worker:call_collect/4`, so it waits for response from `teletype` application (or deprecated `notify` app) to send back whether or not it sends the notifications. Upon receiving a `timeout` or error from teletype, the notification payload will be saved in a `pending_notifications` database to retry to publish later by `kz_notify_resender` task in Tasks app.

#### Configuration

All configuration are in `system_config/notify`.

`should_persist_for_retry` and `notify_persist_exceptions` settings can be set in account level as well.

Key | Description | Type | Default
--- | ----------- | ---- | -------
`notify_persist_enabled` | Enabled persistence of publish notifications failure globally | `boolean` | `true`
`notify_persist_exceptions` | List of notification types have to be ignored to persist if they failed to publish | `array` | `["system_alert"]`
`notify_persist_temporary_force_exceptions` | List of notification types have to be ignored temporary globally to persist if they failed to publish | `array` | `[]`
`notify_publisher_timeout_ms` | Timeout in milliseconds when publishing notification using notify publisher | `integer` | `10000`
`should_persist_for_retry` | Enabled persist of failed to publish notifications for account/system | `boolean` | `true`

#### Enabling/Disabling persisting notification payload

There are two configuration to control whether or not to persist notification payload when it failed to published.

* `notify_persist_enabled` is global settings, can be utilized in occasions which you want intentionally disable persistence globally
* `should_persist_for_retry` is account level, if you want to disable persistence for a specific account

#### Disable persistence for specific type of notifications

Some notification are noisy (like `deregister`, `register`) and some are can be ignore if they didn't published, some are both noisy and can be ignore (`system_alert`).

To control which notification type should be excluded to persist, use these settings:

* `notify_persist_temporary_force_exceptions`: can be set in `system_config/notify`, useful if you want to permanently disable or temporarily ignore some notification type (e.g. you know phones are losing registrations and don't want to receive their notifications temporarily until the issue is resolved)
* `notify_persist_exceptions`: can be set both on the account- and system-level, same as above but allows to control this for a specific account
