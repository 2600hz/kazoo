### Notification Publisher

To publish a notification reliably to `teletype` you can use `kapps_notify_publisher`.

`kapps_notify_publisher` is wrapper around `kz_amqp_worker` and `kapi_notifications`, which publish notification using `kz_amqp_worker:call_collect/4`, so it waits for response from `teletype` application (or depreacted `notify` app) to send back whether or not it sends the notifications. Upon recieiving a `timeout` or error from teletype, the notification payload will be saved in a `pending_notifications` database to retry to publish later by `kz_notify_resender` task in Tasks app.

#### Configuration

All configuration are in `system_config/notify`.

`should_presist_for_retry` and `notify_presist_exceptions` settings can be set in account level as well.

Key | Description | Type | Default
--- | ----------- | ---- | -------
`notify_presist_enabled` | Enabled presistence of publish notifications faliure globally | `boolean` | `true`
`notify_presist_exceptions` | List of notification types have to be ignored to presist if they failed to publish | `array` | `["system_alert"]`
`notify_presist_temprorary_force_exceptions` | List of notification types have to be ignored temprorary globally to presist if they failed to publish | `array` | `[]`
`notify_publisher_timeout_ms` | Timeout in milliseconds when publishing notification using notify publisher | `integer` | `10000`
`should_presist_for_retry` | Enabled presist of failed to publish notifications for account/system | `boolean` | `true`

#### Enabling/Disabling presisting notification payload

There are two confguration to control whether or not to presist notfication payload when it failed to published.

* `notify_presist_enabled` is global settings, can be utilized in occasions which you want intentionally disable persistence globally
* `should_presist_for_retry` is account level, if you want to disable presistence for a sepific account

#### Disable presistence for specific type of notifications

Some notification are noisy (like `deregister`, `register`) and some are can be ignore if they didn't published, some are both noisy and can be ignore (`system_alert`).

To control which notification type should be excluded to presist, use these settings:

* `notify_presist_temprorary_force_exceptions`: can be set in `system_config/notify`, useful if you want permanently disable or temprorly ignore some notification type (e.g. you know phone are losing regidtration and don't want recieve their notifications temproray until the issue is resovled)
* `notify_presist_exceptions`: can be set both in account level and system level, same as above but for allows to control this for a specific account
