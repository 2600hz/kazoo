### Maintenance


#### List pending notifications to be process

> List first 100 (if available) to be processed.

```shell
{SUP} notify_resend_maintenance pending
```

> List `{SHOW_COUNT}` number of pending notifications

```shell
{SUP} notify_resend_maintenance pending {SHOW_COUNT}
```

> List `{SHOW_COUNT}` number of pending notifications, optionally shows
> the whole JSON object if `{DETAILS}` is `true`

```shell
{SUP} notify_resend_maintenance pending {SHOW_COUNT} {Details}
```

> List pending notifications by notification type (e.g. voicemail_new, ...)

```shell
{SUP} notify_resend_maintenance pending_by_type {Type}
```

#### List notifications which reached their max retries

> List first 100 (if available)

```shell
{SUP} notify_resend_maintenance failed
```

> List `{{SHOW_COUNT}}` number of failed notifications

```shell
{SUP} notify_resend_maintenance failed {SHOW_COUNT}
```

> List `{SHOW_COUNT}` number of failed notifications, optionally shows
> the whole JSON object if `{DETAILS}` is `true`

```shell
{SUP} notify_resend_maintenance failed {SHOW_COUNT} {Details}
```

> List failed notifications by notification type (e.g. voicemail_new, ...)

```shell
{SUP} notify_resend_maintenance failed_by_type {Type}
```


#### Get a specific notification's details

```shell
{SUP} notify_resend_maintenance notify_info {Id}
```


#### Send a single notification

```shell
{SUP} notify_resend_maintenance send_notify {Id}
```


#### Get statistics of notification re-sender

```shell
{SUP} notify_resend_maintenance statistics
```


#### Delete a notification

```shell
{SUP} notify_resend_maintenance delete {Id}
```


#### Delete notifications older than a specific timestamp

> Timestamp is Gregorian seconds

```shell
{SUP} notify_resend_maintenance delete_older_than {Timestamp}
```

#### Delete notifications between specific timestamp period

> Timestamps are Gregorian seconds

```shell
{SUP} notify_resend_maintenance delete_between {START_TIMESTAMP} {END_TIMESTAMP}
```
