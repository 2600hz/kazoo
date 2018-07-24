### Notification re-sender task

This task is for try to re-publish notifications which failed to published at their generation time.

Notifications are tried to publish by `kapps_notify_publisher`, if the publisher failed to publish them for any reason (network problem, teletype are not running), it saves them to a database which this task is picking them up from there and retries to publish.

#### Configuration

All configuration are in `system_config/tasks.notify_resend`.

Key | Description | Type | Default
--- | ----------- | ---- | -------
`cycle_delay_time_ms` | Timeout in milliseconds between each cycle | `integer` | `300000`
`max_doc_read` | Max number of notifications to read from database for each cycle | `integer` | `20`
`max_retries` | Default max retries to re-publish | `integer` | `3`
`reschedule_rules` | Re-schedule rules for each notification type to apply | `object` | `{}`
`retry_after_fudge_s` | Constant time in seconds which would be multiplied with attempts to set retry time | `integer` | `600`

### Rescheduler

Notify resender read a specific number of jobs from database and tries to re-publish them. If for any reason the publish failed, the notification would be reschedule to retry to another time. This where you can apply your own reschedule logic to each notification type.

Take `voicemail_new` as example, maybe you want to reschedule it to run after 15 minutes after first attempt, 45 minutes after second attempts and etc.

Rescheduler logic a JSON object, each key of the object is the name of the notification type (`voicemail_new` for example) and their value is another JSON object which contains the rules for that notification type in the `rules` key.

This `rules` is JSON object as follow:

```json
{
	"name_of_the_rule_1": {
		"retry_after_s": 900,
		"retries": 4,
		"attempt": 1
	}
}
```

So if the publish attempt is the first attempt, this match this rule, and it would be retries in at least in `1000` milliseconds (as specified by `retry_after_s`).

The whole default `reschedule_rules` rules is as follow:

```json
{
  "voicemail_new": {
    "rules": {
      "after_one_day": {
        "retry_after_s": 86400,
        "retries": 4,
        "attempt": 4
      },
      "after_two_hours": {
        "retry_after_s": 7200,
        "retries": 4,
        "attempt": 3
      },
      "after_45_mins": {
        "retry_after_s": 2700,
        "retries": 4,
        "attempt": 2
      },
      "after_15_mins": {
        "retry_after_s": 900,
        "retries": 4,
        "attempt": 1
      }
    }
  }
}
```
