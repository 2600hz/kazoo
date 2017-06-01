
# Ananke *Callback features*

This application originate calls on various events

## *voicemail_saved*

When this event is received the following fields are checked

* `notify.callback.number` in corresponding vmbox
* or `voicemail.notify.callback.number` in vmbox owner's document

to get a number.

Also the following fields are checked

* `notify.callback.disabled` in vmbox
* `voicemail.notify.callback.disabled` in vmbox owner's document

If number was determined and notifications are not disabled, the call is being originated. The second leg is voicemail check callflow number.

To determinate callbacks `schedule` following fields are checked

* `notify.callback.schedule` in vmbox
* `voicemail.notify.callback.schedule` in vmbox owner's document
* `voicemail.notify.callback.schedule` in account document

`schedule` should be array of intervals (in seconds) between callback attempts.

If `schedule` is not found a call will determine schedule from `attempts` and `interval_s` values. In this case call will be attempted `attempts` times every `interval_s` seconds.

Call timeout can be adjasted by modifying `timeout_s` option.

These parameters can be set in (first one that's set wins).

* `attempts`
* * `notify.callback.attempts` in vmbox
* * `voicemail.notify.callback.attempts` in vmbox owner's document
* * `voicemail.notify.callback.attempts` in account's document
* * `voicemail.notify.callback.attempts` in ananke's system config (default 5)
* `interval_s`
* * `notify.callback.interval_s` in vmbox
* * `voicemail.notify.callback.interval_s` in vmbox owner's document
* * `voicemail.notify.callback.interval_s` in account's document
* * `voicemail.notify.callback.interval_s` in ananke's system config (default 300)
* `timeout_s`
* * `notify.callback.timeout_s` in vmbox
* * `voicemail.notify.callback.timeout_s` in vmbox owner's document
* * `voicemail.notify.callback.timeout_s` in account's document
* * `voicemail.notify.callback.timeout_s` in ananke's system config (default 20)

Note: for automatic `mailbox` number detection `single_mailbox_login=true` must be set in voicemail callflow module and user should use only one voice mailbox.

Note: options `voicemail.notify.callback.disabled = true` in vmbox owner's document and `notify.callback.disabled = false` in vmbox will allow callback notification.

How it works (brief version):
- after checking conditions (checking if called party is not disabled) we look for voicemail checking callflow.
- after that it checks that there are no notifications planned for this vmbox yet
- if there are planned notifications, nothing is done
- if not - the call is planned after given timeout
- if call attempt was successful - there will be no further notifications
- if call was not answered/was not successul and number of tries doesn't exceed limit, the next call is planned

## *cron events*

You may add `schedules` array to `ananke` system config.

*WARNING*: all times in UTC.

The item mandatory properties:

* `type` - strategy to run task, should be one of `once` | `periodic` | `every` (see below)
* `action` - task description (see below)

### Strategy types

#### `once` - task will be runned once at specified time

Optional properties:

* `second` at what second run task (default is `0`)
* `minute` at what minute run task (dafault is `0`)
* `hour` at what hour run task (default is `0`)
* `day` at what day run task (default is `1`)
* `month` at what day run task (default is `1`)
* `year` at which year run task (default is `1970`)

#### `periodic` - task will be runned repeatedly

Optional properties:

* `seconds` default is `0`
* `minutes` default is `0`
* `hours` default is `0`
* `days` default is `0`

All properties will be converted to seconds. Running task period will be total sum.

#### `every` - task will be runned when time will satisfy conditions. It's like unix cron

Optional properties:

* `minutes` - `all` or array of minutes, default `all`
* `hours` - `all` or array of hours, default `all`
* `month_days` - `all` or array of month days, default `all`
* `monthes` - `all` or array of monthes, default `all`
* `weekdays` - `all` or array of weekdays (0 - 6, 0 is Sunday), default `all`

### Task action format

Mandatory property:
* `type`  - currently only `check_voicemail` is supported

#### Action `check_voicemail`

When task of this type will be runned it will check voicemail box for unread messages.
If any is found, then application will do the same thing as in `voicemail_saved` event.

Mandatory properties:
* `account_id`
* `vmbox_id`

### Cron event example

```json
"schedules": [
    {
        "type": "every",
        "minutes": [23,24,25],
        "hours": [9],
        "month_days": "all",
        "monthes": "all",
        "weekdays": [1,2,3,4,5],
        "action": {
            "type": "check_voicemail",
            "account_id": "72fabca989b3102c28482c60070aac5b",
            "vmbox_id": "b2f1c691e3e733f52c7c8b4b621289bc"
        }
    },
    {
        "type": "once",
        "minute": 30,
        "hour": 2,
        "day": 1,
        "month": 1,
        "year": 2016,
        "action": {
            "type": "check_voicemail",
            "account_id": "72fabca989b3102c28482c60070aac5b",
            "vmbox_id": "b2f1c691e3e733f52c7c8b4b621289bc"
        }
    },
    {
        "type": "periodic",
        "seconds": 3,
        "minutes": 30,
        "hours": 2,
        "days": 1,
        "action": {
            "type": "check_voicemail",
            "account_id": "72fabca989b3102c28482c60070aac5b",
            "vmbox_id": "b2f1c691e3e733f52c7c8b4b621289bc"
        }
    }
```
