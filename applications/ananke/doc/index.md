/*
Section: Ananke
Title: Ananke
Language: en-US
*/

# Ananke *Callback features*

This application originate calls on various events

## *new_voicemail*

When this event is received the following fields are checked

** `notify_callback_number` in corresponding vmbox
** or `vm_notify_callback_number` in vmbox owner's document

to get a number.

Also the following fields are checked

** `notify_callback_disabled` in vmbox
** `vm_notify_callback_disabled` in vmbox owner's document

If number was determined and notifications are not disabled, the call is being originated. The second leg is voicemail check callflow number.
A-leg custom channel variable `Caller-ID-Number` is set being equal to `mailbox` parameter in corresponding document.

A call will be attempted `attempts` times every `interval` minutes. Call timeout can be adjasted by modifying `callTimeout` option (in seconds)

These parameters can be set in (first one that's set wins)

* `tries`
* * `notify_callback_attempts` in vmbox
* * `vm_notify_callback_attempts` in vmbox owner's document
* * `vm_notify_callback_attempts` in account's document
* * `vm_notify_callback_attempts` in ananke's system config (default 5)
* `interval`
* * `notify_callback_interval` in vmbox
* * `vm_notify_callback_interval` in vmbox owner's document
* * `vm_notify_callback_interval` in account's document
* * `vm_notify_callback_interval` in ananke's system config (default 5)
* `callTimeout`
* * `notify_callback_timeout` in vmbox
* * `vm_notify_callback_timeout` in vmbox owner's document
* * `vm_notify_callback_timeout` in account's document
* * `vm_notify_callback_timeout` in ananke's system config (default 20)

Also you can set `originartor_type` to define channel variable.

Note: for automatic  `mailbox` number detection `single_mailbox_login=true` must be set in voicemail callflow module and user should use only one voice mailbox.

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
If any is found, then application will do the same thing as in `new_voicemail` event.

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
