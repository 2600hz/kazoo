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
