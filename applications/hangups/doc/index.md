/*
Section: Hangups
Title: Hangups
Language: en-US
*/

# Hangups

Abnormal hangup cause logging

### hangup causes to monitor

- WRONG_CALL_STATE
- NO_ROUTE_DESTINATION
- CALL_REJECT
- MANDATORY_IE_MISSING
- PROGRESS_TIMEOUT
- RECOVERY_ON_TIMER_EXPIRE



## Add monitors for the above hangup causes

    sup hangups_maintenance activate_monitors {{AccountId}} {{per-minute threshold}}


## Add a monitor for a specific hangup cause

    sup hangups_maintenance activate_monitor {{AccountId}} {{hangup cause}}


## Set threshold for a specific hangup cause

    sup hangups_maintenance set_monitor_threshold {{hangup cause}} {{per-minute threshold}}


## Set a particular threshold for a specific hangup cause

    sup hangups_maintenance set_monitor_threshold {{hangup cause}} {{threshold name}} {{value}}

Where {{threshold name}} is one of
* one
* five
* fifteen
* day

