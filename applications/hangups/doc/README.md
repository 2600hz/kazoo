# Hangups

## About Hangups

Most of the time, calls end in a handful of expected ways that are considered 'normal'. Rarely seen hangup causes are typically not relevant in isolation but when spikes of the same abnormal hangup cause(s) occur, it can be beneficial to respond appropriately.

Just as most computers show you metrics related to CPU usage over time (one, five and fifteen minutes typically), the hangups app tracks hangup causes and gives you the tools to send alerts when thresholds are exceeded.

For instance, a common sign that an upstream carrier is having issues with their network is an increase in `PROGRESS_TIMEOUT` hangup causes when sending calls to the carrier. When the INVITE goes to the carrier, Kazoo waits a certain amount of time (8 seconds by default) to hear back a "progress" SIP message (180, 183, or 200 typically). If the carrier fails to respond in time, the leg is hung up with `PROGRESS_TIMEOUT` and the cause, and the next carrier (if any) is tried. If you set the threshold for `PROGRESS_TIMEOUT` to `0.5` for the `one` minute metric, you will receive alerts when the number of calls terminating with `PROGRESS_TIMEOUT` increase to the point that the threshold is tripped.

Other hangup causes can imply other failure scenarios worth investigating. The table below offers some suggestions for interpreting the hangup cause.

## Typical Abnormal Hangup Causes

Hangup State | Possible Causes |
------------ | --------------- |
`WRONG_CALL_STATE` | We see these sometimes when ACLs are out of sorts
`NO_ROUTE_DESTINATION` | This could mean there is no callflow defined for the number, or the number is unassigned to a PBX in PBX Connector. You'll have to figure out which side of the dialog is hanging up first though.
`CALL_REJECTED` | The side sending this hangup isn't going to route the call.
`MANDATORY_IE_MISSING` | This might be because the leg was challenged for authentication and was unable to comply. Another cause could be no codec was negotiated between the two sides. Check the SDP codec listings for both sides.
`PROGRESS_TIMEOUT` | The endpoint (carrier or device) failed to progress to early media, ringing, or answering the call within the allotted time. May be indicative of errors on the endpoint's side. If a carrier, consider removing them from the offnet/account routing until you can discover the issue. These hangups impact PDD (post-dial delay) and are quite noticeable to the caller.
``RECOVERY_ON_TIMER_EXPIRE` | Often seen when NAT is interfering with receiving responses from the endpoint. Check the firewall at the customer's site for SIP ALG (and turn it off), try port 7000 or TCP as necessary.

## Configuring Hangup Causes

In the `system_config` database there is a `hangups` document that includes a list of hangup causes that are ignored (deemed 'normal') and a list to explicitly monitor.

Update `ignore_hangup_causes` with additional hangup causes you'd like to *not* receive alerts about.

## Setting up monitors

### Setting the load average

Load averages help tell you when something is occurring regularly. But how to know what thresholds should cause alerting? There are many articles that talk about this. [One article](http://blog.scoutapp.com/articles/2009/07/31/understanding-load-averages) uses a bridge metaphor that can be helpful. The truth is you'll need to play with these as you go, since as your volume of calls increase, the thresholds at which you reach "hair is on fire" severity will change. In CPU load terms, 1.0 is high when you have a single CPU but is nothing when you have a 24-core server.

`{LOAD_AVG}` should be the number above which alerting should start (0.0 will disable alerting for that metric).

`{METRIC}` is one of:
    - `one`
    - `five`
    - `fifteen`
    - `day`

### Set thresholds for a hangup cause / metric

**Globally**
```shell
$> sup hangups_maintenance set_threshold {HANGUP_CAUSE} {METRIC} {LOAD_AVG}
set {METRIC} for hangups.{HANGUP_CAUSE} to {LOAD_AVG}
ok
```

**Per-Account**
```shell
$> sup hangups_maintenance set_threshold {ACCOUNT_ID} {HANGUP_CAUSE} {METRIC} {LOAD_AVG}
```

### Set metric thresholds

A good way to initialize your thresholds is to apply the same threshold across all tracked hangup causes.

**Globally**
```shell
$> sup hangups_maintenance set_metric {METRIC} {LOAD_AVG}
set {METRIC} for hangups.WRONG_CALL_STATE to {LOAD_AVG}
set {METRIC} for hangups.NO_ROUTE_DESTINATION to {LOAD_AVG}
set {METRIC} for hangups.CALL_REJECT to {LOAD_AVG}
set {METRIC} for hangups.MANDATORY_IE_MISSING to {LOAD_AVG}
set {METRIC} for hangups.PROGRESS_TIMEOUT to {LOAD_AVG}
set {METRIC} for hangups.RECOVERY_ON_TIMER_EXPIRE to {LOAD_AVG}
ok
```

**Per-Account**
```shell
$> sup hangups_maintenance set_metric {ACCOUNT_ID} {METRIC} {LOAD_AVG}
```

## Deprecated SUP commands

### Monitor an account's minute load average

```shell
sup hangups_maintenance activate_monitors {ACCOUNT_ID} {LOAD_AVG}
```

## Add a monitor for a specific hangup cause

    sup hangups_maintenance activate_monitor {ACCOUNT_ID} {HANGUP_CAUSE}


## Set threshold for a specific hangup cause

    sup hangups_maintenance set_monitor_threshold {HANGUP_CAUSE} {{per-minute threshold}}


## Set a particular threshold for a specific hangup cause

    sup hangups_maintenance set_monitor_threshold {HANGUP_CAUSE} {{threshold name}} {{value}}

Where {{threshold name}} is one of
* one
* five
* fifteen
* day
