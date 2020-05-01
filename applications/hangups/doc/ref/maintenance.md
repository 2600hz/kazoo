## SUP-able functions

| Function | Arguments | Description |
| -------- | --------- | ----------- |
| `account_summary/1` | `(AccountId)` | |
| `activate_monitor/2` | `(AccountId,HangupCause)` | |
| `activate_monitors/2` | `(AccountId,ThresholdOneMinute)` | |
| `hangup_summary/1` | `(HangupCause)` | |
| `hangup_summary/2` | `(HangupCause,AccountId)` | |
| `hangups_summary/0` |  | |
| `set_metric/2` | `(Metric,LoadAvg)` | |
| `set_metric/3` | `(AccountId,Metric,LoadAvg)` | |
| `set_monitor_threshold/2` | `(HangupCause,TOM)` | |
| `set_monitor_threshold/3` | `(HangupCause,ThresholdName,T)` | |
| `set_threshold/3` | `(HangupCause,Metric,LoadAvg)` | |
| `set_threshold/4` | `(AccountId,HangupCause,Metric,LoadAvg)` | |
