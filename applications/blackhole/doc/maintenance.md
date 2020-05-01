## SUP-able functions

| Function | Arguments | Description |
| -------- | --------- | ----------- |
| `active_sessions/0` |  | List active connections |
| `active_sessions_by_account/1` | `(AccountId)` | List active connections for a given account ID |
| `active_sessions_by_ip/1` | `(IPAddr)` | List active connections for a given IP address |
| `running_modules/0` |  | List blackhole modules running on the server |
| `start_module/1` | `(ModuleBin)` | start blackhole module |
| `start_module/2` | `(ModuleBin,Persist)` | start (and toggle persistence) blackhole module |
| `stop_module/1` | `(ModuleBin)` | stop blackhole module |
| `stop_module/2` | `(ModuleBin,Persist)` | stop (and toggle persistence) blackhole module |
