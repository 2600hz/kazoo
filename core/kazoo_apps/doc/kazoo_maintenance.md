## SUP-able functions

| Function                 | Arguments  | Description                                                            |
| --------                 | ---------  | -----------                                                            |
| `console_level/1`        | `(Level)`  | Set the console (Erlang VM's console) log level                        |
| `crash/0`                |            | Creates a debug dump file and halts the VM                             |
| `debug_dump/0`           |            | Created a debug dump file in /tmp/{NODE}_{TIMESTAMP}                   |
| `error_level/1`          | `(Level)`  | Set the error log file's log level                                     |
| `ets_info/0`             |            | Prints information about the system's ETS tables                       |
| `gc_all/0`               |            | Garbage collects all running processes (500ms pause between each proc) |
| `gc_pids/1`              | `(Ps)`     | Garbage collect a list of PIDs                                         |
| `gc_top_mem_consumers/0` |            | Garbage collect the top 10 memory consuming PIDs                       |
| `gc_top_mem_consumers/1` | `(N)`      | Garbage collect the top N memory consuming PIDs                        |
| `hotload/1`              | `(Module)` | Hotload a the corresponding {MODULE}.beam file                         |
| `hotload_app/1`          | `(App)`    | Hotload all listed modules in the {APP}.app                            |
| `mem_info/0`             |            | Prints information about memory usage in the VM                        |
| `syslog_level/1`         | `(Level)`  | Set the syslog log level                                               |
| `top_mem_consumers/0`    |            | Print the top 10 memory consuming processes                            |
| `top_mem_consumers/1`    | `(Len)`    | Print the top N memory consuming processes                             |
