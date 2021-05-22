### Troubleshooting

#### Queue Manager Diagnostics

The `acdc_queue_manager_diag` and `acdc_queue_manager_diag_sup` modules provide a real-time play-by-play of strategy state changes for a queue manager. This tool enables more reliable diagnosis of issues related to unexpected strategy states (e.g. agents in incorrect ringing/busy/available/unavailable states).

To start diagnostics for a queue, open a remote shell to the Kazoo node using `kazoo-applications connect`. On the remote shell, run

```
acdc_maintenance:start_queue_diagnostics(AccountId, QueueId).
```

replacing `AccountId` and `QueueId` with the binary representations of those values. For example: `acdc_maintenance:start_queue_diagnostics(<<"3d24e4ba001a096df9d925e1c2dda09b">>, <<"854ebe1f6871cff2e292cfcab6bfbff6">>).`

You will see output like:

```
|  Timestamp  | Message                                                        |
|-------------|----------------------------------------------------------------|
This diagnostic process is expensive. Remember to stop diagnostics when you are done!
To stop: `acdc_maintenance:stop_queue_diagnostics(list_to_pid("<0.15215.0>")).`
ok
```

As mentioned in the output, you can run `acdc_maintenance:stop_queue_diagnostics` to stop the diagnostics stream at any point. The PID in the output will match the PID for your particular diagnostics stream so you don't have to guess!

A stream of events like the following will be displayed as agents change state, calls come/go, and calls are distributed to agents. The kinds of emitted events will vary depending on the selected strategy for the observed queue.

```
|-------------|----------------------------------------------------------------|
| 63790338474 | got next winner from [<<"37bc41c35f1738781cb63c57709b73d5">>]  |
|-------------|----------------------------------------------------------------|
| 63790338474 | agent 37bc41c35f1738781cb63c57709b73d5 updated in SS           |
|             |                                                                |
|             | ringing agents: [<<"37bc41c35f1738781cb63c57709b73d5">>]       |
|             |                                                                |
|             | busy agents: []                                                |
|             |                                                                |
|             | agent queue: []                                                |
|-------------|----------------------------------------------------------------|
```
