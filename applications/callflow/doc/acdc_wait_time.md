## Acdc Wait Time

### About Acdc Wait Time

Handles branching the callflow based on the current average wait time of a queue.
Branch keys are integer-convertible binaries (e.g. `<<"60">>`) that represent wait time thresholds in seconds. The default branch is represented by the `_` key. The branch with the highest threshold which the queue's current average wait time exceeds is the branch followed in the callflow's execution. If none are exceeded, the default branch is followed.

#### Schema

Validator for the acdc_wait_time callflow data object



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`id` | Queue ID | `string()` |   | `false`
`window` | Window over which average wait time is calculated (in seconds) | `integer()` |   | `false`
