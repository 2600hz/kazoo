# Kazoo Tasks

Run background jobs on Kazoo clusters.

Task inputs are CSV, JSON data or nothing at all & generate a CSV output.

## APIs

Kazoo Tasks has its own Crossbar module implementing a RESTful API over at [cb_tasks](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/doc/tasks.md).

There is also a maintenance module whose entry points are [documented here](./maintenance.md).

## Task discovery

The `tasks` application discovers the different kinds of tasks provided by Kazoo applications on a cluster using `tasks_bindings`.
A task has to bind to `tasks.Category.help`, `tasks.Category.help.Action` & `tasks.Category.Action`.

### A task's metadata

* `description` (`string()`): short sentence explaining the tasks input & outputs.
* `doc` (`string()`): multi-line paragraph explaining `description` further.
* `expected_content` (`"text/csv" | undefined`): MIME type of input data. Either CSV or no input data.
* `mandatory` (`[string()]`): list of CSV fields that must be defined in the input data. List can be empty.
* `optional` (`[string()]`): the rest of the CSV fields. Can be empty too.

Note: `mandatory` & `optional` can both be empty only if `expected_content` is `undefined`.
Such a task is a *"`noinput` task"*: a task that does not requires CSV input data.

## Writing tasks

Note: input rows are processed one by one, top to bottom and produce 0 or 1 rows of output.
For `noinput` tasks it is also possible to create more than one row of output.

Let's call `Module` the name of the module implementing an app's tasks and `TaskName` one of these tasks' name.

### `Module:output_header(TaskName)`

This function is called before attempting to run the task.
It should return a CSV header as allowed by the type `kz_csv:row()`.
If the call crashes, the header used is made of `mandatory`, `optional` & `"error"`.

### `Module:Verifier/1`

Before applying the task, verifiers are applied to each cell of the row.
`Verifier` is the CSV header name of the current cell.
The function take the cell as input and should return
* `true`: if the cell is valid input
* `false`: otherwise
If the call crashes, `false` is assumed.

### `Module:TaskName/2,3`

#### Called as `Module:TaskName(ExtraArgs, Iterator)`.

This means the scheduler determined this to be a `noinput` task.

The first argument `ExtraArgs` contains the following pairs:
* `account_id`: account id that created the task instance.
* `auth_account_id`: account id of the X-Auth-Token used when creating the task instance.

As second argument, the function takes one of:
* `init`: so that the function can return `{ok, Data}`. Nothing is written to output and `Data` will be passed to the function on next call as the 2nd argument.
* `Data`: the term that a previous call to the function returned. This way one can work with state in between iterations.

If the call crashes, the current input row plus an `"error"` column is written to output.csv.

#### Called as `Module:TaskName(ExtraArgs, Iterator, Args)`.

This call applies the task with the current row as a map in `Args`.

If an `optional` input value is not defined or empty, its value is `undefined`.

The same rules as above apply on the 1st & 2nd arguments.

### Return values

The function must return a valid instance of the type `kz_tasks:return()`:

* `stop`: ends the task & uploads the output CSV.
* `ok`: row is counted as successful, nothing is written in the error column.
* `ne_binary()`: the error to write in the error column.
* `kz_csv:row()`: the row to write (useful if `output_header(TaskName)` was implemented).
* `[kz_csv:row()]`: this is only supported for `noinput` tasks. Writes more than 1 row to output.
* `{ok, Data}`: nothing is written to output and `Data` will be passed to the function on next call.
* `{ToWrite, Data}`: where `ToWrite` is either a `kz_csv:row()` or `[kz_csv:row()]`. Writes them to output & will pass `Data` on next call.
* `{binary(), Data}`: writes the binary string to output & will pass `Data` on next call.
* `{Error, Data}`: attempts to write `Error` as an error to output & will pass `Data` on next call.


### Examples

Examples of both kinds of tasks can be found in

* [kt_numbers](https://github.com/2600hz/kazoo/blob/master/applications/tasks/src/modules/kt_numbers.erl)
* [kt_services](https://github.com/2600hz/kazoo/blob/master/applications/tasks/src/modules/kt_services.erl)

## Task statuses

Once a task has been added it can have one of the following statuses:

* `"pending"`: task created (input uploaded, if any) but has not been started yet.
* `"executing"`: task has been started & has not finished yet.
* `"success"`: task finished executing & no rows failed to process.
* `"failure"`: task finished executing & all rows failed to process.
* `"partial"`: task finished executing & some rows failed to process.
* `"internal_error"`: all other cases. Maybe the application crashed? Maybe disk is full? ...

Once a task completed (with either `"success"`, `"failure"` or `"partial"`), if the upload
of the output.csv file failed, you can find your task's output `/tmp/task_out.TaskId.csv`.

## Configuration

When executing a task, its failed & succeeded rows counts will be periodically updated at a configurable rate.
Set `tasks.send_progress_after_processed` to the rate you prefer (default: `1000`).
Be careful as a rate too low may corrupt a task's state.

After a task's function (`TaskName`) has been called, the worker will wait a configurable number of milliseconds before proceeding with the next row.
Set `tasks.wait_after_row_ms` to the pause you want the system to make in between writes to output (default: `500`).

## Headless Tasks

You can create tasks that run periodically (like cronjobs) or that operation on a subset of databases.

There are a number of triggers you can use in your module's `init/0`:

### Triggers

- Cron-like
    - Minutely
    - Hourly
    - Daily
- Database
    - Account DBs
    - Account MODBs
    - System DBs
    - Other DBs

You can also combine multiple triggers when binding your module

### `init/0`

A simple example:

```erlang
init() ->
    _ = tasks_bindings:bind(?TRIGGER_ALL_DBS, ?MODULE, 'handle_database').
```

Find the trigger macros in [the tasks header](https://github.com/2600hz/kazoo/blob/master/applications/tasks/src/tasks.hrl). This particular example will bind the module's `handle_database/1` to be run each time a database is processed by the [`kz_tasks_trigger`](https://github.com/2600hz/kazoo/blob/master/applications/tasks/src/kz_tasks_trigger.erl) process.

Cron triggers will need an arity-0 function to callback to; database triggers call an arity-1 function.

### Callback function

All modules bound to a particular trigger will run in serial, so be mindful of that. If your operation is quick, do it directly. If it has the potential to take a while, consider spawning the work so the other modules can get to their business too.
