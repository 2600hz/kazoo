/*
Section: Kazoo Tasks
Title: Kazoo Tasks
Language: en-US
*/

# Kazoo Tasks

Run background jobs on Kazoo clusters.

Task inputs are CSV, JSON data or nothing at all & generate a CSV output.


## APIs

Kazoo Tasks has its own Crossbar module implementing a RESTful API over at [cb_tasks](https://github.com/2600hz/kazoo/blob/master/applications/crossbar/doc/tasks.md).

There is also a maintenance module whose entry points are [documented here](./maintenance.md).


## Task discovery

The `kazoo_tasks` application discovers the different kinds of tasks provided by Kazoo applications on a cluster by sending an `help_req` event.
An app can listen to the `help_req` event from `tasks` and then reply with the kinds of tasks it is offering.

A reply to `help_req` must have these JSON values:
* `Tasks-Category` (`nonempty_string()`): name of the category tasks will be listed under.
* `Tasks-Module` (`nonempty_string()`): name of module implementing the tasks callbacks.
* `Tasks` (`kz_json:object()`): to each task's name is associated its metadata.

Examples of such replies:
* [knm_tasks_listener](https://github.com/2600hz/kazoo/blob/master/core/kazoo_number_manager/src/knm_tasks_listener.erl) for [knm_tasks](https://github.com/2600hz/kazoo/blob/master/core/kazoo_number_manager/src/knm_tasks.erl)
* [kz_services_tasks_listener](https://github.com/2600hz/kazoo/blob/master/core/kazoo_services/src/kz_services_tasks_listener.erl) for [kz_services_tasks](https://github.com/2600hz/kazoo/blob/master/core/kazoo_services/src/kz_services_tasks.erl)


### A task's metadata

* `description` (`string()`): short sentence explaining the tasks input & outputs.
* `doc` (`string()`): multiline paragraph explaining `description` further.
* `expected_content` (`"text/csv" | undefined`): MIME type of input data. Either CSV or no input data.
* `mandatory` (`[string()]`): list of CSV fields that must be defined in the input data. List can be empty.
* `optional` (`[string()]`): the rest of the CSV fields. Can be empty too.

Note: `mandatory` & `optional` can both be empty only if `expected_content` is `undefined`.
Such a task is a *"`noinput` task"*: a task that does not requires CSV input data.


## Writing tasks

Note input rows are processed one by one, top to bottom and produce 0 or 1 row of output.
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

### `Module:TaskName/Arity`

This call applies the task with the current row as input.

Note the first argument proplist contains the following pairs:
* `auth_account_id`: account id that created the task instance.

If the call crashes, the current input row plus a crash reason column is written to output.csv.

#### Tasks with input data

`Arity` then equals the sum of the count of `mandatory` & `optional` input fields plus one.
Arguments of `TaskName/Arity` are first the +1 proplist then the input fields in order.
If an `optional` input value is not defined, its value is `undefined`.

The function must return a valid instance of the type `task_return()`:
* `ok`: row is counted as successful, nothin is written in the error column.
* `ne_binary()`: the error to write in the error column.
* `kz_csv:row()`: the row to write (usefull if `output_header(TaskName)` was implemented).
* `[kz_csv:row()]`: this is only supported for `noinput` tasks.


#### Tasks without input data

`Arity` is then equals to 2: the proplist & task iterator.

As second argument, the function takes one of:
* `init`: so that the function can return `{ok, Data}`. Nothing is written to output and `Data` will be passed to the function on next call.
* `Data`: the term that a previous call to the function returned. This way one can work with state in between iterations.

The function must return one of:
* `stop`: ends the task & uploads the output CSV.
* `{ok, Data}`: nothing is written to output and `Data` will be passed to the function on next call.
* `{ToWrite, Data}`: where `ToWrite` is either a `kz_csv:row()` or `[kz_csv:row()]`. Writes them to output & will pass `Data` on next call.
* `{binary(), Data}`: writes the binary string to output & will pass `Data` on next call.
* `{Error, Data}`: attempts to write `Error` as an error to output & will pass `Data` on next call.

Examples of both kinds of tasks can be found in
* [knm_tasks](https://github.com/2600hz/kazoo/blob/master/core/kazoo_number_manager/src/knm_tasks.erl)
* [kz_services_tasks](https://github.com/2600hz/kazoo/blob/master/core/kazoo_services/src/kz_services_tasks.erl)


## Task statuses

Once a task has been added it can have one of the following statuses:

* `"pending"`: task has not been started yet.
* `"executing"`: task has been started & has not finished yet.
* `"success"`: task finished executing & no rows failed to process.
* `"failure"`: task finished executing & all rows failed to process.
* `"partial"`: task finished executing & some rows failed to process.
* `"internal_error"`: all other cases.

Note: when executing a task, its failed & succeeded rows counts will be updated at a configurable rate.
Set `tasks.send_progress_after_processed` to the rate you prefer (default: `1000`).
Be careful as a rate too low may corrupt a task's state.

Note: after a task's function (`TaskName`) has been called, the worker will wait a configurable number of milliseconds.
Set `tasks.wait_after_processed` to the pause you want the system to make in between writes to output CSV (default: `500`).
