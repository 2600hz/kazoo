### Maintenance


#### List available tasks

```shell
{SUP} tasks_maintenance help
```

```shell
{SUP} tasks_maintenance help {CATEGORY}
```

```shell
{SUP} tasks_maintenance help {CATEGORY} {ACTION}
```


#### List all tasks

```shell
{SUP} tasks_maintenance tasks
```

```shell
{SUP} tasks_maintenance tasks {ACCOUNT_ID}
```


#### Add a new task

Note: set timeout to 300 seconds or more when your `{CSV_FILE}` fails to upload.

```shell
{SUP} -t 300 tasks_maintenance add {AUTH_ACCOUNT_ID} {ACCOUNT_ID} {CATEGORY} {ACTION} {CSV_FILE}
```

Or, for a task that does not require an input file:

```shell
{SUP} -t 300 tasks_maintenance add {AUTH_ACCOUNT_ID} {ACCOUNT_ID} {CATEGORY} {ACTION}
```


#### Remove a completed task

```shell
{SUP} tasks_maintenance remove {TASK_ID}
```


#### Get a specific task's details

```shell
{SUP} tasks_maintenance task {TASK_ID}
```

Note: set timeout to 300 seconds or more when the CSV file fails to download.

```shell
{SUP} -t 300 tasks_maintenance task_input {TASK_ID}
```


#### Start a task

```shell
{SUP} tasks_maintenance start {TASK_ID}
```


#### Retrieve a task's output CSV

Note: set timeout to 300 seconds or more when the errors listing fails to download.

```shell
{SUP} -t 300 tasks_maintenance task_output {TASK_ID}
```
