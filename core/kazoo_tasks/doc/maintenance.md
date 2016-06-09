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
{SUP} -t 300 tasks_maintenance add {ACCOUNT_ID} {CATEGORY} {ACTION} {CSV_FILE}
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
{SUP} -t 300 tasks_maintenance task_csv {TASK_ID}
```


#### Start a task

```shell
{SUP} tasks_maintenance start {TASK_ID}
```


#### Retrieve an errors listing

Note: set timeout to 300 seconds or more when the errors listing fails to download.

```shell
{SUP} -t 300 tasks_maintenance task_errors {TASK_ID}
```
