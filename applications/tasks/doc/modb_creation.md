# MODB Creation Task

Create account MODBs ahead of time.

## Metered creation

On the configured day of the month (the 28th by default), the task will calculate the seconds left until the start of the next month. Based on that time range and the number of accounts, MODB creation will be spaced across that time period.

For instance:

```
% May 28 2019 = 63726220800
% June 1 2019 = 63726566400

DiffS = (63726566400 - 63726220800) = 345600s

Accounts = 1000

DelayPerAccount = (DiffS / Accounts) = 345600 / 1000 = 345s
```

An MODB will be created every 345s until the end of the month.

## Manually run

If you need to manually run the task, you can use SUP to accomplish it:

```
sup kt_modb_creation create_modbs
```

This will spawn a worker and return immediately with the PID of that worker.

## System Config

### Date of starting creation

System admins can select which day of the month to start MODB creation for the next month. Set `tasks.modb_creation` doc's `creation_day` to a number in `1..28` to ensure each month will trigger creation.

### Parallel Creation

System admins can speed this up by increasing the `tasks.modb_creation` doc's `create_in_parallel` (default is 1). If set to `10` for instance, the delay becomes `(DiffS / InParallel) / Accounts` or `34s` - 10 MODBs created every 34 seconds.
