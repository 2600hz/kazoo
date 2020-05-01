# SUP Over API

## Overview

The SUP API is meant to mirror command-line interactions with the SUP tool. It will only run on the local API server.

!!! note
    You must be `super_duper_admin` to access the SUP endpoint.

### Activation

To update the running Crossbar system with this endpoint, issue the following sup command:

```shell
sup crossbar_maintenance start_module cb_sup
```

If you want this endpoint to load by default, modify the crossbar doc in the `system_config` database, and add `cb_sup` to the `autoload_modules` list.

### URL mapping

Remember that SUP commands follow the format of:

```shell
sup module_maintenance function [arg1, arg2,...]
```

The Crossbar URL is similarly constructed:

```
/v2/sup/module/[function[/arg1/arg2/...]]
```

The important differences are:

* No need to specify the `_maintenance` portion of the module
* `function` is optional and defaults to status/0 if not supplied

### Examples

| Command line | Crossbar |
|--------------------------------------------|-----------------------------------------|
| `sup kazoo_maintenance syslog_level debug` | `curl /v2/sup/kazoo/syslog_level/debug` |

## Execute Maintenance Status Command

Shortcut to run `status` command if the maintenance command has `status` command.

> GET /v2/sup/{MODULE}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/sup/{MODULE}
```

## Execute A Command With No Arguments

> GET /v2/sup/{MODULE}/{FUNCTION}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/sup/{MODULE}/{FUNCTION}
```

## Execute A Command Arguments

> GET /v2/sup/{MODULE}/{FUNCTION}/{ARGS}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/sup/{MODULE}/{FUNCTION}/{ARGS}
```

