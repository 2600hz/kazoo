# NkSIP Stats Plugin

* [Name](#name)
* [Description](#description)
* [Dependant Plugins](#dependant-plugins)
* [Configuration Values](#configuration-values)
* [API Functions](#api-functions)
* [Callback Functions](#callback-functions)
* [Examples](#examples)


## Name
### `nksip_stats`

## Description

This plugins implements a (yet) very simple statistics module.
Once activated, it will take note of the response time for each received request, and for each period (5 secs by default), compute minimum, maximum, average and standard deviation of response times.


## Dependant Plugins

None


## Configuration Values

### SipApp configuration values

None

### Global configuration values

You can use the global configuration value `nksip_stats_period` to change the statistics calculation period from the default of 5 seconds


## API functions

### info/0
```erlang
info() ->
    nksip:optslist().
```
Gets some statistics about current number of calls, dialogs, queues, etc.


### get_uas_avg/0
```erlang
get_uas_avg() ->
    {Min::integer(), Max::integer(), Avg::integer(), Std::integer()}.
```
Gets the call statistics for the current period.



## Callback functions

None

## Examples

```erlang
nksip:start("my_app", nksip_sipapp, [], [{plugins, [nksip_stats]}]).
```

and afterwards

```erlang
nksip_stats:get_uas_avg().
```
