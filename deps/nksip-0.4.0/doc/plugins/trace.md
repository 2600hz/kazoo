# SIP Trace Plugin

* [Name](#name)
* [Description](#description)
* [Dependant Plugins](#dependant-plugins)
* [Configuration Values](#configuration-values)
* [API Functions](#api-functions)
* [Callback Functions](#callback-functions)
* [Examples](#examples)


## Name
### `nksip_trace`


## Description

This plugin allows to trace sent and received SIP messages to console or disk. 
You can start tracing any specific SipApp, and for any IP or a specific set of IPs.

See the `nksip_trace` option [bellow](#configuration-values).



## Dependant Plugins

None


## Configuration Values

### SipApp configuration values

Option|Default|Description
---|---|---
nksip_trace|-|Configures tracing for this SipApp (see bellow)

You can use the following options for `nksip_trace`:
* `{nksip_trace, console}`: Tracing for all IPs will be sent to the console
* `{nksip_trace, File::string()}`: Tracing will be sent to this file
* `{nksip_trace, {console, IpSpec::list()}}`: Trace to console, but only for this IPs (see bellow)
* `{nksip_trace, {File::string(), IpSpec::list()}}`: Trace to file, but only for this IPs (see bellow)

IpSpec can be an IP (like `"10.0.0.1"`), list of IPs (like `["10.0.0.1", "10.0.0.2"]`) or a regular expression or list of regular expressions (like `["10.0.0.1", "^11.*"]`)

Tracing will be much faster if you don't use IP filtering.


## API functions

### get_all/0
```erlang
get_all() ->
    [{App::nksip:app_name(), File::console|binary(), IpList::all|[binary()]}].
```

Get all SipApps currently tracing messages.


### start/0
```erlang
start() -> 
    [{nksip:app_name(), ok|{error, term()}}].
```

Equivalent to `start(AppId, console, all)` for all started SipApps.


### start/1
```erlang
start(App::nksip:app_id()|nksip:app_name()) -> 
    ok | {error, term()}.
```

Equivalent to `start(AppId, console, all)` for a started SipApp.


### start/2
```erlang
start(App::nksip:app_id()|nksip:app_name(), File::file()) -> 
    ok | {error, term()}.
```

Equivalent to `start(AppId, File, all)` for a started SipApp.


### start/3
```erlang
start(nksip:app_id()|nksip:app_id(), file(), ip_list()) ->
    ok | {error, term()}.
```

Configures a SipApp to start tracing SIP messages. Under the hood, it calls `nksip:update/2` to modify the SipApp configuration and include the corresponding `nksip_trace` option.


### stop/0
```erlang
stop() -> 
    ok.
```

Stop all tracing processes, closing all open files.


### stop/1
```erlang
stop(App::nksip:app_id()|nksip:app_name()) ->
    ok | {error, term()}.
```

Stop tracing in a specific SipApp, closing trace file if it is opened.


### print/1
```erlang
print(Msg::nksip:request()|nksip:response()) ->
 ok.
```

Pretty-print a `nksip:request()` or `nksip:response()`.


### print/2
```erlang
print(Tag::string()|binary(), Msg::nksip:request()|nksip:response()) ->
    ok.
```

Pretty-print a `nksip:request()` or `nksip:response()` with a Tag.


## Examples

```erlang
nksip_trace:start("my_app", "/tmp/1.trace", ["10.0.0.1", "^11.*"]).
```

or using the SipApp configuration file:

```erlang
nksip:start("my_app", nksip_sipapp, [], [
				{plugins, [nksip_trace]},
				{nksip_trace, {"/tmp/1.trace", ["10.0.0.1", "^11.*"]}}]).
```

removing the trace:

```erlang
nksip:update("my_app", [{plugins, []}]).
```



