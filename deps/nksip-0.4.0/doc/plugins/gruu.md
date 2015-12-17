# Globally Routable User Agent URIs (GRUUs) Plugin

* [Name](#name)
* [Description](#description)
* [Dependant Plugins](#dependant-plugins)
* [Configuration Values](#configuration-values)
* [API Functions](#api-functions)
* [Callback Functions](#callback-functions)
* [Examples](#examples)


## Name
### `nksip_gruu`


## Description

This plugin provides full support for Globally Routable User Agent URIs (GRUUs), according to RFC5628. It modifies the [nksip_registrar](registrar.md) plugin to support GRUUS.

When activating this plugin, "gruu" is added the all automatically generated _Supported_ headers. When the registrar receives a registration request with a `+sip.instance` option, and the client supports GRUU, it will generate a _public_ and a _temporary_ GRUUs.

The client supporting this plugin will detect this and store them for later user, calling [get_gruu_pub/1](#get_gruu_pub1) and [get_gruu_temp/1](#get_gruu_temp1).

When implementing a registrar, call [registrar_find/2](#registrar_find2) instead of [nksip_registrar:find/2,4](nksip_registrar.md#find2) to decode the generated GRUUs.


## Dependant Plugins

None, but if [nksip_registrar](registrar.md) is activated it will be modified to support GRUUs. Without nksip_registrar, can be used as a client.


## Configuration Values

### SipApp configuration values

None

## API functions

### get_gruu_pub/1

```erlang
get_gruu_pub(nksip:app_name()|nksip:app_id()) ->
    {ok, nksip:uri()} | undefined | {error, term()}.
```
Gets the last detected public GRUU

### get_gruu_temp/1
```erlang
get_gruu_temp(nksip:app_name()|nksip:app_id()) ->
    {ok, nksip:uri()} | undefined | {error, term()}.
```

Gets the last detected temporary GRUU


### registrar_find/2

```
registrar_find(nksip:app_name()|nksip:app_id(), nksip:uri()) ->
    [nksip:uri()].
```

Use this function instead of [nksip_registrar:find/2,4](nksip_registrar.md#find2) to decode the generated GRUUs.




## Callback functions

None


## Examples

See [gruu_test.erl](../../test/gruu_test.erl) for examples
