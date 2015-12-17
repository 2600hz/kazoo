# Outbound Plugin

* [Name](#name)
* [Description](#description)
* [Dependant Plugins](#dependant-plugins)
* [Configuration Values](#configuration-values)
* [API Functions](#api-functions)
* [Callback Functions](#callback-functions)
* [Examples](#examples)


## Name
### `nksip_outbound`


## Description

This plugin provides full support for SIP Outbound extension, according to RFC5626.
When activating this plugin, "outbound" will be added to all generated _Supported_ headers. 


## Dependant Plugins

None, but if [nksip_registrar](registrar.md) is activated it will be modified to support outbound.


## Configuration Values

None


## API functions

None


## Callback functions

None


## Examples

See [outbound_test.erl](../../test/outbound_test.erl) for examples
