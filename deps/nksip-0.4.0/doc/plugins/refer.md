# REFER Plugin

* [Name](#name)
* [Description](#description)
* [Dependant Plugins](#dependant-plugins)
* [Configuration Values](#configuration-values)
* [API Functions](#api-functions)
* [Callback Functions](#callback-functions)
* [Examples](#examples)


## Name
### `nksip_refer`


## Description

When activating this plugin, and new REFER is received, if you don't implement [sip_refer/2](../reference/callback_functions.md#sip_refer2) callback function, NkSIP will process the request automatically:
* It will call [sip_refer/3](#sip_refer3) in your callback module, and you must decide to procceed with the REFER or not.
* If you reply `true`, a new INVITE is sent to the referred address, and NkSIP starts calling [sip_refer_update/3](#sip_refer_update3) in your callback module to make you know about the progress.
* All received _SUSBSCRIBE_ and _NOTIFY_ requests belonging to the REFER are replied with a _200 OK_ automatically


## Dependant Plugins

None


## Configuration Values

### SipApp configuration values

None


## API functions

### process/2

```erlang
process(nksip:request(), nksip:call()) ->
    nksip:sipreply().
```

Use this function to process an incoming REFER manually (for example if you implement [sip_refer/2](../reference/callback_functions.md#sip_refer2)).



## Callback functions

You can implement any of these callback functions in your SipApp callback module.


### sip_refer/3

```erlang
sip_refer(ReferTo::nksip:uri(), Req::nksip:request(), Call::nksip:call()) ->
        boolean().
```

Called when a REFER request arrives. Reply `true` to continue the processing of the plugin or `false` to reply with a _403 Forbidden_
    

### sip_refer_update/3

```erlang
sip_refer_update(SubsId, Status, Call) ->
	ok
	when SubsId :: nksip:handle(), 
		 Status :: init | active | {notify, binary()} | terminated,
		 Call :: nksip:call().
```

Called when a REFER event is received.


## Examples

See [refer_test.erl](../../test/refer_test.erl) for examples
