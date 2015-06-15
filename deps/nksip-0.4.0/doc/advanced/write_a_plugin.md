# Plugin writer guide

Please have a look at [runtime configuration](runtime_configuration.md) and [plugin architecture](plugin_architecture.md) before reading this guide.


* [Plugins or SipApps](#plugins-or-sipapps)
* [Plugin main module description](#plugin-main-module-description)
* [Plugin callbacks module description](#plugin-callbacks-module-description)
* [Application callbacks module description](#application-callbacks-module-description)
* [How to write a plugin](#how-to-write-a-plugin)


## Plugins or SipApps

Sometimes the doubt about writing a specific functionality as a SipApp or a plugin may arise. As a rule of thumb, plugins should implement functionality useful to a broad range of SipApps.

Examples of plugins could be:
* New RFCs implementation.
* Event packages.
* New authentication algorithms.
* Connection to standard external databases or services.

Some differences of SipApps and plugins are:

Concept|SipApp Callbacks|Plugin Callbacks
---|---|---
Language|Erlang currently (in the future other languages will be available)|Erlang only
Diffculty|Low|High (must understand how NkSIP works)
Flexibility|Medium|High (full access to NkSIP structures)
Dependency on NkSIP version|Low|High
Speed|Very High (if Erlang)|Very High



## Plugin main module description

### Mandatory functions
A plugin must have a _main_ Erlang module, that **must implement** the following functions:

#### version/0
```erlang
version() ->
    string().
```

Must return the version of the plugin.


#### deps/0
```erlang
deps() ->
    [{atom(), string()}].
```

Must return the list of dependant plugins. Its type is `[{Plugin::atom(), VersionRE::string()|binary()}]`. NkSIP will find the requested plugins and make sure their version is correct. `VersionRE` is a regular expression that will be applied againts found dependant plugins.


### Optional functions
There are other **optional** functions that your plugin main module can implement:


#### parse_config/1
```erlang
parse_config(nksip:optslist()) ->
    {ok, nksip:optslist()} | {error, term()}.
```

When the SipApp activating this plugin starts, the list of configuration options is parsed. If implemented, this function is called after parsing the core options. The plugin can add new options or modify existing ones.

It is also called when the SipApp is reconfigured.


#### init/2
```erlang
init(nksip:app_id(), nksip_sipapp_srv:state()) ->
    {ok, nksip_siapp_srv:state()}.
```

When the gen_server supporting the SipApp starts (or the plugin is activated after a reconfiguration), this function is called from it. The plugin can read and store information in the gen_server's state, using the funcions `nksip_sipapp_srv:get_meta/2` and `nksip_siapp_srv:set_meta/3` (the [nksip_uac_auto_register](../../plugins/src/nksip_uac_auto_register.erl) plugin is an example of using this functions).


#### terminate/2
```erlang
terminate(nksip:app_id(), nksip_sipapp_srv:state()) ->
   {ok, nksip_sipapp_srv:state()}.
```

When the SipApp stops (or the plugin is deactivated because of a reconfiguration) this function is called. The plugin must clean any stored state.


## Plugin callbacks module description

Then plugin can also include an Erlang module called after the main module, but ending in `_callbacks` (for example `nksip_registrar_callbacks`). 

In this case, any function exported in this module is considered as a _plugin callback_ by NkSIP:

* If NkSIP has a _plugin callback_ with the same name and arity, this function is included in the plugin chain as described in [plugin architecture](plugin_architecture.md).
* If there is no _plugin callback_ with the same name and arity, it is a new _plugin callback_ that other plugins can overload.

The [nksip_registrar](../plugins/registrar.md) plugin is an example of plugin that exports _plugin callback_ functions for other plugins to overload.


## Application callbacks module description

Then plugin can also include an Erlang module called after the main module, but ending in `_sipapp` (for example `nksip_uac_auto_register_sipapp`). 

In this case, any exported function in this module is considered as an _application callback_, that any SipApp activating this plugin can implement in its callback module.

# How to write a plugin

To write a new plugin:

1. Create a module with the name of your plugin (for example `my_plugin.erl`).
1. Define the function `my_plugin:version()` returning the current version of it (as a `string()` or `binary()`). Other plugins may request a dependency on a specific version of the plugin.
1. Define the function `my_plugin:deps()` returning a list of dependant plugins and required versions.
1. Put in this module any other public API function you want to make available to SipApps. 
1. Create a module with the same name as the main module but ending in "_callbacks" (for example `my_plugin_callbacks.erl`), and implement in it the [plugin callbacks functions](plugin_callbacks.md) you want to override. Each plugin callback can return:
	* `continue`: continue calling the next plugin callback, or the default NkSIP implementation if no other is available.
	* `{continue, Args::list()}`: continue calling the next plugin, but changing the calling parameters on the way.
	* `AnyOther`: finish the callback chain, return this value to NkSIP. The effect depends on each specific callback (see [_plugin callbacks_](plugin_callbacks.md)).
1. If your plugin is going to offer new application-level callbacks to SipApps using this module, place them in a module with the same name as the main module but ending in "_sipapp" (for example `my_plugin_sipapp.erl`). Please notice that NkSIP will not allow two different pluigns implement the same application callback module, or reimplement a [standard NkSIP application callback](../reference/callback_functions.md). For this reason it is recomended that they are named after the name of the plugin (i.e. "my_plugin_callback1").
1. SipApp can now request to use your plugin using the `plugins` configuration option.

If it is neccessary, implement the optional functions [parse_config/1](#parse_config1), [init/2](#init2) and/or [terminate/2](#terminate2).

NkSIP's official plugins are found in the `plugins` directory of the distribution. You can place new plugins wherever you want, as long as they are compiled and available in the Erlang code path. They can be standard Erlang applications or not, whatever makes more sense for each specific plugin.



### Contribute a plugin

If you think your plugin is interesting for many users, and it meets NkSIP code quality standards, please send a pull request to be included as a standard plugin.
