# Plugin architecture

### Overall picture

NkSIP features a sophisticated plugin system, that allows a developer to modify its behaviour without having to modify its core, while having nearly zero overhead.

As described in the user guide, each SipApp must provide a _callback module_, that can implement a number of functions that NkSIP will call depending on various events. In this page we will call them "application callbacks", since they are quite different from "plugin callbacks".

NkSIP defines a serie of [_plugin callbacks_](plugin_callbacks.md) that defined plugins can override to modify NkSIP standard behaviour or make new functionality available to all SipApps. They have the concept of _dependency_, so that, if a _pluginA_ implements callback _callback1_, and another plugin _pluginB_ also implements _callback1_:
* If _pluginA_ depends on _pluginB_, the _callback1_ version of _pluginB_ is the one that will be called first, and only if it returns `continue` or `{continue, list()}` the version of `pluginA` would be called.
* If none of them depend on the other, the order in which the callbacks are called is not defined.

Any plugin can have any number of dependant plugins. NkSIP will ensure that versions of plugin callbacks on higher order plugins are called first, calling the next only in case it returns `continue` or `{continue, NewArgs}`, and so on, until reaching the default NkSIP's implementation of the callback (defined in [nksip_callbacks.erl](../../src/nksip_callbacks.erl) if all defined callbacks decide to continue, or no plugin has implemented this callback).

Plugin callbacks must be implemented in a module called with the same name as the plugin plus _"_callbacks"_ (for example `nksip_registrar_callbacks.erl`).

This callback chain behaviour is implemented in Erlang and **compiled on the fly**, into the generated runtime application callback module (see [run time configuration](runtime_configuration.md)). This way, any call to any plugin callback function is blazing fast, exactly the same as if it were hard-coded from the beginning. Calls to plugin callbacks not implemented in any plugin go directly to the default NkSIP implementation.

Each SipApp can have **different set of plugins**, and it can **it be changed in real time** as any other SipApp configuration value.


### SipApp callbacks

The _application callbacks_ are in fact called from specific _plugin callbacks_ `nksip_callbacks:app_call/3` and `nksip_callbacks:app_method/2`, so your plugins can override this function to modify the calling to standard _application callbacks_.

However, plugins can define its own _application callbacks_, available to be implemented in SipApps' callback modules, at the same level of NkSIP standard [application callbacks](../reference/callback_functions.md). If a module with the same name of the plugin plus _"_sipapp"_ is defined (for example `nksip_registrar_sipapp`), it can offer any number of additional _application callbacks_. They will be called from the _plugin callback_ `nksip_callbacks:app_call/3` as any standard application callback.

Two different plugins _cannot_ implement the same _application callback_ twice, and they cannot reimplement any of the currently defined NkSIP standard application callbacks. 


### Plugin configuration


When a SipApp starts, [you can configure it](../guide/start_a_sipapp.md) to activate any number of the currently installed plugins. For each defined plugin NkSIP will perform a serie of actions (`nksip_registrar` is used as an example plugin): 

1. Find the module `nksip_registrar` that must be loaded.
1. Call `nksip_registrar:version()` to get the current version of this plugin. It can be any `string()` or `binary()`.
1. Call `nksip_registrar:deps()` to get the list of dependant modules. Its type is `[{Plugin::atom(), VersionRE::string()|binary()}]`. NkSIP will then include this plugin as if we had included it also in the application configuration, before the dependant plugin, and checking that the version is correct.
1. After the sorted list of plugins is complete, implemented callbacks on each of them are extracted as described above, and the corresponding Erlang code is generated.
1. Application callbacks are then extracted from the defined SipApp's callback module, plugins having a "_sipapp" module and NkSIP standard `nksip_sipapp.erl` module.
1. All of it is compiled _on the fly_ in the runtime application callbacks module (called with the same name as the _internal name_ of the SipApp) 

