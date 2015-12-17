# Run-time configurarion

NkSIP implements and advanced and very efficient run-time configuration system. As explained in [the reference guide](../reference/configuration.md), it has two types of configuration options:
* Global configuration options. They are defined as standard Erlang environment variables for nksip application. Any started SipApp can override most of them.
* SiApp configuration options. They are defined when starting the SipApp calling `nksip:start/4`.
* Call Config

### Global config

During NkSIP startup, the functions in module [nksip_config.erl](../../src/nksip.config.erl) read the global configuration values from the nksip application environment, defining defaults for missing values, and storing them in an ETS table, accesible using `nksip_config:get/1,2`.

It also generates and compiles _on the fly_ the module `nksip_config_cache`, copying commonly used values as functions in it, for example `nksip_config_cache:global_id/0`. The on-disk file `nksip_config_cache.erl` is only a mockup to make it easier to understand the process. Now any process can call any on this functions to get common config values without calling to access the ETS table.


### SipApp config

When a SipApp starts, it copies all configuration values from the global config, allowing it to change any of its values for this specific application. 

Then it generates and compiles on the fly a module called with the same name as the _internal name_ of the application. This module has a number of functions that also work as a cache configuration (like the previous `nksip_config_cache`), but specific for this SipApp. It has functions as `name/0` (for the _user name_ of the application), `config/0` for the full config, etc.

This module also includes all the functions in the _callback module_, and all the detected _plugin callbacks_.


### Calls config

Each time a new call process is started, NkSIP copies three values in the process registry: the current _log level_, the application and the _call-id_. These values are read-only and only used to speed the logging process.




