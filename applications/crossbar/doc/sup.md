

The SUP API is meant to mirror command-line interactions with the SUP tool. It will only run on the local API server.

You must be `super_duper_admin` to access the SUP endpoint.

#### Activation

To update the running Crossbar system with this endpoint, issue the following sup command:

    sup crossbar_maintenance start_module cb_sup

If you want this endpoint to load by default, modify the crossbar doc in the `system_config` database, and add `cb_sup` to the `autoload_modules` list.

#### URL mapping

Remember that SUP commands follow the format of:

    sup module_maintenace function [arg1, arg2,...]

The Crossbar URL is similarly constructed:

    /v1/sup/module/[function[/arg1/arg2/...]]

The important differences are:

* No need to specify the **_maintenance** portion of the module
* **function** is optional and defaults to status/0 if not supplied

##### Examples

| Command line | Crossbar |
|--------------------------------------------|-----------------------------------------|
| `sup kazoo_maintenance syslog_level debug` | `curl /v1/sup/kazoo/syslog_level/debug` |
