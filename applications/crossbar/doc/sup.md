

The SUP API is meant to mirror command-line interactions with the SUP tool. It will only run on the local API server.

You must be super\_duper\_admin to access the SUP endpoint.

#### Activation

To update the running Crossbar system with this endpoint, issue the following sup command:

    sup crossbar_maintenance start_module cb_sup

If you want this endpoint to load by default, modify the crossbar doc in the system\_config database, and add cb\_sup to the autoload\_modules list.

#### URL mapping

Remember that SUP commands follow the format of:

    sup module_maintenace function [arg1, arg2,...]

The Crossbar URL is similarly constructed:

    /v1/sup/module/[function[/arg1/arg2/...]]

The important differences are:

* No need to specify the *_maintenance* portion of the module
* *function* is optional and defaults to status/0 if not supplied

##### Examples

<table border=1>
  <thead>
    <tr><th>Command line</th><th>Crossbar</th></tr>
  </thead>
  <tbody>
    <tr>
      <td>`sup kazoo_maintenance syslog_level debug`</td>
      <td>`curl /v1/sup/kazoo/syslog_level/debug`</td>
    </tr>
  </tbody>
</table>
