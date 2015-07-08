# Logging Options

NkSIP uses [Lager](https://github.com/basho/lager) for logging, supporting multiple log levels, log rotation, etc. The following `Lager` levels are used:

Level|Typical use
---|---
`debug`|Maximum level of information. Do not use it in production
`info`|Detailed information. Not recommended in production
`notice`|Important information. Recommended in production
`warning`|Actions you should take into account
`error`|Important internal errors
`critical`|Not used currently
`alert`|Not used currently
`emergency`|Not used currently

You can configure Lager using its erlang environment variables, or using an erlang start up configuration file (usually called `app.config`). See the `samples` directory for an example of use.

Lager supports several backends, typically console and disk. You can change the current _log level_ with `lager:set_loglevel/2,3` (for example `lager:set_loglevel(lager_console_backend, debug)` and `lager:set_loglevel(lager_file_backend, "console.log", debug)`).

In order for a SipApp to change its log level, not only the global log level must be changed, but also the [application configuration](configuration.md), using the `log_level` option. SipApp config options can be changed on the fly.

To get SIP message tracing, activate the [nksip_trace](../plugins/trace.md) plugin.
