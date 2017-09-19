/*
Section: Kazoo Edr
Title: Kazoo Edr
Language: en-US
*/

# EDR

TLDR: See recipies at the bottom for ways to use EDR.

## Description

This app is responsible for getting EDR events (see kazoo_edr) out of Kazoo. It must be started per
kazoo node you wish to receive events from.

## Backends

The EDR app is essentially a collection of backends. Each backend will recieve EDR events matching
a binding, and send them somewhere else (usually out of Kazoo).

Backends are registered in system_config/edr.

### General registerd backend configuration options

#### name

This is a unique identifier for a configured backend. This name is used to refer to the backend in
edr_maintenance sup commands.

#### type

This determines what backend module is used this registered backend.
A type of `file` maps to the backend module `edr_be_file`.

#### options

This is a JSON object which contains configuration options specific to the backend module.

#### bindings

These describe the types of events that should be recieved by the backend.
A binding has the following fields:
- account_id: The account id to recieve events from. If not specified or * then events for all accounts will be recieved
- app_name: Which apps should we recieve events from? If not specified or * then events for all apps will be recieved
- exact_severity: If true, then only events with severity exactly matching the severity field will be recieved. If false then more severe events will also be recieved
- exact_verbosity: If true, then only events with verbosity exactly matching the severity field will be recieved. If false then less verbose events will also be recieved
- include_descendants: Should we recieve events relating to descendant accounts if an account is specified?
- severity: What severity are we interested in events for? Possible values are `ok`, `warning`, `critical`
- verbosity: What verbosity are we interested in events for? Possible values are `trace`, `debug`, `info`, `warn`, `error`, `fatal`

### Backend types

#### amqp (edr_be_amqp)

This module forwards each event to RabbitMQ for use across nodes. kapi_edr_amqp can be used to bind to these events.
The routing key used is the same as that which is used by edr_bindings for consistency `edr.{SEVERITY}.{VERBOSITY}.{ACCOUNT_ID}.{APP_NAME}`.

There are configuration options yet, however a configurable routing key would be useful in future.

#### file (edr_be_file)

This module outputs each recieved event to a line in a log file. Each line is formatted by the specified formatter.
The configuration options are as follows:
- path (required): The path to the file. Ensure file (or directory, if the file does not exist) has the correct permissions
- formatter: Configuration as to how to format each event. This defaults to json with the default options.

#### http (edr_be_http)

This module sends out a HTTP request for each EDR event it recieves.
The configuration options are as follows:
- url (required): The URL the request will be sent to
- method: The HTTP method the request should use. Valid options are `put`, `post`, and `patch`. This defaults to `post`.
- headers: A JSON object containing headers and their values
- async: Should the request be asynchronous? Currently only `false` is implemented.
- connect_timeout: Connection timeout in milliseconds
- timeout: Request timeout in milliseconds
- formatter: Configuration as to how to format each event. Defaults to json with the default options.

### Formatters

A formatter is responsible for taking an event and formatting it for a backend. This is not configurable for every backend.

#### json (edr_fmt_json)

The configuration options are as follows:
- include_metadata: If set to `true` the output will be the whole EDR event. If set to `false` the output will just be the body. Defaults to `true`.
- normalize: Should the metadata fields be normalized to snake case? Defaults to `true`
- pretty: Should the output be nicely formatted? Defaults to `false`

## Sup commands

- `sup edr_maintenance register_backend {NAME} [{TYPE}] [{JSON_OPTS}] [{JSON_BINDINGS}] [{ENABLED}]`:
    Register a backend in the system_config, and start it if it is enabled. It is probably easier to use the system_config API and run `sup edr_maintenance enable_backend {NAME}` if you wish to specify options or bindings.

- `sup edr_maintenance delete_backend {NAME}`:
    The opposite of `register_backend`. Stop the backend if it is running, and remove it from the the system_config.

- `sup edr_maintenance enable_backend {NAME}`
    Start the backend, and enable it if it isn't already enabled.

- `sup edr_maintenance disable_backend {NAME}`
    Stop the backend, and disable it if it isn't already disabled.

- `sup edr_maintenance registered_backends`
    Show the configured backends.

## Recipies

Practical uses of EDR. It would be great if people could submit PRs with their configurations for other people to use.

### Multiple logs with different verbosities

#### system_config/edr:
```json
{
   "backends": [
       {
           "name": "debug_log",
           "type": "file",
           "options": {
               "path": "/var/log/kazoo/edr_debug.log"
           },
           "bindings": [
               {
                   "account_id": "*",
                   "include_descendants": false,
                   "app_name": "*",
                   "severity": "ok",
                   "exact_severity": false,
                   "verbosity": "debug",
                   "exact_verbosity": false
               }
           ],
           "enabled": true
       },
       {
           "name": "info_log",
           "type": "file",
           "options": {
               "path": "/var/log/kazoo/edr_info.log"
           },
           "bindings": [
               {
                   "account_id": "*",
                   "include_descendants": false,
                   "app_name": "*",
                   "severity": "ok",
                   "exact_severity": false,
                   "verbosity": "info",
                   "exact_verbosity": false
               }
           ],
           "enabled": true
       }
   ]
}
```

### EDR over Blackhole

This involves forwarding EDR events specific to an account over AMQP, which are then pushed over websockets with Blackhole.
The binding key is `edr.{SEVERITY}.{VERBOSITY}.{APP_NAME}`. Only events where the account_id matches that associated with the auth token will be recieved.

Note: You can request events with a verbosity or severity greater than what edr_be_amqp is bound to, however you won't recieve any of them.

#### system_config/edr:
```json
{
   "backends": [
       {
           "name": "amqp",
           "type": "amqp",
           "options": {
           },
           "bindings": [
               {
                   "account_id": "*",
                   "include_descendants": false,
                   "app_name": "*",
                   "severity": "ok",
                   "exact_severity": false,
                   "verbosity": "info",
                   "exact_verbosity": false
               }
           ],
           "enabled": true
       }
    ]
}
```

#### Start the Blackhole module

`sup blackhole_maintenance start_module bh_edr`

### EDR events to HTTPBin

An example of sending EDR events over HTTP.

```json
{
   "backends": [
       {
           "name": "mock_bin",
           "type": "http",
           "options": {
               "url": "http://mockbin.org/bin/021cb9cb-b688-4f1e-9894-c80b4a19e654",
               "headers": {
                   "Content-Type": "application/json"
               }
           },
           "bindings": [
               {
                   "account_id": "*",
                   "include_descendants": false,
                   "app_name": "*",
                   "severity": "ok",
                   "exact_severity": false,
                   "verbosity": "info",
                   "exact_verbosity": false
               }
           ],
           "enabled": true
       }
    ]
}
```
