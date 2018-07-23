
# Blackhole *Real-time HTTP Websocket Events*

### Binding to events

The binding syntax is the same used throughout all Crossbar applications, since it is the same as AMQP's binding syntax.

Here are a non-exhaustive list of bindings provided per default callback module:
* `bh_call`:
    * `call.CHANNEL_CREATE.*`
    * `call.CHANNEL_ANSWER.*`
    * `call.CHANNEL_DESTROY.*`
    * `call.CHANNEL_BRIDGE.*`
* `bh_conference`:
    * `conference.event.*.*`
    * `conference.command.*`
* `bh_fax`:
    * `fax.status.*`

### Writing your own bindings

Blackhole [callback modules](https://github.com/2600hz/kazoo/tree/master/applications/blackhole/src/modules) provide bindings to Kazoo events.
If however you do not find a callback module that provides the bindings you are looking for, you can easily add your own!

1. Copy [bh_skel](https://github.com/2600hz/kazoo/blob/master/applications/blackhole/src/modules/bh_skel.erl) into `bh_mymodule.erl`
1. Make sure the module name is prefixed by `bh_`
1. Make sure that it resides in the `modules/` directory
1. Now to make it listen to the events you want:
    * Your Blackhole callback module has to export the following functions:
        1. `handle_event/2`
            * Receives an `EventJObj` from AMQP
            * Then, one can do some pre-processing of the data (validation, normalization, ...)
            * And conditionally forward it down the Websockets pipe, calling `fun blackhole_data_emitter:emit/3`
        1. `{add,rm}_amqp_binding/2`
            * These two functions bind/unbind the Websocket consumer to the corresponding Blackhole producer.
            * The bindings are added when the Blackhole app starts
            * And removed when it stops
