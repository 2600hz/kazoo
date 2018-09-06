# Kazoo EDR

EDR is an extension of logging and events in Kazoo, making it easy to add new events, and consume
them both by both Kazoo and external applications.

This is the core component of the EDR ecosystem, and allow for sending and subscribing to EDR events
within kazoo applications. This is not to be confused with the edr application, which is used to
take these events and distribute them to applications outside of Kazoo (or across AMQP).

## edr.hrl

A header file containing records and types that are used throughout the EDR ecosystem. It
contains:
  - #edr_event{}:
    A record representing an event. This is passed around the EDR ecosystem. This aims to capture
    all the generic fields for an event. The body field contains all info useful for a specific
    event
  - #edr_binding{}:
    A record representing a binding to edr events. This allows apps using EDR to bind to quite
    specific events. Eventually this may be extended to allow filtering based on the body field.


## edr_bindings.erl

This module is used to bind to edr events of interest. The following functions are useful for
general use:
  - bind(Binding(s), CallbackModule, CallbackFunctionName [, Payload]):
    This functions the same as the kazoo_bindings bind function, except instead of taking a binding
    key, it will take a single, or list of edr_binding records. The callback function will be
    called with an edr_event record, and a payload if specified whenever an event matching the
    binding is emitted on the same node.
  - bindings_from_json(JObj(s)), bindings_to_json(Binding(s)):
    These functions are used to serialize to JSON / deserialize binding records from JSON


## kz_edr.erl

This module is used to emit edr events. The following functions are useful for general use:
  - event(AppName, AppVersion, Severity, Verbosity, Body [, AccountId])
    This constructs an edr_event record, which is then delivered to all the listeners who's bindings
    match the event.
