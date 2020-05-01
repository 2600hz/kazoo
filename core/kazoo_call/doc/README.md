# Kazoo Call

This application provides functionality related to tracking a call's metadata and executing call commands.

## kapps_call

Record containing information about a call. Generally populated from `route_req` and `route_win` AMQP payloads and updated by the winning application. It can be serialized and passed to other Kazoo applications for tranferring control of the call processing.

## kapps_call_command

Set of convenience functions for executing dialplan on a call. A thin wrapper around kapi_dialplan with argument defaults, as well as async and sync versions of some command (sync noted by the `b_` prefix for blocking).

## kapps_call_events

Manages a cache of call events for Kazoo applications to ask for call events. At the moment, on startup, it registers with `kz_hooks:bind` to send `CHANNEL_DESTROY` events to the `kapps_call_cache`. The module exposes a function for Kazoo applications to query if the `CHANNEL_DESTROY` has appeared - this is typically used when setting up call processing when the call may terminate while setup is still being completed (and the destroy event is missed).
