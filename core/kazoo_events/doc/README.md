# Kazoo Events

## Hooks

Kazoo provides a mechanism, `kz_hooks`, for Kazoo applications to receive call events without needing to setup an AMQP queue and binding. This reduces AMQP usage by establishing a single AMQP queue and bindings and using internal VM message passing and function calling to distribute to the interested processes.

There are three ways to ask for call events from `kz_hooks`:

1. `kz_hooks:register` will register the calling PID to receive a copy of the message from the "unique" hooks AMQP queue (all VMs will receive the payloads)
2. `kz_hooks:register_rr` will register using the "named" AMQP queue which will effectively round-robin the payloads within the zone.
3. `kz_hooks:bind` will allow the caller to specify the `M:F/A` or a local function to be called on event reception from the "unique" AMQP queue.
