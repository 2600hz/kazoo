
Kazoo configuration, for the most part, lives in the `system_config` database (with a few exceptions for BigCouch, RabbitMQ, and basic Kazoo settings).

The `sysconf` app provides an AMQP listener for requests to manipulate values from the `system_config` database without needing to interact with the database directly.

# AMQP API

## Get system config values

To fetch a value, publish a `get_req` to the `sysconf` AMQP exchange using "sysconf.get" as the routing key. The payload to publish:

* `Event-Category`: "sysconf"
* `Event-Name`: "get_req"
* `Category`: The name of the `system_config` document (eg "ecallmgr", "kapps_controller", etc)
* `Key`: The key of the value desired (eg "fs_nodes")
* `Default`: (optional) If no `Key` is found, return this value as the result (will also set `Default` in the config document
* `Node`: The node to select the config from ("default" or "node@host.com")
* `Server-ID`: The AMQP queue, bound to the `targeted` exchange, to send the reply.
* `Msg-ID`: Unique identifier for this request

### The Response

The response will be published to the `targeted` AMQP exchange, using the `Server-ID` as the routing key. The payload published will be:

* `Event-Category`: "sysconf"
* `Event-Name`: "get_resp"
* `Category`: The category retrieved
* `Key`: The key retrieved
* `Value`: The value retrieved
* `Msg-ID`: The request's `Msg-ID`

### Building ACLs

Most of the time, requests will simply read the value from the `system_config` document. ACL generation is a special case, as Kazoo needs to fetch data from the `sip_auth` database, where local resources (per-account carriers) and auth-by-ip devices live.

The `sysconf_acls` module implements a scatter-gather approach:

1. The requesting PID spawns three processes:
    1. Process the `sip_auth` database's auth-by-ip docs
    2. Process the `offnet` database's (or global resources) docs
    3. Process the `sip_auth` database's local resources (account carriers)
2. Fetches the ecallmgr ACL list from `system_config`
3. Receives 0 or more messages from the spawned processes (or their children processes) with additional ACLs to add
4. After all three child processes have finished (or the request times out), the collected ACLs are returned to the requestor.

Within the three sub processes, each will search for IPs and hostnames. If a hostname is encountered, attempts will be made to resolve the hostname into IP(s). These resolved hostnames and raw IPs will then create an ACL entry and communicate that back to the requesting PID (the Collector in the code).

## Set system config values

To set a value in a particular config doc, publish a `set_req` to the `sysconf` AMQP exchange using "sysconf.set" as the routing key. The payload to publish:

* `Event-Category`: "sysconf"
* `Event-Name`: "set_req"
* `Category`: The name of the `system_config` document (eg "ecallmgr", "kapps_controller", etc)
* `Key`: The key to set
* `Value`: The value to set
* `Node-Specific`: (optional) Set as the "default" config or as a node's specific config ('false' by default - will set in the "default" config)
* `Node`: If `Node-Specific` is "true", use this value as the top-level key into the `system_config`'s `Category` document.
* `Server-ID`: The AMQP queue, bound to the `targeted` exchange, to send the reply.

### The Response

The response will be published to the `targeted` AMQP exchange, using the `Server-ID` as the routing key. The payload published will be:

* `Event-Category`: "sysconf"
* `Event-Name`: "set_resp"
* `Category`: The category being updated
* `Key`: The key being updated
* `Value`: The value set
* `Msg-ID`: The request's `Msg-ID`

## Flushing values

Sometimes a change will be made to a document in `system_config` directly in the database. Because Kazoo caches the config documents when they're fetched, it may be necessary to flush a given `Category` doc from the Kazoo cache. Publish a `flush_req` to the `sysconf` AMQP exchange using "sysconf.flush" as the routing key. The payload to publish:

* `Event-Category`: "sysconf"
* `Event-Name`: "flush_req"
* `Category`: The name of the `system_config` document (eg "ecallmgr", "kapps_controller", etc)
* `Key`: (optional) Flush only this key's value from the cached doc
