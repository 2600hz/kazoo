## Global registry

When you need to register a process that is unique across the cluster. Globals will handle finding the pid(), either local on the VM or a pid() that will proxy the request to AMQP.

### Starting a new process

```erlang
gen_server:start_link({'via', 'kz_globals', YourName}, Module, Function, Args).
```

Note that `YourName` can be any Erlang term! Now, you can use `gen_server:call({'via', 'kz_globals', YourName}, Payload)` to send `YourName`

### Globals API

* `whereis_name(Name)`: returns the pid() or 'undefined' registered to Name
* `register_name(Name, Pid)`: registers Pid under the name Name.
* `unregister_name(Name)`: unregisters the pid() from Name
* `send(Name, Msg)`: sends the pid() registered as Name a Msg.

## How kz_globals works

### Registering a name

When client code calls (directly or using `{'via', 'kz_globals', Name}`) `register_name/2`, kz_globals will first attempt to locate a pid() already registered:

1. Check the local ETS table for an existing registration
2. With no local registration found, kz_globals will issue an AMQP request to register Name with `State="pending"`.
3. Once responses are collected, check each response's `State` for a non-pending value.
  a. If only `pending` states are found, register the name locally in ETS and advertise the registration to AMQP
  b. If a State is non-pending, fail the registration back to the client

#### On the remote side(s)

1. Receive an AMQP `register` payload with `State="pending"`.
2. Check the local ETS table for the Name
  a. If not found, reply back to request with `State="none"`
  b. If found:
      1. Reply back to request with the value of `state` in the ETS entry (`none`, `local`, `remote`)
      2. Advertise the registration so all remote nodes can update their local ETS (if needed)

When receiving the advertised registration:

1. Receive an AMQP `register` payload with `State="local"`.
2. Create the `#kz_global{}` record to point to the remote node
3. Start a proxy pid() to represent the remote side
4. Insert the updated `#kz_global{}` record into the local ETS table

## Testing

There is a quickcheck model that will test the operation of the registry at `core/kazoo_globals/test/kz_globals_pqc.erl`.

Compile the core lib with `make proper`
```bash
$ cd path/to/kazoo/core/kz_globals
$ make proper
```

and in your VM shell, issue:
```erlang
(pdx@pdx.2600hz.com)1> code:add_path("/path/to/kazoo/core/kazoo_globals/test").
(pdx@pdx.2600hz.com)2> code:load_file(kz_globals_pqc).
(pdx@pdx.2600hz.com)3> proper:quickcheck(kz_globals_pqc:correct()).
....................................................................................................
OK: Passed 100 test(s).

27% {kz_globals,unregister_name,1}
25% {kz_globals,registered,0}
24% {kz_globals,register_name,2}
22% {kz_globals,whereis_name,1}
true
```

You can call `kz_globals_pqc:correct_parallel()` to run tests in parallel against the kz_globals server.

Do note that you must have a running VM, but these tests are really only designed to run on a single node (non-clustered/federated). The test suite will simulate remote nodes.
