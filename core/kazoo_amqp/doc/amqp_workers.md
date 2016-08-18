/*
Section: Kazoo AMQP
Title: AMQP Workers
Language: en-US
Version: 3.22
*/

AMQP workers provide an abstraction over the publishing/consuming of messages from the AMQP bus. Most applications have two primary needs when dealing with AMQP:

1. Publish a message
2. Publish a request and receive zero or more responses

AMQP workers take care of both so that the app doesn't need to manage Channels, Queues, Exchange setup, etc.

## Publishing a message

To publish a message, the calling process needs only supply the Request and the publishing function to use (arity 1). A generic call would look like:

```erlang
Req = [{<<"Key">>, <<"value">>}
      ,{<<"Important">>, true}
       | kz_api:default_headers(?APP_NAME, ?APP_VERSION)
      ],
kz_amqp_worker:cast(Req, fun kapi_module:publish_message/1).
```

`Req` can be a proplist or JSON object.

The `kz_amqp_worker:cast/2` returns either 'ok' or an error tuple:

* `{'error', 'flow_control'}`: flow control is active and the worker is waiting on the broker to allow new messages
* `{'error', 'not_ready'}`: if the worker doesn't have all its initialization done with the broker yet. Should only see this while the worker is starting up or recovering from a down broker.
* `{'error', Error}`: if the publishing function fails, the captured error will be in `Error`
* `{'error', Other}`: if the publishing function returns something other than 'ok', that return will be in `Other`
* `{'error', 'badarg'}`, `{'error', 'function_clause'}`: If the publishing function crashes, these are two of the common reasons and are returned explicitly
* `{'error', Reason}`: `Reason` is the Class:Reason in the catch clause

Check the logs for stacktraces on the crashes.

## Collecting Responses

Often applications would like to query the cluster and get back responses, either the first valid response, or a collection of valid responses. `kz_amqp_worker` provides a couple variants to help the developer collect in various ways.

### `call/3`, `call/4`

These are the basic request/response calls. An example invocation might look like:

```erlang
kz_amqp_worker:call(Req, fun kapi_module:publish_req/1, fun kapi_module:resp_v/1).
```

This will publish the `Req` and wait until a message is received that causes the `VFun` to return `true`. `call/3` will return a timeout (use `call/4` to set the timeout) if no valid response is received.

Errors returned will be similar to `cast/2`.

#### Deferred Responses

As a process publishing responses, you can add a flag `Defer-Response:true` to let the AMQP worker know that this response is to be deferred from returning to the receiving process until the timeout period has expired. So, if the timeout is 2 seconds, and the deferred response is received immediately, the AMQP worker will continue receiving responses while tucking the deferred response away for later. If a non-deferred, valid response is received, that is returned to the receiving process; if the timeout expires, the deferred response is used as the result.

### `call_collect/2`, `call_collect/3`, `call_collect/4`

Suppose you want to aggregate responses from many apps or from many instances of a particular app. The `call_collect` family is here to help!

The arity/2 invocation instructs the AMQP worker to collect valid responses until the default timeout period has expired:

```erlang
kz_amqp_worker:call_collect(Req, fun kapi_module:publish_req/1).
```

Typically, we want responses from an app inparticular and to not wait for more responses that aren't coming.
This reduces the amount of time the calling process is blocked. Let's look at some more `call_collect` variants:

* `call_collect/3`: The third parameter can be either the timeout to wait (in essence `call_collect/2` with custom timeout **OR** it can be a `collect_until()` option, described below.
* `call_collect/4`: This differentiates the `call_collect/3`, allowing you to set both the `collect_until()` and a custom timeout.

#### `collect_until()`

There are five ways to terminate collection more quickly than a simple timeout:

##### Supply a user-defined function (`collect_until_fun`):

You can supply an arity/1 function:

```erlang
UntilFun = fun(CollectedJObj) -> custom_code(CollectedJObjs) end,
kz_amqp_worker:call_collect(Req, fun kapi_module:publish_req/1, UntilFun).
```

The `UntilFun` returns a `boolean()`: `true` meaning stop collecting and return to the caller; `false` means keep collecting.

You can also supply an arity/2 function and an accumulator argument thusly:

```erlang
InitAcc = [],
UntilFun = fun(CollectedJObj, Acc) -> custom_code(CollectedJObjs, Acc) end,
kz_amqp_worker:call_collect(Req, fun kapi_module:publish_req/1, {UntilFun, InitAcc}).
```

The `UntilFun` should return either a `boolean()` or `{'false', Acc1}` to continue with a new accumulator.

##### Supply the kapp name

You can choose a kapp name of the likely responding application. The kapp name will be used to count how many running instances are in the local zone (basically, which are connected to the same broker). For example:

```erlang
Kapp = 'ecallmgr',
kz_amqp_worker:call_collect(Req, fun kapi_module:publish_req/1, Kapp).
```

This will look at how many ecallmgr instances are connected to the broker and collect responses until that count is reached or a timeout occurs.

##### Get count from federated instances too

Say you have two or more zones and you'd like to make sure you get presence information from all the omnipresence apps running across your zones. Just supplying `omnipresence` in the third argument will only get the local zone's count. If you pass in `{Kapp, 'true'}`, `kz_amqp_worker` will count running instances across the zones. So if you have one in each zone, with two zones, `kz_amqp_worker` will wait for 2 responses (or timeout). For example:

```erlang
Kapp = 'ecallmgr',
kz_amqp_worker:call_collect(Req, fun kapi_module:publish_req/1, {Kapp, 'true'}).
```

##### Filter responses

You can supply a validation function as in the `call/3` case to make sure responses are what you're looking for:

```erlang
Kapp = 'ecallmgr',
kz_amqp_worker:call_collect(Req, fun kapi_module:publish_req/1, {Kapp, fun kapi_module:resp_v/1}).
```

Now responses will only count if `VFun` return `true`. Without a `VFun`, any response is collected.

##### Get federated count and validation

If you want to both get responses from across zones **and** validate those responses before collecting them:

```erlang
Kapp = 'ecallmgr',
kz_amqp_worker:call_collect(Req, fun kapi_module:publish_req/1, {Kapp, fun kapi_module:resp_v/1, 'true'}).
```

#### Get shared counts

Some apps, within a zone, will have identical information, or will receive requests on a shared queue (which means even though there are N instances running in that zone, the request payload is only delivered to one instance). This means if there are 2 instances in each of 2 zones (4 instances) but only one instance in each zone will receive the request, `kz_amqp_worker` will be waiting for 4 responses before returning. This means it will wait until the timeout occurs.

If you only expect a single response per zone, regardless of how many instances are running in each zone:

```erlang
Kapp = 'ecallmgr',
IncludeFederated = 'true',
IsShared = 'true',
kz_amqp_worker:call_collect(Req, fun kapi_module:publish_req/1, {Kapp, IncludeFederated, IsShared}).
```

If you set `IncludeFederated` to `false` in the above code, the count will be 1 (since only the local zone will be considered).

##### All the options!

Finally, if you want every option, we've got you covered:

```erlang
Kapp = 'ecallmgr',
VFun = fun kapi_message:resp_v/1,
IncludeFederated = 'true',
IsShared = 'true',
kz_amqp_worker:call_collect(Req, fun kapi_module:publish_req/1, {Kapp, VFun, IncludeFederated, IsShared}).
```

## Custom calls

If you need to make a call, but expect the response on an exchange other than 'targeted', you'll need to tell the worker to add the binding to its queue:

```erlang
Kapi = 'kapi_custom',
BindOptions = [],
kz_amqp_worker:call_collect(Req, fun kapi_module:publish_req/1, fun kapi_custom:resp_v/1, {Kapi, BindOptions}).
```

The `BindOptions` are the same options you would use when initializing a `gen_listener`.
