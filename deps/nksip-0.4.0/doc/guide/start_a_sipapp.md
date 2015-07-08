# Starting a SipApp

* [Starting a SipApp](#starting-a-sipapp)
* [Implementation](#sipapp-implementation)
* [Saving State](#saving-state-information)
* [Reconfiguration](#reconfigure-the-sipapp)


## Starting a SipApp
A SipApp is a SIP entity that starts listening on one or several sets of transport (UDP, TCP, TLS, SCTP, WS or WSS currently), ip address and port. After starting it, you can [send any SIP request](sending_requests.md), and, when any other SIP entity [sends you a request](receiving_requests.md) NkSIP will notify you so that you can process it and [send an answer](sending_responses.md).

You must first create a _callback Erlang module_ (for very simple applications, you can use the default callback module included with NkSIP, [`nksip_sipapp`](../../src/nksip_sipapp.erl)). The callback module can implement any callback function from the list of NkSIP SipApp's [callback functions](../reference/callback_functions.md). You can take a look to the [`nksip_sipapp`](../../src/nksip_sipapp.erl) module to find the default implementation of each of them.

Once defined the callback module, call [`nksip:start/4`](../../src/nksip.erl) to start the SipApp:
```erlang
> nksip:start("my_app", nksip_sipapp, [], [{transports, [{udp, {127,0,0,1}, 5060}]}]).
{ok,ac0a6o5}
```

* You can use any Erlang term you want to name your SipApp, but NkSIP will generate a _internal name_ as an `atom()`, based on a hash over your application name (`ac0a6o5` in the example). In most NkSIP functions you can use any of them to refer to your SipApp, but the internal format is faster.
* The second argument should be the name of a valid Erlang module implementing the SipApp's _callback module_, or `nksip_sipapp` to use the default callback module.
* The third argument refers to the initial arguments to be sent to `init/1` callback function, and is described later in this page.
* The fourth argument refers to the list of options you want configure your application with, defined in the [reference page](../reference/configuration.md).

After starting your SipApp, you can send any request:
```erlang
> nksip_uac:options("my_app", "sip:sip2sip.info", []).
{ok,200,[]}
```

or 

```erlang
> nksip_uac:options(ac0a6o5, "sip:sip2sip.info", []).
{ok,200,[]}
```

If another entity sends you a SIP request to one of your listening addresses, NkSIP will call one or several functions in your _callback module_, if implemented. For example, for an _OPTIONS_ request it will call your [sip_options/2](../reference/callback_functions.md#sip_options2) callback function, among others.


## SipApp Implementation

The first time you start a SipApp, NkSIP will generate a new RFC4122 compatible _UUID_ (used for example in SIP registrations), and will save it to disk in a file with the same name of the internal application (see configuration options for the default directory).

Each started SipApp is under the hood a standard OTP _gen&#95;server_ process. If you define the [`init/1`](../reference/callback_functions.md#init1) callback function in your _callback module_, NkSIP will call it while starting the SipApp, using the third argument in the call to `nksip:start/4`. The returning value shall include the initial state value, associated with the gen_server process.

As a standard gen_server process, you can use functions like `gen_server:call/3` or `gen_server:cast/2`. The registered name of the process is the same atom as the internal application name. If you implement callback functions [`handle_call/3`](../reference/callback_functions.md#handle_call3), [`handle_cast/2`](../reference/callback_functions.md#handle_cast2) or [`handle_info/2`](../reference/callback_functions.md#handle_info2), they will called as in a standard gen_server behaviour (when someone calls `gen_server:call/2,3`, `gen_server:cast/2` or the process receives a message).

Should the SipApp process stop due to an error, it will be automatically restarted by its supervisor.


## Saving state information

NkSIP offers SipApps two different methods to store specific runtime application information:
* SipApp variables
* SipApp gen_server state

### SipApp Variables
Yon can store, read and delete _any Erlang term_ under _any key_ calling [nksip:put/3](../api/sipapp.md#put3), [nksip:get/2,3](../api/sipapp.md#get2) and [nksip:del/2](../api/sipapp.md#del2) API functions. NkSIP uses a standard Erlang _ETS table_ for storage, and it will destroyed when you stop the SipApp.

You can call these functions from any point, inside or outside a callback function. Keep in mind that, as and ETS table, if you are calling them from simultaenous processes (as it will happen from callback functions belonging to different calls) there is no guarantee that any call will be processed before another. You shouldn't, for example, read a value and store a modified version later on with a key that another call can also use, because another process could have changed it in between. If you want to control access using shared keys, you can call them from inside the SipApp's gen_server process, using `nksip:call/2,3` or `nksip:cast/2`, and calling `nksip:put/3`, `nksip:get/2,3` and `nksip:del/2` from inside the `handle_call` or `handle_cast` callback function.

The ETS table is associated to the SipApp's supervisor process, so it will not be lost in case your application fails and it is restarted by the supervisor. It will be lost when stopping the SipApp.


### SipApp gen_server state
You can also use the state stored in the gen_server process, following the standard OTP pattern. The initial state is stored when calling callback `init/1`. You should call `gen_server:call/2,3` or `gen_server:cast/2` from your callbacks functions, or send any message to the registered application name, and NkSIP will call again your `handle_call/3`, `handle_cast/2` or `handle_info/2` with the current state, so that you can access it and change it necessary.

Keep in mind that if your are receiving heavy traffic this can be a bottleneck, as all callbacks must wait for the gen_server process. Also if the application fails and the supervisor restarts the gen_server process the state would be lost.


## Reconfigure the SipApp
You can change any configured parameter for the SiApp at any time, on the fly, except for new transports specifications. You only need to call [nksip:update/2](../api/sipapp.md#update2) to update any configuration parameter.
