
# Overview

The Skel application serves as the barest of examples of creating a listener on the AMQP message bus, handling the JSON requests that come in, and responding with JSON as appropriate.

The example listener is for route requests, but the reader should know that several other requests exist, not limited to authentication, authorization, and other whapp-specific requests.

## The Listener

Generally speaking, most action starts at the listener. The listener typically builds on top of the gen\_listener pattern (found in lib/kazoo-1.0.0/src) to abstract the AMQP connection and reception of payloads off the message bus. Instead, the gen\_listener behaviour allows the implementing module to specify the types of events to pull off the AMQP bus, and the handler modules/functions for each type of JSON payload received. The handlers run as processes separate from the listener process, allowing it to consuming AMQP payloads as quickly as possible.

The main areas of the listener that need editing are the bindings, responders, optional queue name, queue options, consume options, and QoS for initialization, and the handle\_event/2 callback.

### Initialization

The full example first:

    gen_listener:start_link(?MODULE
                            ,[{bindings, [{api_module, [options]}
                                          ,{other_api_module, [options]}
                                         ]}
                             ,{responders, [{ {callback_module, callback_function}
                                              ,[{<<"category">>, <<"name">>}]
                                            }
                                           ]}
                             ,{queue_name, <<>>}
                             ,{queue_options, []}
                             ,{consume_options, []}
                             ,{basic_qos, 1}
                            ], []).

#### Bindings

The AMQP APIs available can be found in the modules in lib/kazoo-1.0.0/api/ (prefixed with kapi\_) and define the bindings needed to receive AMQP payloads of a certain type.

For instance, to receive route requests, one would add the 'route' binding and payloads sent directly to our queue, one would configure the bindings list like:

    {bindings, [{route, []}
                ,{self, []}
               ]
    }

These two 2-tuples will cause the gen\_listener to call `kapi_route:bind_q/2` and `kapi_self:bind_q/2` with the listener's queue. When shutting down the listener, the `unbind_q/2` variants will be called to cleanup the AMQP bindings.

Some of the kapi modules allow parameters to be passed in the second element of the tuple, changing either what bindings are added, or the binding key used. For instance, in lib/kazoo-1.0.0/src/api/kapi\_route.erl, one can see in the `bind_q/2` function that one could specify a realm, which would restrict the route requests received by this listener to just the realm.

#### Responders

Now that we've bound to all appropriate AMQP messages we want to consume, we need to tell the gen\_listener behaviour what to do with them when received. To do this, we pass in a list of responder module/function pairs and a list of event category and name pairs.

In the case of binding to route requests, we know to expect to receive events with ("dialplan", "route\_req") as the category/name. So we create a responder module skel\_route\_req.erl and define a handle\_req/2 function. In the start\_link parameters, we would inform gen\_listener of this like so:


    {responders, [{{skel_route_req, handle_req}, [{<<"dialplan">>, <<"route_req">>}]}]}.

As you can see, the same callback can take more than one category/name pair. One could also create one callback module (skel\_handlers, for instance) with a handle\_\* function specific to each type of category/name pair.

If you want to handle more than one type of name for a given category, you can use the <<"\*">> to match any category or name. So, if we expect multiple event names for the dialplan category, and we want to use the same handler for each, we could denote that like:

    {responders, [{{skel_handlers, handle_dialplan_req}, [{<<"dialplan">>, <<"*">>}]}]}

#### The rest

queue\_name: If you want an auto-generated, anonymous queue, you can omit the queue\_name tuple, or set it to {queue\_name, <<>>} (this is the default). If you want to use a named queue, include the name as the second element: `{queue_name, <<"named_queue">>}`.

queue\_options: Another parameter that can be omitted from the list of configs sent to gen\_listener, use this if you need different values than in lib/kazoo\_amqp-1.0.0/src/amqp\_util.erl, the new\_queue/2 function. This list controls how the broker creates the queue.

consume\_options: Omittable as well, these options control how consumption from the queue will be managed (see basic\_consume/2 in amqp\_util.erl).

basic\_qos: Only needed if you need to control prefetch count from the queue to multiple consumers; otherwise omit from the list.

### handle\_event/2

The one interaction with the AMQP payload the listener module has is with the handle\_event/2 callback. Before calling into the handler module(s)' functions, the gen\_listener will pass the payload and the listener's state, allowing the listener to pass back a proplist of values the callbacks may need/want. The response from handle\_event/2 should be `{reply, ConfigList}`, where ConfigList is an empty list or a list of key/value tuples.

This ConfigList is prepended with the following default config pairs: `{queue_name, Name}` and `{server, pid()}`. Name is the name of the AMQP queue (needed when the callbacks might need responses to their own AMQP requests (and why you typically bind with `{self, []}`). The pid() is the PID of the listener process, in case you need to make queries from the process's state.

The `ConfigList ++ [{queue\_name, Name}, {server, pid()}]` is passed as the second argument to the handler module(s)' functions (the first being the AMQP payload.

### Usage

Typical usage of the gen\_listener behaviour falls into two main patterns.

#### One App Instance per message

When you have multiple application instances running on different servers and you want only one instance to process any given message, creating a shared listener will utilize the round-robin functionality of the AMQP queue. Setup is easy:

    {'queue_name', <<"some_predefined_queue_name">>}
    ,{'queue_options', [{'exclusive', 'false'}]}
    ,{'consume_options', [{'exclusive', 'false'}]}

When each gen\_listener process starts, it will bind to the same queue on RabbitMQ and each message that enters the queue will be round-robined among the available consumers.

#### Every message to every app

When you want each message to go to all instances of an application, you need to give each instance its own queue on the broker. This is the default configuration for gen\_listener processes when you omit `queue_name`, `queue_options`, and `consume_options`.
