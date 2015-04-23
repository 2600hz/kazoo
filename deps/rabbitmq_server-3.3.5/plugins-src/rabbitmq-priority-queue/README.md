# Overview

This plugin adds support for priority queues to RabbitMQ.

# Downloading

You can download a pre-built binary of this plugin from
http://www.rabbitmq.com/community-plugins.html.

# Building

You can build and install it like any other plugin (see
[the plugin development guide](http://www.rabbitmq.com/plugin-development.html)).

# Declaring priority queues

Once the plugin is enabled, you can declare priority queues using the
`x-max-priority` argument. This argument should be an integer
indicating the maximum priority the queue should support. For example,
using the Java client:

    Channel ch = ...;
    Map<String, Object> args = new HashMap<String, Object>();
    args.put("x-max-priority", 10);
    ch.queueDeclare("my-priority-queue", true, false, false, args);

You can then publish prioritised messages using the `priority` field
of basic.properties. Larger numbers indicate higher priority.

There is a simple example using the Java client in the `examples` directory.

## Caution

While this plugin implements priority queues in terms of standard
queues, it does not support converting between a priority queue and a
standard queue, and the on-disc format is somewhat different. This has
the following implications:

* _It is dangerous to disable the plugin when durable priority queues exist_;
  the broker will fail to start again. Remember that on broker upgrade
  non-bundled plugins like this one need to be reinstalled.
* It is similarly dangerous to enable the plugin if you have declared
  durable queues with an `x-max-priority` argument without it. I have no
  idea why you'd do that, since you wouldn't get priority queues, but
  it would also lead to broker crashes on startup.
* Priority queues can only be defined by arguments, not policies. Queues can
  never change the number of priorities they support.

## Argument equivalence

RabbitMQ does not have a way for plugins to validate queue
arguments. Therefore the usual equivalence-checking that happens with
arguments does not happen here:

* You can declare a queue with `x-max-priority` and then declare it
  again without `x-max-priority`; no error will result.
* Conversely, you can declare a queue without `x-max-priority` and then
  declare it again with `x-max-priority`; again no error will result,
  (but the queue will not become a priority queue).
* You can declare a queue with an `x-max-priority` argument which is not
  an integer. The plugin will ignore this argument.

# Behaviour

The AMQP spec is a bit vague about how priorities work. It says that
all queues MUST support at least 2 priorities, and MAY support up to
10. It does not define how messages without a priority property are
treated.

In contrast to the AMQP spec, RabbitMQ queues by default do not
support priorities. When creating priority queues using this plugin,
you can specify as many priority levels as you like. Note that:

* There is some in-memory and on-disc cost per priority level per
  queue, so you may not wish to create huge numbers of levels.
* The message `priority` field is defined as an unsigned byte, so in
  practice priorities should be between 0 and 255.

Messages without a priority property are treated as if their priority were
0. Messages with a priority which is higher than the queue's
maximum are treated as if they were published with the maximum priority.

## Interaction with other features

In general priority queues have all the features of standard RabbitMQ
queues: they support persistence, paging, mirroring, and so on. There
are a couple of interactions that should be noted though:

* Messages which should expire (as at
  http://www.rabbitmq.com/ttl.html) will still only expire from the
  head of the queue. This means that unlike with normal queues, even
  per-queue TTL can lead to expired lower-priority messages getting
  stuck behind non-expired higher priority ones. These messages will
  never be delivered, but they will appear in queue statistics.

* Queues which have a max-length set (as at
  http://www.rabbitmq.com/maxlength.html) will, as usual, drop
  messages from the head of the queue to enforce the limit. This means
  that higher priority messages might be dropped to make way for lower
  priority ones, which might not be what you would expect.
