### Call priorities

Call priorities are designed to help processing important or VIP calls with special care (including "pushing' them through queues). Generally, the higher (larger) priority of a call, the faster it should be answered.

#### Priority support in Callflows
There is a `call-priority` call time variable that can be set using `cf_set_variable` module:

Usage example for `cf_set_variable` module:

	 "flow": {
	     "data": {
	         "variable": "call_priority",
	         "value": "6",
	         "channel": "a"
	     },
	     "module": "set_variable",
	     "children": {
	     }
	 }

The corresponding module `cf_branch_variable` forks a callflow to branches depending on variable value. If some key in child branch matches the value, the callflow will go that way.

As for now `cf_set_variable` and `cf_branch_variable` modules only support  `call_priority` variable.

Usage example for `cf_branch_variable` module:

	 "flow": {
	     "data": {
             "variable": "call_priority"
	     },
	     "module": "branch_variable",
         "children": {
             "7": {
                 "data": {
                     "id": "2e7a812a6e9405ad99ea1493123df03a",
                     "timeout": "20"
                 },
                 "module": "device",
                 "children": {}
             },
             "_": {
                 "data": {
                     "id": "4f18af4f889a368e8153232a7e891209",
                     "timeout": "20",
                 },
                 "module": "device",
                 "children": {}
             }
	     }
	 }

#### Call priority support in ACDC

ACDC application respects call\_priority value in calls and "pushes" the call through the queue until it matches queued call with higher priority (in fact, RabbitMQ queue priorities are used).

There are two ways to process a prioritized call with ACD queue:

* set the `priority` attribute in `cf_acdc_member` callflow module. The call will be put to the queue with set priority
* Use `cf_set_variable` module to set `call_priority` variable and then use `cf_acdc_member`. It will process the `call_priority` value automatically.

Some installation requirements exist for call priorities to work with ACDC:

 - Re-assure twice that  `rabbitmq_priority_queue` plugin is enabled in RabbitMQ (prior to 3.5.0). For 3.5.0 and later it is provided as part of the broker itself.
	 - execute `rabbitmq-plugins list` and make sure the plugin is included.
	 - make sure that there are no prioritized queues in the broker before the `rabbitmq_priority_queue` plugin is enabled. .
	 - shutdown the broker
	 - execute `rabbitmq-plugins enable rabbitmq_priority_queue`
	 - start the broker
 - New ACD queues now may contain `max_priority` so they will support given number of priorities!

For example:

    {
        "data": {
            "max_priority": 10,
            "name": "10_priority_queue"
        }
    }

Configuration example for broker with enabled `rabbitmq_priority_queue`:

    [root@... ~]### cat /etc/kazoo/rabbitmq/enabled_plugins
    [rabbitmq_management,rabbitmq_priority_queue].

#### **WARNING**

The following things are really **DANGEROUS** and should be avoided:

* Creating prioritized queues before priorities are enabled in the broker.
* Change number of priorities in existing queue
* Disable priorities in the broker while prioritized queues exist

If you need to do something from the list above please refer to the [manual](https://github.com/rabbitmq/rabbitmq-priority-queue/tree/3431dc1ef8ea53e9a556c6be8bc1b417ac03b58d).
