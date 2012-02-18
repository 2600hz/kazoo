Erlang Priority Queue Implementation
====================================

The priority queue implementations implement a subset of the stdlib Erlang queue interface as seen in the implementation used by both [Riak and RabbitMQ](https://github.com/basho/riak_core/blob/master/src/priority_queue.erl).

The implementations:

* priority_queue (fastest for any priorities when only using a single priority at a time)
* pqueue (fastest for 41 priorities, -20 (high) to 20 (low), when using 2 or more priorities at the same time)
* pqueue2 (slower heap implementation)
* pqueue3 (faster than pqueue2 and priority_queue when using 64 or more priorities at the same time)
* pqueue4 (slightly slower than pqueue but fastest for allowing 257 priorities, -128 (high) to 128 (low), i.e., fastest when using 42 or more priorities at the same time)

[The latest results are here](http://okeuday.livejournal.com/19539.html), with [the benchmark here](http://github.com/okeuday/erlbench).

Author
------

Michael Truog (mjtruog [at] gmail (dot) com)

Thanks
------

* Jesper Louis andersen (PropEr integration and testing)
* Ulf Wiger (suggestions and insight)

License
-------

BSD

