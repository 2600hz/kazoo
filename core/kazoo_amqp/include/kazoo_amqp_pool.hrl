-ifndef(KAZOO_AMQP_POOL).

-include_lib("kazoo_stdlib/include/kz_types.hrl").

-define(DEFAULT_POOL_SIZE, 150).
-define(DEFAULT_POOL_OVERFLOW, 100).
-define(DEFAULT_POOL_THRESHOLD, 5).
-define(DEFAULT_POOL_SERVER_CONFIRMS, false).

%%% Move the section to kazoo_apps or ecallmgr for per-vm control

-define(POOL_NAME_ARGS(Name, Args), ?WORKER_NAME_ARGS('poolboy', Name, Args)).

-define(ADD_POOL_ARGS(Pool, Broker, Size, Overflow, Bindings, Exchanges, ServerAck),
        [[{'worker_module', 'kz_amqp_worker'}
         ,{'name', {'local', Pool}}
         ,{'size', Size}
         ,{'max_overflow', Overflow}
         ,{'strategy', 'fifo'}
         ,{'neg_resp_threshold', ?POOL_THRESHOLD}
         ,{'amqp_broker', Broker}
         ,{'amqp_queuename_start', Pool}
         ,{'amqp_bindings', Bindings}
         ,{'amqp_exchanges', Exchanges}
         ,{'amqp_server_confirms', ServerAck}
         ]]).

-define(KAZOO_AMQP_POOL, 'true').
-endif.
