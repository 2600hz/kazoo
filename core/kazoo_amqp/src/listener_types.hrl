-ifndef(KZ_LISTENER_TYPES_HRL).

-include_lib("kazoo_amqp/include/kz_amqp.hrl").
-include_lib("kazoo/include/kz_types.hrl").
-include_lib("kazoo/include/kz_log.hrl").

-type binding_module() :: atom() | ne_binary().
-type binding() :: {binding_module(), kz_proplist()}. %% {kapi_module, options}
-type bindings() :: [binding()].

%% ExchangeName, ExchangeType[, ExchangeOptions]
-type declare_exchange() :: {ne_binary(), ne_binary()} |
                            {ne_binary(), ne_binary(), kz_proplist()}.
-type declare_exchanges() :: [declare_exchange()].

-type start_params() :: [{'responders', responder_start_params()} |
                         {'bindings', bindings()} |
                         {'queue_name', binary()} |
                         {'queue_options', kz_proplist()} |
                         {'consume_options', kz_proplist()} |
                         {'basic_qos', non_neg_integer()} |
                         {'broker' | 'broker_tag', ne_binary()} |
                         {'declare_exchanges', declare_exchanges()}
                        ].

-type responder_callback_fun2() :: fun((kz_json:object(), kz_proplist()) -> no_return()).
-type responder_callback_fun3() :: fun((kz_json:object(), kz_proplist(), basic_deliver()) -> no_return()).
-type responder_callback_fun() :: responder_callback_fun2() | responder_callback_fun3().

-type responder_callback() :: atom() | {atom(), atom()} | responder_callback_fun().
-type responder_callback_mapping() :: {ne_binary(), ne_binary()}.
-type responder_callback_mappings() :: [responder_callback_mapping()].

-type responder_start_params() :: [{responder_callback(), responder_callback_mappings()}].

%% { {Event-Category, Event-Name}, CallbackModule | {CallbackModule, Function} | CallbackFun}
-type responder() :: {responder_callback_mapping(), responder_callback()}.
-type responders() :: [responder()].

-define(KZ_LISTENER_TYPES_HRL, 'true').
-endif.
