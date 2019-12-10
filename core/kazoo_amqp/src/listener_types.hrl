-ifndef(KZ_LISTENER_TYPES_HRL).

-include_lib("kazoo_amqp/include/kz_amqp.hrl").
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").

-type binding_module() :: atom() | kz_term:ne_binary().

-type binding_property() :: {'restrict_to', list()} |
                            {'callid', kz_term:ne_binary()}.
-type binding_properties() :: [binding_property()].

-type binding() :: {binding_module(), binding_properties()}.
%% `{kapi_module, options}'
-type bindings() :: [binding()].

-type declare_exchange() :: {kz_term:ne_binary(), kz_term:ne_binary()} |
                            {kz_term:ne_binary(), kz_term:ne_binary(), kz_term:proplist()}.
%% `ExchangeName, ExchangeType[, ExchangeOptions]'
-type declare_exchanges() :: [declare_exchange()].

-type start_params() :: [{'responders', responder_start_params()} |
                         {'bindings', bindings()} |
                         {'queue_name', binary()} |
                         {'queue_options', kz_term:proplist()} |
                         {'consume_options', kz_term:proplist()} |
                         {'basic_qos', non_neg_integer()} |
                         {'broker' | 'broker_tag', kz_term:ne_binary()} |
                         {'declare_exchanges', declare_exchanges()} |
                         {'auto_ack', boolean()} |
                         {'server_confirms', boolean()} |
                         {'channel_flow', boolean()}
                        ].

-type responder_callback_fun2() :: fun((kz_json:object(), kz_term:proplist()) -> any()).
-type responder_callback_fun3() :: fun((kz_json:object(), kz_term:proplist(), basic_deliver()) -> any()).
-type responder_callback_fun4() :: fun((kz_json:object(), kz_term:proplist(), basic_deliver(), amqp_basic()) -> any()).
-type responder_callback_fun() :: responder_callback_fun2() |
                                  responder_callback_fun3() |
                                  responder_callback_fun4().

-type responder_callback() :: module() |
                              {module(), atom()} |
                              responder_callback_fun().
-type responder_callback_mapping() :: {kz_term:ne_binary(), kz_term:ne_binary()}.
%% `{Event-Category, Event-Name}`
-type responder_callback_mappings() :: [responder_callback_mapping()].

-type responder_start_params() :: [{responder_callback(), responder_callback_mappings()}].

-type responder_mfa() :: mfa() | {responder_callback_fun(), arity()}.
%% `CallbackModule | {CallbackModule, Function} | CallbackFun'

-type responder() :: {responder_callback_mapping(), responder_mfa()}.
%% `{{Event-Category, Event-Name}, CallbackModule | {CallbackModule, Function} | CallbackFun}'
-type responders() :: [responder()].

-define(KZ_LISTENER_TYPES_HRL, 'true').
-endif.
