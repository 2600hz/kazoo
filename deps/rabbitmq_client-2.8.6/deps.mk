ebin/rabbit_routing_util.beam: src/rabbit_routing_util.erl include/amqp_client.hrl include/rabbit_routing_prefixes.hrl
ebin/amqp_gen_consumer.beam: src/amqp_gen_consumer.erl include/amqp_client.hrl
ebin/amqp_sup.beam: src/amqp_sup.erl include/amqp_client.hrl
ebin/amqp_connection_type_sup.beam: src/amqp_connection_type_sup.erl include/amqp_client.hrl include/amqp_client_internal.hrl
ebin/amqp_uri.beam: src/amqp_uri.erl include/amqp_client.hrl
ebin/amqp_connection.beam: src/amqp_connection.erl include/amqp_client.hrl include/amqp_client_internal.hrl
ebin/amqp_rpc_client.beam: src/amqp_rpc_client.erl include/amqp_client.hrl
ebin/amqp_network_connection.beam: src/amqp_network_connection.erl ebin/amqp_gen_connection.beam include/amqp_client.hrl include/amqp_client_internal.hrl
ebin/amqp_auth_mechanisms.beam: src/amqp_auth_mechanisms.erl include/amqp_client.hrl
ebin/amqp_selective_consumer.beam: src/amqp_selective_consumer.erl include/amqp_client.hrl include/amqp_gen_consumer_spec.hrl ebin/amqp_gen_consumer.beam
ebin/amqp_channel_sup_sup.beam: src/amqp_channel_sup_sup.erl include/amqp_client.hrl
ebin/amqp_main_reader.beam: src/amqp_main_reader.erl include/amqp_client.hrl include/amqp_client_internal.hrl
ebin/amqp_direct_connection.beam: src/amqp_direct_connection.erl ebin/amqp_gen_connection.beam include/amqp_client.hrl include/amqp_client_internal.hrl
ebin/amqp_channel_sup.beam: src/amqp_channel_sup.erl include/amqp_client.hrl include/amqp_client_internal.hrl
ebin/amqp_rpc_server.beam: src/amqp_rpc_server.erl include/amqp_client.hrl
ebin/uri_parser.beam: src/uri_parser.erl
ebin/amqp_direct_consumer.beam: src/amqp_direct_consumer.erl include/amqp_client.hrl include/amqp_gen_consumer_spec.hrl ebin/amqp_gen_consumer.beam
ebin/amqp_channels_manager.beam: src/amqp_channels_manager.erl include/amqp_client.hrl include/amqp_client_internal.hrl
ebin/amqp_gen_connection.beam: src/amqp_gen_connection.erl include/amqp_client.hrl include/amqp_client_internal.hrl
ebin/amqp_channel.beam: src/amqp_channel.erl include/amqp_client.hrl include/amqp_client_internal.hrl
ebin/amqp_connection_sup.beam: src/amqp_connection_sup.erl include/amqp_client.hrl
ebin/amqp_client.beam: src/amqp_client.erl
deps.mk: ../rabbitmq_server-2.8.6/generate_deps
