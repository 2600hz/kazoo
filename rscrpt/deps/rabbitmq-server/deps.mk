$(EBIN_DIR)/rabbit_alarm.beam: src/rabbit_alarm.erl
$(EBIN_DIR)/rabbit_msg_store_ets_index.beam: src/rabbit_msg_store_ets_index.erl include/rabbit_msg_store_index.hrl $(EBIN_DIR)/rabbit_msg_store_index.beam include/rabbit_msg_store.hrl include/rabbit.hrl
$(EBIN_DIR)/rabbit_node_monitor.beam: src/rabbit_node_monitor.erl
$(EBIN_DIR)/delegate.beam: src/delegate.erl $(EBIN_DIR)/gen_server2.beam
$(EBIN_DIR)/rabbit_exchange_type_fanout.beam: src/rabbit_exchange_type_fanout.erl include/rabbit_exchange_type_spec.hrl include/rabbit.hrl $(EBIN_DIR)/rabbit_exchange_type.beam
$(EBIN_DIR)/rabbit_heartbeat.beam: src/rabbit_heartbeat.erl
$(EBIN_DIR)/rabbit_memory_monitor.beam: src/rabbit_memory_monitor.erl $(EBIN_DIR)/gen_server2.beam
$(EBIN_DIR)/rabbit_restartable_sup.beam: src/rabbit_restartable_sup.erl include/rabbit.hrl
$(EBIN_DIR)/rabbit_sup.beam: src/rabbit_sup.erl include/rabbit.hrl
$(EBIN_DIR)/worker_pool.beam: src/worker_pool.erl $(EBIN_DIR)/gen_server2.beam
$(EBIN_DIR)/file_handle_cache.beam: src/file_handle_cache.erl
$(EBIN_DIR)/priority_queue.beam: src/priority_queue.erl
$(EBIN_DIR)/rabbit_binary_generator.beam: src/rabbit_binary_generator.erl include/rabbit.hrl include/rabbit_framing.hrl
$(EBIN_DIR)/rabbit_exchange.beam: src/rabbit_exchange.erl include/rabbit.hrl include/rabbit_framing.hrl
$(EBIN_DIR)/rabbit_msg_store.beam: src/rabbit_msg_store.erl $(EBIN_DIR)/gen_server2.beam include/rabbit_msg_store.hrl include/rabbit.hrl
$(EBIN_DIR)/rabbit_multi.beam: src/rabbit_multi.erl include/rabbit.hrl
$(EBIN_DIR)/rabbit_plugin_activator.beam: src/rabbit_plugin_activator.erl
$(EBIN_DIR)/rabbit_reader.beam: src/rabbit_reader.erl include/rabbit.hrl include/rabbit_framing.hrl
$(EBIN_DIR)/tcp_acceptor_sup.beam: src/tcp_acceptor_sup.erl
$(EBIN_DIR)/pg_local.beam: src/pg_local.erl
$(EBIN_DIR)/rabbit_msg_file.beam: src/rabbit_msg_file.erl include/rabbit_msg_store.hrl include/rabbit.hrl
$(EBIN_DIR)/rabbit.beam: src/rabbit.erl include/rabbit.hrl include/rabbit_framing.hrl
$(EBIN_DIR)/rabbit_guid.beam: src/rabbit_guid.erl
$(EBIN_DIR)/rabbit_invariable_queue.beam: src/rabbit_invariable_queue.erl include/rabbit_backing_queue_spec.hrl include/rabbit.hrl $(EBIN_DIR)/rabbit_backing_queue.beam
$(EBIN_DIR)/tcp_client_sup.beam: src/tcp_client_sup.erl
$(EBIN_DIR)/worker_pool_worker.beam: src/worker_pool_worker.erl $(EBIN_DIR)/gen_server2.beam
$(EBIN_DIR)/bpqueue.beam: src/bpqueue.erl
$(EBIN_DIR)/rabbit_dialyzer.beam: src/rabbit_dialyzer.erl
$(EBIN_DIR)/rabbit_exchange_type.beam: src/rabbit_exchange_type.erl
$(EBIN_DIR)/rabbit_framing.beam: src/rabbit_framing.erl include/rabbit_framing.hrl
$(EBIN_DIR)/rabbit_misc.beam: src/rabbit_misc.erl include/rabbit.hrl include/rabbit_framing.hrl
$(EBIN_DIR)/rabbit_msg_store_gc.beam: src/rabbit_msg_store_gc.erl $(EBIN_DIR)/gen_server2.beam include/rabbit.hrl
$(EBIN_DIR)/rabbit_queue_index.beam: src/rabbit_queue_index.erl include/rabbit.hrl
$(EBIN_DIR)/tcp_acceptor.beam: src/tcp_acceptor.erl
$(EBIN_DIR)/gen_server2.beam: src/gen_server2.erl
$(EBIN_DIR)/rabbit_ctl_usage.beam: src/rabbit_ctl_usage.erl
$(EBIN_DIR)/rabbit_multi_usage.beam: src/rabbit_multi_usage.erl
$(EBIN_DIR)/rabbit_net.beam: src/rabbit_net.erl include/rabbit.hrl
$(EBIN_DIR)/vm_memory_monitor.beam: src/vm_memory_monitor.erl
$(EBIN_DIR)/rabbit_access_control.beam: src/rabbit_access_control.erl include/rabbit.hrl
$(EBIN_DIR)/rabbit_basic.beam: src/rabbit_basic.erl include/rabbit.hrl include/rabbit_framing.hrl
$(EBIN_DIR)/rabbit_exchange_type_topic.beam: src/rabbit_exchange_type_topic.erl include/rabbit_exchange_type_spec.hrl include/rabbit.hrl $(EBIN_DIR)/rabbit_exchange_type.beam
$(EBIN_DIR)/rabbit_mnesia.beam: src/rabbit_mnesia.erl include/rabbit.hrl
$(EBIN_DIR)/rabbit_types.beam: src/rabbit_types.erl include/rabbit.hrl
$(EBIN_DIR)/rabbit_writer.beam: src/rabbit_writer.erl include/rabbit.hrl include/rabbit_framing.hrl
$(EBIN_DIR)/rabbit_load.beam: src/rabbit_load.erl
$(EBIN_DIR)/rabbit_networking.beam: src/rabbit_networking.erl include/rabbit.hrl
$(EBIN_DIR)/tcp_listener_sup.beam: src/tcp_listener_sup.erl
$(EBIN_DIR)/rabbit_amqqueue_sup.beam: src/rabbit_amqqueue_sup.erl include/rabbit.hrl $(EBIN_DIR)/supervisor2.beam
$(EBIN_DIR)/rabbit_backing_queue.beam: src/rabbit_backing_queue.erl
$(EBIN_DIR)/rabbit_queue_collector.beam: src/rabbit_queue_collector.erl include/rabbit.hrl
$(EBIN_DIR)/rabbit_tests.beam: src/rabbit_tests.erl include/rabbit.hrl include/rabbit_framing.hrl
$(EBIN_DIR)/rabbit_exchange_type_direct.beam: src/rabbit_exchange_type_direct.erl include/rabbit_exchange_type_spec.hrl include/rabbit.hrl $(EBIN_DIR)/rabbit_exchange_type.beam
$(EBIN_DIR)/rabbit_limiter.beam: src/rabbit_limiter.erl $(EBIN_DIR)/gen_server2.beam
$(EBIN_DIR)/rabbit_binary_parser.beam: src/rabbit_binary_parser.erl include/rabbit.hrl
$(EBIN_DIR)/rabbit_channel.beam: src/rabbit_channel.erl $(EBIN_DIR)/gen_server2.beam include/rabbit.hrl include/rabbit_framing.hrl
$(EBIN_DIR)/rabbit_control.beam: src/rabbit_control.erl include/rabbit.hrl
$(EBIN_DIR)/rabbit_error_logger.beam: src/rabbit_error_logger.erl include/rabbit.hrl include/rabbit_framing.hrl
$(EBIN_DIR)/rabbit_persister.beam: src/rabbit_persister.erl include/rabbit.hrl
$(EBIN_DIR)/rabbit_router.beam: src/rabbit_router.erl include/rabbit.hrl
$(EBIN_DIR)/rabbit_tracer.beam: src/rabbit_tracer.erl
$(EBIN_DIR)/supervisor2.beam: src/supervisor2.erl
$(EBIN_DIR)/tcp_listener.beam: src/tcp_listener.erl
$(EBIN_DIR)/rabbit_amqqueue.beam: src/rabbit_amqqueue.erl include/rabbit.hrl
$(EBIN_DIR)/rabbit_exchange_type_registry.beam: src/rabbit_exchange_type_registry.erl
$(EBIN_DIR)/rabbit_framing_channel.beam: src/rabbit_framing_channel.erl include/rabbit.hrl
$(EBIN_DIR)/rabbit_hooks.beam: src/rabbit_hooks.erl
$(EBIN_DIR)/rabbit_sasl_report_file_h.beam: src/rabbit_sasl_report_file_h.erl
$(EBIN_DIR)/delegate_sup.beam: src/delegate_sup.erl
$(EBIN_DIR)/gatherer.beam: src/gatherer.erl $(EBIN_DIR)/gen_server2.beam
$(EBIN_DIR)/rabbit_error_logger_file_h.beam: src/rabbit_error_logger_file_h.erl
$(EBIN_DIR)/rabbit_exchange_type_headers.beam: src/rabbit_exchange_type_headers.erl include/rabbit_exchange_type_spec.hrl include/rabbit.hrl $(EBIN_DIR)/rabbit_exchange_type.beam include/rabbit_framing.hrl
$(EBIN_DIR)/worker_pool_sup.beam: src/worker_pool_sup.erl
$(EBIN_DIR)/rabbit_amqqueue_process.beam: src/rabbit_amqqueue_process.erl $(EBIN_DIR)/gen_server2.beam include/rabbit.hrl include/rabbit_framing.hrl
$(EBIN_DIR)/rabbit_log.beam: src/rabbit_log.erl
$(EBIN_DIR)/rabbit_msg_store_index.beam: src/rabbit_msg_store_index.erl
$(EBIN_DIR)/rabbit_variable_queue.beam: src/rabbit_variable_queue.erl include/rabbit_backing_queue_spec.hrl include/rabbit.hrl $(EBIN_DIR)/rabbit_backing_queue.beam
deps.mk: generate_deps
