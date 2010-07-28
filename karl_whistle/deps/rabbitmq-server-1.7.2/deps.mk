$(EBIN_DIR)/rabbit_alarm.beam: src/rabbit_alarm.erl
$(EBIN_DIR)/rabbit_node_monitor.beam: src/rabbit_node_monitor.erl
$(EBIN_DIR)/rabbit_heartbeat.beam: src/rabbit_heartbeat.erl
$(EBIN_DIR)/rabbit_sup.beam: src/rabbit_sup.erl
$(EBIN_DIR)/priority_queue.beam: src/priority_queue.erl
$(EBIN_DIR)/rabbit_binary_generator.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl include/rabbit_framing.hrl src/rabbit_binary_generator.erl
$(EBIN_DIR)/rabbit_exchange.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl include/rabbit_framing.hrl src/rabbit_exchange.erl
$(EBIN_DIR)/rabbit_multi.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl src/rabbit_multi.erl
$(EBIN_DIR)/rabbit_plugin_activator.beam: src/rabbit_plugin_activator.erl
$(EBIN_DIR)/rabbit_reader.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl include/rabbit_framing.hrl src/rabbit_reader.erl
$(EBIN_DIR)/tcp_acceptor_sup.beam: src/tcp_acceptor_sup.erl
$(EBIN_DIR)/pg_local.beam: src/pg_local.erl
$(EBIN_DIR)/rabbit.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl include/rabbit_framing.hrl src/rabbit.erl
$(EBIN_DIR)/rabbit_guid.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl src/rabbit_guid.erl
$(EBIN_DIR)/tcp_client_sup.beam: src/tcp_client_sup.erl
$(EBIN_DIR)/rabbit_dialyzer.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl src/rabbit_dialyzer.erl
$(EBIN_DIR)/rabbit_framing.beam: include/rabbit_framing.hrl src/rabbit_framing.erl
$(EBIN_DIR)/rabbit_misc.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl include/rabbit_framing.hrl src/rabbit_misc.erl
$(EBIN_DIR)/tcp_acceptor.beam: src/tcp_acceptor.erl
$(EBIN_DIR)/gen_server2.beam: src/gen_server2.erl
$(EBIN_DIR)/rabbit_net.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl src/rabbit_net.erl
$(EBIN_DIR)/vm_memory_monitor.beam: src/vm_memory_monitor.erl
$(EBIN_DIR)/rabbit_access_control.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl src/rabbit_access_control.erl
$(EBIN_DIR)/rabbit_basic.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl include/rabbit_framing.hrl src/rabbit_basic.erl
$(EBIN_DIR)/rabbit_mnesia.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl src/rabbit_mnesia.erl
$(EBIN_DIR)/rabbit_writer.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl include/rabbit_framing.hrl src/rabbit_writer.erl
$(EBIN_DIR)/rabbit_load.beam: src/rabbit_load.erl
$(EBIN_DIR)/rabbit_networking.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl src/rabbit_networking.erl
$(EBIN_DIR)/tcp_listener_sup.beam: src/tcp_listener_sup.erl
$(EBIN_DIR)/rabbit_amqqueue_sup.beam: src/rabbit_amqqueue_sup.erl
$(EBIN_DIR)/rabbit_tests.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl src/rabbit_tests.erl
$(EBIN_DIR)/rabbit_limiter.beam: $(EBIN_DIR)/gen_server2.beam src/rabbit_limiter.erl
$(EBIN_DIR)/rabbit_binary_parser.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl src/rabbit_binary_parser.erl
$(EBIN_DIR)/rabbit_channel.beam: $(EBIN_DIR)/gen_server2.beam include/rabbit_framing_spec.hrl include/rabbit.hrl include/rabbit_framing.hrl src/rabbit_channel.erl
$(EBIN_DIR)/rabbit_control.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl src/rabbit_control.erl
$(EBIN_DIR)/rabbit_error_logger.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl include/rabbit_framing.hrl src/rabbit_error_logger.erl
$(EBIN_DIR)/rabbit_persister.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl src/rabbit_persister.erl
$(EBIN_DIR)/rabbit_router.beam: $(EBIN_DIR)/gen_server2.beam include/rabbit_framing_spec.hrl include/rabbit.hrl src/rabbit_router.erl
$(EBIN_DIR)/rabbit_tracer.beam: src/rabbit_tracer.erl
$(EBIN_DIR)/tcp_listener.beam: src/tcp_listener.erl
$(EBIN_DIR)/rabbit_amqqueue.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl src/rabbit_amqqueue.erl
$(EBIN_DIR)/rabbit_framing_channel.beam: include/rabbit_framing_spec.hrl include/rabbit.hrl src/rabbit_framing_channel.erl
$(EBIN_DIR)/rabbit_hooks.beam: src/rabbit_hooks.erl
$(EBIN_DIR)/rabbit_sasl_report_file_h.beam: src/rabbit_sasl_report_file_h.erl
$(EBIN_DIR)/rabbit_error_logger_file_h.beam: src/rabbit_error_logger_file_h.erl
$(EBIN_DIR)/rabbit_amqqueue_process.beam: $(EBIN_DIR)/gen_server2.beam include/rabbit_framing_spec.hrl include/rabbit.hrl include/rabbit_framing.hrl src/rabbit_amqqueue_process.erl
$(EBIN_DIR)/rabbit_log.beam: src/rabbit_log.erl
deps.mk: generate_deps
