RELEASABLE:=true
DEPS:=rabbitmq-server rabbitmq-erlang-client erlang-rfc4627-wrapper rabbitmq-web-dispatch mochiweb-wrapper rabbitmq-jsonrpc

define construct_app_commands
	cp -r $(PACKAGE_DIR)/priv $(APP_DIR)
endef
