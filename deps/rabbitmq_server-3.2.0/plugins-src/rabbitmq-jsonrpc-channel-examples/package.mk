RELEASABLE:=true
DEPS:=rabbitmq-server rabbitmq-jsonrpc-channel

define construct_app_commands
	cp -r $(PACKAGE_DIR)/priv $(APP_DIR)
endef
