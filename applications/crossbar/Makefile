ROOT = ../..
PROJECT = crossbar

EBINS = $(shell find $(ROOT)/core/whistle_number_manager-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_couch-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_transactions-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/braintree-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/kazoo_oauth-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/kazoo_bindings-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/cowboy-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/ejson-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/rabbitmq_client-* -maxdepth 2 -name ebin -print)

all: compile

-include $(ROOT)/make/kz.mk
