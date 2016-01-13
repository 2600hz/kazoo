ROOT = ../..
PROJECT = crossbar

EBINS = $(wildcard $(ROOT)/core/whistle_number_manager-*/ebin) \
	$(wildcard $(ROOT)/core/whistle_couch-*/ebin) \
	$(wildcard $(ROOT)/core/whistle_transactions-*/ebin) \
	$(wildcard $(ROOT)/core/braintree-*/ebin) \
	$(wildcard $(ROOT)/core/kazoo_oauth-*/ebin) \
	$(wildcard $(ROOT)/core/kazoo_bindings-*/ebin) \
	$(wildcard $(ROOT)/deps/cowboy-*/ebin) \
	$(wildcard $(ROOT)/deps/ejson-*/ebin) \
	$(shell find $(ROOT)/deps/rabbitmq_erlang_client-* -name ebin)

all: compile

-include $(ROOT)/make/kz.mk
