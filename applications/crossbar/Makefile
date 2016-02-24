ROOT = ../..
PROJECT = crossbar

EBINS = $(ROOT)/core/whistle_number_manager/ebin \
	$(ROOT)/core/whistle_couch/ebin \
	$(ROOT)/core/whistle_transactions/ebin \
	$(ROOT)/core/braintree/ebin \
	$(ROOT)/core/kazoo_oauth/ebin \
	$(ROOT)/core/kazoo_bindings/ebin \
	$(wildcard $(ROOT)/deps/cowboy-*/ebin) \
	$(wildcard $(ROOT)/deps/ejson-*/ebin) \
	$(shell find $(ROOT)/deps/rabbitmq_erlang_client-* -name ebin)

all: compile

include ../kz.mk
