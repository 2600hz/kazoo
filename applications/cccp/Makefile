ROOT = ../..
PROJECT = cccp

EBINS = $(wildcard $(ROOT)/core/whistle_apps-*/ebin) \
	$(wildcard $(ROOT)/core/whistle_number_manager-*/ebin) \
	$(shell find $(ROOT)/deps/rabbitmq_erlang_client-* -name ebin)

all: compile

-include $(ROOT)/make/kz.mk
