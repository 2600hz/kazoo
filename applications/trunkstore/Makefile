ROOT = ../..
PROJECT = trunkstore

EBINS = $(ROOT)/core/whistle_number_manager/ebin \
	$(shell find $(ROOT)/deps/rabbitmq_erlang_client-* -name ebin)

all: compile

-include $(ROOT)/make/kz.mk
