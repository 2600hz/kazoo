ROOT = ../..
PROJECT = cccp

EBINS = $(ROOT)/core/whistle_apps/ebin \
	$(ROOT)/core/whistle_number_manager/ebin \
	$(shell find $(ROOT)/deps/rabbitmq_erlang_client-* -name ebin)

all: compile

include ../kz.mk
