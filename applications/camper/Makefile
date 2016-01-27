ROOT = ../..
PROJECT = camper

EBINS = $(wildcard $(ROOT)/core/whistle_apps-*/ebin) \
	$(shell find $(ROOT)/deps/rabbitmq_erlang_client-* -name ebin)

all: compile

-include $(ROOT)/make/kz.mk
