ROOT = ../..
PROJECT = frontier

EBINS = $(shell find $(ROOT)/core/whistle_apps-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/rabbitmq_client-* -maxdepth 2 -name ebin -print)

all: compile

-include $(ROOT)/make/kz.mk
