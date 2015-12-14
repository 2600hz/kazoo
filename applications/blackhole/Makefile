ROOT = ../..
PROJECT = blackhole

EBINS = $(shell find $(ROOT)/core/whistle_apps-* -maxdepth 2 -name ebin) \
	$(shell find $(ROOT)/deps/rabbitmq_client-* -maxdepth 2 -name ebin)

all: compile

-include $(ROOT)/make/kz.mk
