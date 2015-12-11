ROOT = ../..
PROJECT = stepswitch

EBINS = $(shell find $(ROOT)/core/whistle_number_manager-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/kazoo_sip-* -maxdepth 2 -name ebin -print)

all: compile

-include $(ROOT)/make/kz.mk
