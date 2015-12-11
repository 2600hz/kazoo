ROOT = ../..
PROJECT = acdc

EBINS = $(shell find $(ROOT)/core/whistle_apps-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_couch-* -maxdepth 2 -name ebin -print)

all: compile

-include $(ROOT)/make/kz.mk
