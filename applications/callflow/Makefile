ROOT = ../..
PROJECT = callflow

EBINS = $(shell find $(ROOT)/core/whistle_apps-* -maxdepth 2 -name ebin) \
	$(shell find $(ROOT)/core/whistle_number_manager-* -maxdepth 2 -name ebin) \
	$(shell find $(ROOT)/core/kazoo_documents-* -maxdepth 2 -name ebin)

all: compile

-include $(ROOT)/make/kz.mk
