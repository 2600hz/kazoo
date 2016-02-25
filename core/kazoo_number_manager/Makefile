ROOT = ../..
PROJECT = kazoo_number_manager

EBINS = $(ROOT)/core/kazoo_number_manager/ebin \
	$(ROOT)/core/kazoo_documents/ebin \
	$(ROOT)/core/kazoo_web/ebin \
	$(ROOT)/core/kazoo_xml/ebin \
	$(ROOT)/core/whistle_couch/ebin \
	$(ROOT)/core/whistle_services/ebin \
	$(ROOT)/core/whistle_transactions/ebin \
	$(shell find $(ROOT)/deps/cowlib-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/ejson-* -maxdepth 2 -name ebin -print) \

SOURCES = src/providers/knm_providers.erl src/*.erl src/*/*.erl

all: compile

-include $(ROOT)/make/kz.mk
