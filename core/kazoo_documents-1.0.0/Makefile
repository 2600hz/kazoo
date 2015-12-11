ROOT = ../..
PROJECT = kazoo_documents

EBINS = $(shell find $(ROOT)/applications/crossbar -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_couch-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/cowboy-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/ejson-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/jesse-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/kazoo_documents-* -maxdepth 2 -name ebin -print)

all: compile

-include $(ROOT)/make/kz.mk
