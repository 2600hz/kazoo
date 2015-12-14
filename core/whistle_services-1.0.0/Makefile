ROOT = ../..
PROJECT = whistle_services

TEST_EBINS = $(shell find $(ROOT)/core/whistle_couch-* -maxdepth 2 -name ebin) \
	$(shell find $(ROOT)/core/kazoo_documents-* -maxdepth 2 -name ebin) \
	$(shell find $(ROOT)/deps/ejson-* -maxdepth 2 -name ebin)

ELIBS = $(ERL_LIBS):$(subst $(eval) ,:,$(wildcard $(ROOT)/core))

all: compile

-include $(ROOT)/make/kz.mk
