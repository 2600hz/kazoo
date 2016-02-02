ROOT = ../..
PROJECT = kazoo_documents

EBINS =	$(ROOT)/core/kazoo_documents/ebin

TEST_EBINS = $(ROOT)/applications/crossbar/ebin \
	$(ROOT)/core/whistle_couch/ebin \
	$(wildcard $(ROOT)/deps/cowboy-*/ebin) \
	$(wildcard $(ROOT)/deps/ejson-*/ebin) \
	$(wildcard $(ROOT)/deps/jesse-*/ebin)

all: compile

-include $(ROOT)/make/kz.mk
