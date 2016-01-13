ROOT = ../..
PROJECT = whistle_services

EBINS = $(wildcard $(ROOT)/core/whistle_transactions-*/ebin) \
	$(wildcard $(ROOT)/core/braintree-*/ebin) \
	$(wildcard $(ROOT)/core/whistle_number_manager-*/ebin)

TEST_EBINS = $(wildcard $(ROOT)/core/whistle_couch-*/ebin) \
	$(wildcard $(ROOT)/core/kazoo_documents-*/ebin) \
	$(wildcard $(ROOT)/deps/ejson-*/ebin)

ELIBS = $(ERL_LIBS):$(subst $(eval) ,:,$(wildcard $(ROOT)/core))

all: compile

-include $(ROOT)/make/kz.mk
