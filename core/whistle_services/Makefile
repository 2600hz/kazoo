ROOT = ../..
PROJECT = whistle_services

EBINS = $(ROOT)/core/whistle_transactions/ebin \
	$(ROOT)/core/braintree/ebin \
	$(ROOT)/core/whistle_number_manager/ebin

TEST_EBINS = $(ROOT)/core/whistle_couch/ebin \
	$(ROOT)/core/kazoo_documents/ebin \
	$(wildcard $(ROOT)/deps/ejson-*/ebin)

all: compile

include ../kz.mk
