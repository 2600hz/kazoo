ROOT = ../..
PROJECT = kazoo_endpoint

all: compile

eunit: kze-test

test: kze-test

compile-test: kze-test

kze-test:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_number_manager/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_sip/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_stdlib/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_schemas/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_call/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_data/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_fixturedb/

include $(ROOT)/make/kz.mk
