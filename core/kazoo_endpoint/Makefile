ROOT = ../..
PROJECT = kazoo_endpoint

all: compile

eunit: kze-test

test: kze-test

kze-test:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_number_manager/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_sip/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_stdlib/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_schemas/

include $(ROOT)/make/kz.mk
