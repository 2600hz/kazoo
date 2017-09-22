ROOT = ../..
PROJECT = stepswitch

all: compile

eunit: kze-test

test: kze-test

kze-test:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_stdlib
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_data
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_config

include $(ROOT)/make/kz.mk
