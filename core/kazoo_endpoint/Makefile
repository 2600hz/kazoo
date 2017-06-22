ROOT = ../..
PROJECT = kazoo_endpoint

all: compile

eunit: kze-test

test: kze-test

kze-test:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_number_manager/

include $(ROOT)/make/kz.mk
