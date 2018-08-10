ROOT = ../..
PROJECT = kazoo_call

all: compile

compile-test: compile-test-other-apps

compile-test-other-apps:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_stdlib/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_config/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_data/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_documents/

include $(ROOT)/make/kz.mk
