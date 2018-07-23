ROOT = ../..
PROJECT = kazoo_apps

all: compile

compile-test: compile-also

compile-also:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_stdlib/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_documents/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_data/

include $(ROOT)/make/kz.mk
