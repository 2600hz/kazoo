ROOT = ../..
PROJECT = kazoo_provisioner

all: compile

compile-test: compile-others

compile-others:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_documents
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_schemas
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_data

-include $(ROOT)/make/kz.mk
