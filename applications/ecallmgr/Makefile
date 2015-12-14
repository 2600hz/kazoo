ROOT = ../..
PROJECT = ecallmgr

EBINS = $(shell find $(ROOT)/core/kazoo_documents-* -maxdepth 2 -name ebin) \
	$(shell find $(ROOT)/deps/nksip-* -maxdepth 2 -name ebin)

all: compile

-include $(ROOT)/make/kz.mk
