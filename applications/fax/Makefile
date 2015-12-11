ROOT = ../..
PROJECT = fax

EBINS = $(shell find $(ROOT)/core/kazoo_oauth-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/gen_smtp-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/escalus-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/exml-* -maxdepth 2 -name ebin -print)

all: compile

-include $(ROOT)/make/kz.mk
