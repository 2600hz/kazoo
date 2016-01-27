ROOT = ../..
PROJECT = fax

EBINS = $(wildcard $(ROOT)/core/kazoo_oauth-*/ebin) \
	$(wildcard $(ROOT)/deps/gen_smtp-*/ebin) \
	$(wildcard $(ROOT)/deps/escalus-*/ebin) \
	$(wildcard $(ROOT)/deps/exml-*/ebin)

all: compile

-include $(ROOT)/make/kz.mk
