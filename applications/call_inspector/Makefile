ROOT = ../..
PROJECT = call_inspector

ELIBS = $(ERL_LIBS):$(subst $(eval) ,:,$(wildcard $(ROOT)/core))

TEST_EBINS = $(shell find $(ROOT)/deps/mochiweb-* -maxdepth 2 -name ebin) \
             $(shell find $(ROOT)/deps/ejson-* -maxdepth 2 -name ebin)

all: compile

-include $(ROOT)/make/kz.mk
