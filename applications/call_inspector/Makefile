ROOT = ../..
PROJECT = call_inspector

TEST_EBINS = $(shell find $(ROOT)/deps/mochiweb-* -maxdepth 2 -name ebin) \
             $(shell find $(ROOT)/deps/ejson-* -maxdepth 2 -name ebin)

all: compile

-include $(ROOT)/make/kz.mk
