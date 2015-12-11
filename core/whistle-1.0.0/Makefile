ROOT = ../..
PROJECT = whistle

EBINS = $(shell find $(ROOT)/deps/rabbitmq_client-* -maxdepth 2 -name ebin -print)

TEST_EBINS = $(shell find $(ROOT)/deps/mochiweb-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/kazoo_web* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/ejson-* -maxdepth 2 -name ebin -print)

SOURCES = src/kazoo_transform.erl src/*.erl src/api/*.erl

all: compile

-include $(ROOT)/make/kz.mk
