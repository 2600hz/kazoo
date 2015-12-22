ROOT = ../..
PROJECT = whistle

EBINS = $(shell find $(ROOT)/deps/rabbitmq_erlang_client-* -name ebin)

TEST_EBINS = $(shell find $(ROOT)/deps/mochiweb-* -maxdepth 2 -name ebin) \
	$(shell find $(ROOT)/core/kazoo_web* -maxdepth 2 -name ebin) \
	$(shell find $(ROOT)/deps/ejson-* -maxdepth 2 -name ebin)

SOURCES = src/kazoo_transform.erl src/*.erl src/api/*.erl

all: compile

-include $(ROOT)/make/kz.mk
