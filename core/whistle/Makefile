ROOT = ../..
PROJECT = whistle

EBINS = $(shell find $(ROOT)/deps/rabbitmq_erlang_client-* -name ebin)

TEST_EBINS = $(ROOT)/core/kazoo_web/ebin \
	$(wildcard $(ROOT)/deps/mochiweb-*/ebin) \
	$(wildcard $(ROOT)/deps/ejson-*/ebin)

SOURCES = src/kazoo_transform.erl src/*.erl src/api/*.erl

all: compile

-include $(ROOT)/make/kz.mk
