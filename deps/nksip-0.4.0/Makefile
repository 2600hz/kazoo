ROOT = ../..
PROJECT = nksip

EBINS = $(shell find $(ROOT)/deps/wsock-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/lager-* -maxdepth 2 -name ebin -print)

TEST_EBINS = $(shell find $(ROOT)/deps/goldrush-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/lager_syslog-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/erlang-syslog-* -maxdepth 2 -name ebin -print)

ERLC_OPTS += +"{parse_transform, lager_transform}" -Iinclude -Iplugins/include

SOURCES = src/*.erl plugins/src/*.erl

all: compile

-include $(ROOT)/make/kz.mk
