ROOT = ../..
PROJECT = nksip

EBINS = $(wildcard $(ROOT)/deps/wsock-*/ebin)

TEST_EBINS = $(wildcard $(ROOT)/deps/goldrush-*/ebin) \
	$(wildcard $(ROOT)/deps/lager_syslog-*/ebin) \
	$(wildcard $(ROOT)/deps/erlang-syslog-*/ebin)

ERLC_OPTS += +"{parse_transform, lager_transform}" -Iplugins/include

SOURCES = src/*.erl plugins/src/*.erl

all: compile

-include $(ROOT)/make/kz.mk
