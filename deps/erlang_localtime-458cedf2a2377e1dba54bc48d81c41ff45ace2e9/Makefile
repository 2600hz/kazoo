ROOT = ../..
PROJECT = erlang_localtime

EBINS = $(shell find $(ROOT)/deps/erlang_localtime-* -maxdepth 2 -name ebin -print)

ERLC_OPTS += -I $(wildcard $(ROOT)/deps/erlang_localtime-*/include)

COMPILE_MOAR = include/tz_index.hrl

all: compile

-include $(ROOT)/make/kz.mk

include/tz_index.hrl: src/ibuild.erl include/tz_database.hrl
	cd include && ln -s ../src/ibuild.erl && escript ibuild.erl; EV=$$?; rm ibuild.erl; exit $$EV
