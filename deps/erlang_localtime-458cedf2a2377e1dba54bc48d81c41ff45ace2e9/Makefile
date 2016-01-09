ROOT = ../..
PROJECT = erlang_localtime

COMPILE_MOAR = include/tz_index.hrl

all: compile

-include $(ROOT)/make/kz.mk

include/tz_index.hrl: src/ibuild.erl include/tz_database.hrl
	cd include && ln -s ../src/ibuild.erl && escript ibuild.erl; EV=$$?; rm ibuild.erl; exit $$EV
