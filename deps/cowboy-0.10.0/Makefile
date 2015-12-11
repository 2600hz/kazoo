ROOT = ../..
PROJECT = cowboy

EBINS = $(shell find $(ROOT)/core -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps -maxdepth 2 -name ebin -print)

ERLC_OPTS += -I$(ROOT)/deps
     # +bin_opt_info

SOURCES = src/cowboy_middleware.erl src/cowboy_sub_protocol.erl src/*.erl

all: compile

-include $(ROOT)/make/kz.mk
