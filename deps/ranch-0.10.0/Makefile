ROOT = ../..
PROJECT = ranch

EBINS = $(shell find $(ROOT)/core -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps -maxdepth 2 -name ebin -print)

ERLC_OPTS += -I$(ROOT)/deps
     # +bin_opt_info

SOURCES = src/ranch_transport.erl src/*.erl

all: compile

-include $(ROOT)/make/kz.mk
