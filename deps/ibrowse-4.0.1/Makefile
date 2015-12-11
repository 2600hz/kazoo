ROOT = ../..
PROJECT = ibrowse

EBINS = $(shell find $(ROOT)/core -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps -maxdepth 2 -name ebin -print)

all: compile

-include $(ROOT)/make/kz.mk

ERLC_OPTS += -I$(ROOT)/core -I$(ROOT)/deps -I$(wildcard $(ROOT)/deps/ibrowse-*/include) +nowarn_unused_vars
     # +bin_opt_info
