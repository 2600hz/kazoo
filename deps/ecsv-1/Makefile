ROOT = ../..
PROJECT = ecsv

EBINS = $(shell find $(ROOT)/core -maxdepth 2 -name ebin -print) \
        $(shell find $(ROOT)/deps -maxdepth 2 -name ebin -print)

ERLC_OPTS += -I$(ROOT)/core -I$(ROOT)/deps
     # +bin_opt_info

all: compile

-include $(ROOT)/make/kz.mk
