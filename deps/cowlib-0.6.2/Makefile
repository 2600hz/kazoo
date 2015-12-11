ROOT = ../..
PROJECT = cowlib

EBINS = $(shell find $(ROOT)/core -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps -maxdepth 2 -name ebin -print)

ERLC_OPTS += -I$(ROOT)/deps -I./include

all: compile

-include $(ROOT)/make/kz.mk
