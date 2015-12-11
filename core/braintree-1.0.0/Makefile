ROOT = ../..
PROJECT = braintree

EBINS = $(shell find $(ROOT)/core/braintree-* -maxdepth 2 -name ebin -print)

ERLC_OPTS += -Iinclude/

all: compile

-include $(ROOT)/make/kz.mk
