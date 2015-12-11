ROOT = ../../../..
PROJECT = apns

EBINS = $(shell find $(ROOT)/applications/pusher/lib/apns -maxdepth 2 -name ebin -print)

ERLC_OPTS += +warn_missing_spec +warn_unused_vars -Iinclude/

all: compile

-include $(ROOT)/make/kz.mk
