ROOT = ../..
PROJECT = pusher

EBINS = lib/apns/ebin lib/gcm/ebin \
	$(shell find $(ROOT)/deps/nksip-* -maxdepth 2 -name ebin -print)

all: compile

-include $(ROOT)/make/kz.mk

ERLC_OPTS += +warn_missing_spec
