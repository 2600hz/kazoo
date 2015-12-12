ROOT = ../..
PROJECT = pusher

EBINS = lib/apns/ebin lib/gcm/ebin \
	$(shell find $(ROOT)/deps/nksip-* -maxdepth 2 -name ebin -print)

COMPILE_MOAR = compile-lib

all: compile

compile-lib:
	$(MAKE) -C lib/ all

-include $(ROOT)/make/kz.mk

ERLC_OPTS += +warn_missing_spec
