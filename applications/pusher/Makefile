ROOT = ../..
PROJECT = pusher

EBINS = lib/apns/ebin lib/gcm/ebin \
	$(shell find $(ROOT)/deps/nksip-* -maxdepth 2 -name ebin)

COMPILE_MOAR = compile-lib
CLEAN_MOAR = clean-lib

all: compile

compile-lib:
	$(MAKE) -C lib/ all
clean-lib:
	$(MAKE) -C lib/ clean

-include $(ROOT)/make/kz.mk

ERLC_OPTS += +warn_missing_spec
