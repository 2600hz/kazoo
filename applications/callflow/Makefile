ROOT = ../..
PROJECT = callflow

all: schemas compile

compile-test: compile-extra

compile-extra:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_schemas/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_apps/
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_call/

schemas:
	@bash priv/scripts/check-schemas.bash

include $(ROOT)/make/kz.mk
