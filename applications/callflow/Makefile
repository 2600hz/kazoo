ROOT = ../..
PROJECT = callflow

all: schemas compile

compile-test: compile-extra

compile-extra:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_schemas/

schemas:
	@bash priv/scripts/check-schemas.bash

include $(ROOT)/make/kz.mk
