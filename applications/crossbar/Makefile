CWD = $(shell pwd -P)
ROOT = $(realpath $(CWD)/../..)
PROJECT = crossbar

COMPILE_MOAR = priv/couchdb/schemas/port_requests.to_scheduled.json
CLEAN_MOAR = clean-port-request-scheduled-schema

all: compile

eunit: cb-test

test: cb-test

cb-test:
	$(MAKE) compile-test -C $(ROOT)/core/kazoo_endpoint/

priv/couchdb/schemas/port_requests.to_scheduled.json:
	@ERL_LIBS=$(ROOT)/deps/:$(ROOT)/core/:$(ROOT)/applications/ priv/port-request-scheduled-schema.escript

clean-port-request-scheduled-schema:
	$(if $(wildcard $(COMPILE_MOAR)), rm $(COMPILE_MOAR))

include $(ROOT)/make/kz.mk
