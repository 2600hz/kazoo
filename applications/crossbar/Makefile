ROOT = ../..
PROJECT = crossbar

COMPILE_MOAR = priv/couchdb/schemas/port_requests.to_scheduled.json
CLEAN_MOAR = clean-port-request-scheduled-schema

all: compile

priv/couchdb/schemas/port_requests.to_scheduled.json:
	@ERL_LIBS=$(ROOT)/deps/:$(ROOT)/core/:$(ROOT)/applications/ priv/port-request-scheduled-schema.escript

clean-port-request-scheduled-schema:
	$(if $(wildcard $(COMPILE_MOAR)), rm $(COMPILE_MOAR))

include $(ROOT)/make/kz.mk
