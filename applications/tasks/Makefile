ROOT = ../..
PROJECT = tasks

all: compile

compile: headers

headers: src/modules/kt_rates.hrl

src/modules/kt_rates.hrl:
	./schema_to_header.py $(ROOT)/applications/crossbar/priv/couchdb/schemas/rates.json src/modules/kt_rates.hrl

clean: clean-headers

clean-headers:
	rm src/modules/kt_rates.hrl

include $(ROOT)/make/kz.mk
