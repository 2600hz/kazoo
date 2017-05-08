ROOT = ../..
PROJECT = tasks

all: compile

compile: headers

headers: src/modules/kt_rates.hrl src/task_modules.hrl

src/modules/kt_rates.hrl:
	./priv/schema_to_header.py $(ROOT)/applications/crossbar/priv/couchdb/schemas/rates.json src/modules/kt_rates.hrl

src/task_modules.hrl:
	./priv/list_task_modules.py

clean: clean-headers

clean-headers:
	@rm -f src/modules/kt_rates.hrl
	@rm -f src/task_modules.hrl

compile-test: headers

include $(ROOT)/make/kz.mk
