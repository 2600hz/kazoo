## Kazoo Makefile targets

.PHONY: compile json compile-test clean clean-test eunit

SHELL = /bin/bash -o pipefail

ifeq ($(findstring deps, $(abspath Makefile)), deps)
    KZ_APP_OTPS =
else
    KZ_APP_OTPS = -Werror +warn_export_all +warn_unused_import
endif
ERLC_OPTS += $(KZ_APP_OTPS) +debug_info

ELIBS ?= $(ERL_LIBS):$(subst $(eval) ,:,$(wildcard $(ROOT)/deps/rabbitmq_client-*/deps))

EBINS += $(shell find $(ROOT)/deps/lager-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle-* -maxdepth 2 -name ebin -print)
TEST_EBINS += $(EBINS)
PA      = -pa ebin/ $(foreach EBIN,$(EBINS),-pa $(EBIN))
TEST_PA = -pa test/ $(PA) $(foreach EBIN,$(TEST_EBINS),-pa $(EBIN))

SOURCES ?= src/*.erl src/*/*.erl
TEST_SOURCES = $(SOURCES) test/*.erl
MODULES = $(shell      find       src/ -name '*.erl' | sed 's%[/.]% %g' | awk -vORS=, '{print $$(NF-1)}' | sed 's/,$$//')
TEST_MODULES = $(shell find test/ src/ -name '*.erl' | sed 's%[/.]% %g' | awk -vORS=, '{print $$(NF-1)}' | sed 's/,$$//')


compile: $(COMPILE_MOAR) ebin/$(PROJECT).app json

ebin/$(PROJECT).app: $(wildcard $(SOURCES))
	@mkdir -p ebin/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) $(PA) -o ebin/ $?
	@sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' src/$(PROJECT).app.src > ebin/$(PROJECT).app


json: JSON = $(if $(wildcard priv/), $(shell find priv/ -name '*.json' -print))
json:
	@$(ROOT)/scripts/format-json.sh $(JSON)


compile-test: $(COMPILE_MOAR) test/$(PROJECT).app

test/$(PROJECT).app: $(wildcard $(TEST_SOURCES))
	@mkdir -p test/
	ERL_LIBS=$(ELIBS) erlc -DTEST -v $(ERLC_OPTS) $(TEST_PA) -o test/ $?
	@sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' src/$(PROJECT).app.src > test/$(PROJECT).app


clean: clean-test
	$(if $(wildcard lib/), $(MAKE) -C lib/ clean)
	$(if $(wildcard ebin/*), rm ebin/*)
	$(if $(wildcard *crash.dump), rm *crash.dump)

clean-test: $(CLEAN_MOAR)
	$(if $(wildcard test/*.beam), rm test/*.beam)
	$(if $(wildcard test/$(PROJECT).app), rm test/$(PROJECT).app)


test: clean-test eunit

eunit: compile-test
	erl -noshell $(TEST_PA) -pa test/ -eval 'case eunit:test([$(TEST_MODULES)], [verbose]) of ok -> init:stop(); _ -> init:stop(1) end.'
