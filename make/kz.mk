## Kazoo Makefile targets

.PHONY: compile json compile-test clean clean-test eunit dialyze xref

## pipefail enforces that the command fails even when run through a pipe
SHELL = /bin/bash -o pipefail

ERLC_OPTS += +debug_info -Iinclude -Isrc
## Use pedantic flags when compiling apps from applications/ & core/
ERLC_OPTS += -Werror +warn_export_all +warn_unused_import +warn_unused_vars

ELIBS = $(ERL_LIBS):$(ROOT)/deps:$(ROOT)/core
EBINS += $(ROOT)/core/whistle/ebin \
	     $(ROOT)/deps/lager/ebin
TEST_EBINS += $(EBINS)
PA      = -pa ebin/ $(foreach EBIN,$(EBINS),-pa $(EBIN))
TEST_PA = -pa ebin/ $(foreach EBIN,$(TEST_EBINS),-pa $(EBIN))

## SOURCES provides a way to specify compilation order (left to right)
SOURCES     ?= src/*.erl $(if $(wildcard src/*/*.erl), src/*/*.erl)
TEST_SOURCES = $(SOURCES) $(if $(wildcard test/*.erl), test/*.erl)


## COMPILE_MOAR can contain Makefile-specific targets (see CLEAN_MOAR, compile-test)
compile: $(COMPILE_MOAR) ebin/$(PROJECT).app json

ebin/$(PROJECT).app: $(wildcard $(SOURCES))
	@mkdir -p ebin/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) $(PA) -o ebin/ $(SOURCES)
	@sed "s/{modules, \[\]}/{modules, \[`echo ebin/*.beam | sed 's%\.beam ebin/%, %g;s%ebin/%%;s/\.beam//'`\]}/" src/$(PROJECT).app.src > $@


json: JSON = $(if $(wildcard priv/), $(shell find priv/ -name '*.json'))
json:
	@$(ROOT)/scripts/format-json.sh $(JSON)


compile-test: clean-test $(COMPILE_MOAR) test/$(PROJECT).app

test/$(PROJECT).app: ERLC_OPTS += -DTEST
test/$(PROJECT).app: $(wildcard $(TEST_SOURCES))
	@mkdir -p test/
	@mkdir -p ebin/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) $(TEST_PA) -o ebin/ $(TEST_SOURCES)
	@sed "s/{modules, \[\]}/{modules, \[`echo ebin/*.beam | sed 's%\.beam ebin/%, %g;s%ebin/%%;s/\.beam//'`\]}/" src/$(PROJECT).app.src > $@


clean: clean-test
	$(if $(wildcard ebin/*), rm ebin/*)
	$(if $(wildcard *crash.dump), rm *crash.dump)

clean-test: $(CLEAN_MOAR)
	$(if $(wildcard test/$(PROJECT).app), rm test/$(PROJECT).app)


## Use this one when debugging
test: compile-test
	ERL_LIBS=$(ELIBS) erl -noshell $(TEST_PA) -eval "case eunit:test([`echo ebin/*.beam | sed 's%\.beam ebin/%, %g;s%ebin/%%;s/\.beam//'`], [verbose]) of ok -> init:stop(); _ -> init:stop(1) end."

## Use this one when CI
eunit:
	ERL_LIBS=$(ELIBS) erl -noshell $(TEST_PA) -eval "case eunit:test([`echo ebin/*.beam | sed 's%\.beam ebin/%, %g;s%ebin/%%;s/\.beam//'`], [verbose]) of ok -> init:stop(); _ -> init:stop(1) end."


PLT ?= $(ROOT)/.kazoo.plt
$(PLT):
	$(MAKE) -C $(ROOT) '.kazoo.plt'

dialyze: TO_DIALYZE ?= $(abspath ebin)
dialyze: $(PLT) compile
	@$(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt $(TO_DIALYZE)


xref: TO_XREF = ebin/  #FIXME: set TO_XREF to an app's dependencies' ebin/ directories
xref: compile
	@ERL_LIBS=$(ELIBS) $(ROOT)/utils/rebar/rebar xref skip_deps=true -C $(ROOT)/make/xref.local.config
