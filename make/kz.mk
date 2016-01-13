## Kazoo Makefile targets

.PHONY: compile json compile-test clean clean-test eunit dialyze xref

## pipefail enforces that the command fails even when run through a pipe
SHELL = /bin/bash -o pipefail


## Use pedantic flags when compiling apps from applications/ & core/
ifeq ($(findstring deps, $(abspath Makefile)), deps)
    KZ_APP_OTPS =
else
    KZ_APP_OTPS += -Werror +warn_export_all +warn_unused_import +warn_unused_vars
endif

ERLC_OPTS += $(KZ_APP_OTPS) +debug_info -Iinclude -Isrc

## Ensure codebase compatibility throughout supported OTP versions
ERTS_VSN = $(shell erl +V 2>&1 | cut -d' ' -f6)
# Note: R15B03-1's ERTS version is 5.x
ifeq ($(findstring 6., $(ERTS_VSN)), 6.)
    ERLC_OPTS += +nowarn_deprecated_type +nowarn_deprecated_function
endif

ifeq ($(findstring 7., $(ERTS_VSN)), 7.)
    ERLC_OPTS += -DOTP_AT_LEAST_18
endif


EBINS += $(wildcard $(ROOT)/deps/lager-*/ebin) \
	$(wildcard $(ROOT)/core/whistle-*/ebin)
TEST_EBINS += $(EBINS)
PA      = -pa ebin/ $(foreach EBIN,$(EBINS),-pa $(EBIN))
TEST_PA = -pa test/ $(PA) $(foreach EBIN,$(TEST_EBINS),-pa $(EBIN))

## SOURCES provides a way to specify compilation order (left to right)
SOURCES     ?= src/*.erl $(if $(wildcard src/*/*.erl), src/*/*.erl)
TEST_SOURCES = $(SOURCES) $(if $(wildcard test/*.erl), test/*.erl)


## COMPILE_MOAR can contain Makefile-specific targets (see CLEAN_MOAR, compile-test)
compile: $(COMPILE_MOAR) ebin/$(PROJECT).app json

ebin/$(PROJECT).app: $(wildcard $(SOURCES))
	@mkdir -p ebin/
	ERL_LIBS=$(ERL_LIBS) erlc -v $(ERLC_OPTS) $(PA) -o ebin/ $(SOURCES)
	@sed "s/{modules, \[\]}/{modules, \[`echo ebin/*.beam | sed 's%[/.]% %g;s/ebin\|beam//g;s/   /, /g'`\]}/" src/$(PROJECT).app.src > $@


json: JSON = $(if $(wildcard priv/), $(shell find priv/ -name '*.json'))
json:
	@$(ROOT)/scripts/format-json.sh $(JSON)


compile-test: $(COMPILE_MOAR) test/$(PROJECT).app

test/$(PROJECT).app: ERLC_OPTS += -DTEST
test/$(PROJECT).app: $(wildcard $(TEST_SOURCES))
	@mkdir -p test/
	ERL_LIBS=$(ERL_LIBS) erlc -v $(ERLC_OPTS) $(TEST_PA) -o test/ $(TEST_SOURCES)
	@sed "s/{modules, \[\]}/{modules, \[`echo ebin/*.beam | sed 's%[/.]% %g;s/ebin\|beam//g;s/   /, /g'`\]}/" src/$(PROJECT).app.src > $@


clean: clean-test
	$(if $(wildcard ebin/*), rm ebin/*)
	$(if $(wildcard *crash.dump), rm *crash.dump)

clean-test: $(CLEAN_MOAR)
	$(if $(wildcard test/*.beam), rm test/*.beam)
	$(if $(wildcard test/$(PROJECT).app), rm test/$(PROJECT).app)


test: clean-test eunit

eunit: compile-test
	erl -noshell $(TEST_PA) -pa test/ -eval "case eunit:test([`echo test/*.beam | sed 's%[/.]% %g;s/ebin\|beam//g;s/   /, /g'`], [verbose]) of ok -> init:stop(); _ -> init:stop(1) end."


PLT ?= $(ROOT)/.kazoo.plt
$(PLT):
	$(MAKE) -C $(ROOT) '.kazoo.plt'

dialyze: TO_DIALYZE ?= $(abspath ebin)
dialyze: $(PLT) compile
	@$(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt $(TO_DIALYZE)


xref: TO_XREF = $(EBINS)
xref: compile
	@$(ROOT)/scripts/check-xref.escript $(TO_XREF)
