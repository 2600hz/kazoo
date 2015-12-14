## Kazoo Makefile targets

.PHONY: compile json compile-test clean clean-test eunit dialyze

SHELL = /bin/bash -o pipefail


ifeq ($(findstring deps, $(abspath Makefile)), deps)
    KZ_APP_OTPS =
else
    KZ_APP_OTPS = -Werror +warn_export_all +warn_unused_import
endif

ERLC_OPTS += $(KZ_APP_OTPS) +debug_info

# Could use OTP_VERSION, if ≥ 17
OTP_VSN = $(shell erl -noshell -eval 'io:fwrite("~s\n", [erlang:system_info(otp_release)]).' -s erlang halt)

ifeq ($(findstring 1, $(OTP_VSN)), 1)
    ERLC_OPTS += +nowarn_deprecated_type +nowarn_deprecated_function
endif

ifeq ($(findstring 18, $(OTP_VSN)), 18)
    ERLC_OPTS += -DOTP_AT_LEAST_18
endif


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
	@sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' src/$(PROJECT).app.src > $@


json: JSON = $(if $(wildcard priv/), $(shell find priv/ -name '*.json' -print))
json:
	@$(ROOT)/scripts/format-json.sh $(JSON)


compile-test: $(COMPILE_MOAR) test/$(PROJECT).app

test/$(PROJECT).app: ERLC_OPTS += -DTEST
test/$(PROJECT).app: $(wildcard $(TEST_SOURCES))
	@mkdir -p test/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) $(TEST_PA) -o test/ $?
	@sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' src/$(PROJECT).app.src > $@


clean: clean-test
	$(if $(wildcard ebin/*), rm ebin/*)
	$(if $(wildcard *crash.dump), rm *crash.dump)

clean-test: $(CLEAN_MOAR)
	$(if $(wildcard test/*.beam), rm test/*.beam)
	$(if $(wildcard test/$(PROJECT).app), rm test/$(PROJECT).app)


test: clean-test eunit

eunit: compile-test
	erl -noshell $(TEST_PA) -pa test/ -eval 'case eunit:test([$(TEST_MODULES)], [verbose]) of ok -> init:stop(); _ -> init:stop(1) end.'


PLT ?= $(ROOT)/.kazoo.plt
$(PLT):
	$(MAKE) -C $(ROOT) '.kazoo.plt'

dialyze: TO_DIALYZE ?= $(abspath ebin)
dialyze: $(PLT) compile
	@$(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt $(TO_DIALYZE)
