## Kazoo Makefile targets

.PHONY: compile compile-lean json compile-test clean clean-test eunit dialyze xref proper fixture_shell app_src depend $(DEPS_RULES) splchk

## Platform detection.
ifeq ($(PLATFORM),)
    UNAME_S := $(shell uname -s)

    ifeq ($(UNAME_S),Linux)
        PLATFORM = linux
    else ifeq ($(UNAME_S),Darwin)
        PLATFORM = darwin
    else ifeq ($(UNAME_S),SunOS)
        PLATFORM = solaris
    else ifeq ($(UNAME_S),GNU)
        PLATFORM = gnu
    else ifeq ($(UNAME_S),FreeBSD)
        PLATFORM = freebsd
    else ifeq ($(UNAME_S),NetBSD)
        PLATFORM = netbsd
    else ifeq ($(UNAME_S),OpenBSD)
        PLATFORM = openbsd
    else ifeq ($(UNAME_S),DragonFly)
        PLATFORM = dragonfly
    else
        $(error Unable to detect platform.)
    endif

    export PLATFORM
endif

## pipefail enforces that the command fails even when run through a pipe
SHELL = /bin/bash -o pipefail

ifndef ERLC_OPTS_SUPERSECRET
    ERLC_OPTS += +debug_info
else
    ERLC_OPTS += $(ERLC_OPTS_SUPERSECRET)
endif
ERLC_OPTS += -Iinclude -Isrc -I../ +'{parse_transform, lager_transform}'
## Use pedantic flags when compiling apps from applications/ & core/
ERLC_OPTS += +warn_export_all +warn_unused_import +warn_unused_vars +warn_missing_spec +deterministic

ifneq (,$(findstring 21._,$(OTP_VERSION)))
    ERLC_OPTS += -Werror
endif


#ERLC_OPTS += +warn_untyped_record

ELIBS ?= $(if $(ERL_LIBS),$(ERL_LIBS):)$(ROOT)/deps:$(ROOT)/core:$(ROOT)/applications

EBINS += $(ROOT)/deps/lager/ebin

TEST_EBINS += $(EBINS) $(ROOT)/deps/proper/ebin
PA      = -pa ebin/ $(foreach EBIN,$(EBINS),-pa $(EBIN))
TEST_PA = -pa ebin/ $(foreach EBIN,$(TEST_EBINS),-pa $(EBIN))

DEPS_RULES = .deps.mk

comma := ,
empty :=
space := $(empty) $(empty)

KZ_VERSION ?= $(shell $(ROOT)/scripts/next_version)

## SOURCES provides a way to specify compilation order (left to right)
SOURCES     ?= $(wildcard src/*.erl) $(wildcard src/*/*.erl)
MODULE_NAMES := $(sort $(foreach module,$(SOURCES),$(shell basename $(module) .erl)))
MODULES := $(shell echo $(MODULE_NAMES) | sed 's/ /,/g')
BEAMS := $(sort $(foreach module,$(SOURCES),ebin/$(shell basename $(module) .erl).beam))

TEST_SOURCES := $(SOURCES) $(wildcard test/*.erl)
TEST_MODULE_NAMES := $(sort $(foreach module,$(TEST_SOURCES),$(shell basename $(module) .erl)))
TEST_MODULES := $(shell echo $(TEST_MODULE_NAMES) | sed 's/ /,/g')

ifneq ($(wildcard $(DEPS_RULES)),)
include $(DEPS_RULES)
endif

## COMPILE_MOAR can contain Makefile-specific targets (see CLEAN_MOAR, compile-test)
compile: $(COMPILE_MOAR) ebin/$(PROJECT).app json depend $(BEAMS)

compile-lean: ERLC_OPTS := $(filter-out +debug_info,$(ERLC_OPTS))
compile-lean: compile

ebin/$(PROJECT).app:
	@mkdir -p ebin/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) $(PA) -o ebin/ $(SOURCES)
	@sed "s/{modules,[[:space:]]*\[\]}/{modules, \[$(MODULES)\]}/" src/$(PROJECT).app.src \
	| sed -e "s/{vsn,\([^}]*\)}/\{vsn,\"$(KZ_VERSION)\"}/g" > $@

ebin/%.beam: src/%.erl
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) $(PA) -o ebin/ $<

ebin/%.beam: src/*/%.erl
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) $(PA) -o ebin/ $<

depend: $(DEPS_RULES)

$(DEPS_RULES):
	@rm -f $(DEPS_RULES)
	ERL_LIBS=$(ELIBS) erlc -v +makedep +'{makedep_output, standard_io}' $(PA) -o ebin/ $(SOURCES) > $(DEPS_RULES)

app_src:
	@ERL_LIBS=$(ROOT)/deps:$(ROOT)/core:$(ROOT)/applications $(ROOT)/scripts/apps_of_app.escript -a $(shell find $(ROOT) -name $(PROJECT).app.src)


json: JSON = $(shell find . -name '*.json')
json:
	@$(ROOT)/scripts/format-json.sh $(JSON)

compile-test: clean-test $(COMPILE_MOAR) test/$(PROJECT).app json

test/$(PROJECT).app: ERLC_OPTS += -DTEST
test/$(PROJECT).app: $(TEST_SOURCES)
	@mkdir -p test/
	@mkdir -p ebin/
	ERL_LIBS=$(ELIBS) erlc -v +nowarn_missing_spec $(ERLC_OPTS) $(TEST_PA) -o ebin/ $?

	@sed "s/{modules,\s*\[\]}/{modules, \[$(TEST_MODULES)\]}/" src/$(PROJECT).app.src > $@
	@sed "s/{modules,\s*\[\]}/{modules, \[$(TEST_MODULES)\]}/" src/$(PROJECT).app.src > ebin/$(PROJECT).app

clean: clean-test
	@$(if $(wildcard cover/*), rm -r cover)
	@$(if $(wildcard ebin/*), rm ebin/*)
	@$(if $(wildcard *crash.dump), rm *crash.dump)
	@$(if $(wildcard $(DEPS_RULES)), rm $(DEPS_RULES))

clean-test: $(CLEAN_MOAR)
	$(if $(wildcard test/$(PROJECT).app), rm test/$(PROJECT).app)

TEST_CONFIG=$(ROOT)/rel/config-test.ini

## Use this one when debugging
test: compile-test
	KAZOO_CONFIG=$(TEST_CONFIG) ERL_LIBS=$(ELIBS) $(ROOT)/scripts/eunit_run.escript $(TEST_MODULE_NAMES)
test.%: check-compile-test
	KAZOO_CONFIG=$(TEST_CONFIG) ERL_LIBS=$(ELIBS) $(ROOT)/scripts/eunit_run.escript $*

check-compile-test: $(COMPILE_MOAR) test/$(PROJECT).app

COVER_REPORT_DIR=cover

## Use this one when CI
eunit: compile-test eunit-run

eunit-run:
	KAZOO_CONFIG=$(TEST_CONFIG) ERL_LIBS=$(ELIBS) $(ROOT)/scripts/eunit_run.escript --with-cover \
		--cover-project-name $(PROJECT) --cover-report-dir $(COVER_REPORT_DIR) \
		$(TEST_MODULE_NAMES)

cover: $(ROOT)/make/cover.mk
	COVER=1 $(MAKE) eunit

cover-report: $(ROOT)/make/core.mk $(ROOT)/make/cover.mk eunit
	COVER=1 CT_RUN=1 $(MAKE) -f $(ROOT)/make/core.mk -f $(ROOT)/make/cover.mk cover-report

$(ROOT)/make/cover.mk: $(ROOT)/make/core.mk
	wget 'https://raw.githubusercontent.com/ninenines/erlang.mk/master/plugins/cover.mk' -O $(ROOT)/make/cover.mk

$(ROOT)/make/core.mk:
	wget 'https://raw.githubusercontent.com/ninenines/erlang.mk/master/core/core.mk' -O $(ROOT)/make/core.mk

proper: compile-proper eunit-run

compile-proper: ERLC_OPTS += -DPROPER
compile-proper: compile-test

PLT ?= $(ROOT)/.kazoo.plt
$(PLT):
	$(MAKE) -C $(ROOT) '.kazoo.plt'

dialyze: TO_DIALYZE ?= $(abspath ebin)
dialyze: $(PLT) compile
	@ERL_LIBS=$(ROOT)/deps:$(ROOT)/core:$(ROOT)/applications $(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt $(TO_DIALYZE)

dialyze-hard: TO_DIALYZE ?= $(abspath ebin)
dialyze-hard: $(PLT) compile
	@ERL_LIBS=$(ROOT)/deps:$(ROOT)/core:$(ROOT)/applications $(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt --hard $(TO_DIALYZE)

REBAR=$(ROOT)/deps/.erlang.mk/rebar/rebar

xref: TO_XREF = ebin/  #FIXME: set TO_XREF to an app's dependencies' ebin/ directories
xref: compile
	@ERL_LIBS=$(ELIBS) $(REBAR) xref skip_deps=true -C $(ROOT)/make/xref.local.config

fmt: TO_FMT ?= $(shell find src include test -iname '*.erl' -or -iname '*.hrl' -or -iname '*.escript')

perf: ERLC_OPTS += -pa $(ROOT)/deps/horse/ebin -DPERF +'{parse_transform, horse_autoexport}'
perf: compile-test
	$(gen_verbose) @ERL_LIBS=$(ELIBS) erl -noshell  -pa $(ROOT)/deps/horse/ebin -pa $(TEST_PA) \
		-eval 'horse:app_perf($(PROJECT)), init:stop().'

fixture_shell: ERL_CRASH_DUMP = "$(ROOT)/$(shell date +%s)_ecallmgr_erl_crash.dump"
fixture_shell: ERL_LIBS = "$(ROOT)/deps:$(ROOT)/core:$(ROOT)/applications:$(shell echo $(ROOT)/deps/rabbitmq_erlang_client-*/deps)"
fixture_shell: NODE_NAME ?= fixturedb
fixture_shell:
	@ERL_CRASH_DUMP="$(ERL_CRASH_DUMP)" ERL_LIBS="$(ERL_LIBS)" KAZOO_CONFIG=$(ROOT)/rel/config-test.ini \
		erl -name '$(NODE_NAME)' -s reloader "$$@"

include $(ROOT)/make/splchk.mk
include $(ROOT)/make/fmt.mk
