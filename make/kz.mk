## Kazoo Makefile targets

.PHONY: compile json compile-test clean clean-test eunit dialyze xref proper

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
ERLC_OPTS += -Werror +warn_export_all +warn_unused_import +warn_unused_vars +warn_missing_spec

ELIBS = $(ERL_LIBS):$(ROOT)/deps:$(ROOT)/core

EBINS += $(ROOT)/core/kazoo/ebin \
	$(ROOT)/deps/lager/ebin

TEST_EBINS += $(EBINS) $(ROOT)/deps/proper/ebin
PA      = -pa ebin/ $(foreach EBIN,$(EBINS),-pa $(EBIN))
TEST_PA = -pa ebin/ $(foreach EBIN,$(TEST_EBINS),-pa $(EBIN))

## SOURCES provides a way to specify compilation order (left to right)
SOURCES     ?= src/*.erl $(if $(wildcard src/*/*.erl), src/*/*.erl)
TEST_SOURCES = $(SOURCES) $(if $(wildcard test/*.erl), test/*.erl)

## COMPILE_MOAR can contain Makefile-specific targets (see CLEAN_MOAR, compile-test)
compile: $(COMPILE_MOAR) ebin/$(PROJECT).app json

ebin/$(PROJECT).app: $(SOURCES)
	@mkdir -p ebin/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) $(PA) -o ebin/ $?
	@sed "s/{modules, \[\]}/{modules, \[`echo ebin/*.beam | sed 's%\.beam ebin/%, %g;s%ebin/%%;s/\.beam//'`\]}/" src/$(PROJECT).app.src > $@


json: JSON = $(if $(wildcard priv/), $(shell find priv/ -name '*.json'))
json:
	@$(ROOT)/scripts/format-json.sh $(JSON)


compile-test: clean-test $(COMPILE_MOAR) test/$(PROJECT).app

test/$(PROJECT).app: ERLC_OPTS += -DTEST
test/$(PROJECT).app: $(TEST_SOURCES)
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
	ERL_LIBS=$(ELIBS) erl -noshell $(TEST_PA) -eval "_ = cover:start(), cover:compile_beam_directory(\"ebin\"), case eunit:test([`echo ebin/*.beam | sed 's%\.beam ebin/%, %g;s%ebin/%%;s/\.beam//'`], [verbose]) of ok -> cover:export(\"$(PROJECT).coverdata\"), init:stop(); _ -> init:stop(1) end."

proper: ERLC_OPTS += -DPROPER
proper: compile-test test

PLT ?= $(ROOT)/.kazoo.plt
$(PLT):
	$(MAKE) -C $(ROOT) '.kazoo.plt'

dialyze: TO_DIALYZE ?= $(abspath ebin)
dialyze: $(PLT) compile
	@$(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt $(TO_DIALYZE)


REBAR=$(ROOT)/deps/.erlang.mk/rebar/rebar

xref: TO_XREF = ebin/  #FIXME: set TO_XREF to an app's dependencies' ebin/ directories
xref: compile
	@ERL_LIBS=$(ELIBS) $(REBAR) xref skip_deps=true -C $(ROOT)/make/xref.local.config


FMT = $(ROOT)/make/erlang-formatter-master/fmt.sh
$(FMT):
	wget 'https://codeload.github.com/fenollp/erlang-formatter/tar.gz/master' -O - | tar xvz -C $(ROOT)/make/

fmt: TO_FMT ?= $(shell find src include -iname '*.erl' -or -iname '*.hrl' -or -iname '*.app.src')
fmt: $(FMT)
	@$(FMT) $(TO_FMT)
