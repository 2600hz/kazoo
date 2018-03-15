ROOT = $(shell cd "$(dirname '.')" && pwd -P)
RELX = $(ROOT)/deps/relx
ELVIS = $(ROOT)/deps/elvis
FMT = $(ROOT)/make/erlang-formatter/fmt.sh

# You can override this when calling make, e.g. make JOBS=1
# to prevent parallel builds, or make JOBS="8".
JOBS ?= 1

KAZOODIRS = core/Makefile applications/Makefile

.PHONY: $(KAZOODIRS) deps core apps xref xref_release dialyze dialyze-it dialyze-apps dialyze-core dialyze-kazoo clean clean-test clean-release build-release build-ci-release tar-release release read-release-cookie elvis install ci diff fmt clean-fmt bump-copyright apis validate-swagger sdks coverage-report fs-headers docs validate-schemas circle circle-pre circle-fmt circle-codechecks circle-build circle-docs circle-schemas circle-dialyze circle-release circle-unstaged fixture_shell code_checks

all: compile

compile: ACTION = all
compile: deps kazoo

$(KAZOODIRS):
	@$(MAKE) -C $(@D) $(ACTION)

clean: ACTION = clean
clean: $(KAZOODIRS)
	$(if $(wildcard *crash.dump), rm *crash.dump)
	$(if $(wildcard scripts/log/*), rm -rf scripts/log/*)
	$(if $(wildcard rel/dev-vm.args), rm rel/dev-vm.args)
	$(if $(wildcard $(FMT)), rm -r $(dir $(FMT)))

clean-test: ACTION = clean-test
clean-test: $(KAZOODIRS)

clean-kazoo: ACTION = clean
clean-kazoo: $(KAZOODIRS)

compile-test: ACTION = compile-test
compile-test: ERLC_OPTS += +nowarn_missing_spec
compile-test: deps $(KAZOODIRS)

eunit: ACTION = eunit
eunit: $(KAZOODIRS)

proper: ACTION = eunit
proper: ERLC_OPTS += -DPROPER
proper: $(KAZOODIRS)

test: ACTION = test
test: ERLC_OPTS += -DPROPER
test: ERLC_OPTS += +nowarn_missing_spec
test: $(KAZOODIRS)

coverage-report:
	$(ROOT)/scripts/cover.escript

check: ERLC_OPTS += -DPROPER
check: compile-test eunit clean-kazoo kazoo

clean-deps:
	$(if $(wildcard deps/), $(MAKE) -C deps/ clean)
	$(if $(wildcard deps/), rm -r deps/)
	$(if $(wildcard .erlang.mk/), rm -r .erlang.mk/)

.erlang.mk:
	wget 'https://raw.githubusercontent.com/ninenines/erlang.mk/2018.03.01/erlang.mk' -O $(ROOT)/erlang.mk
	@$(MAKE) -f erlang.mk erlang-mk

deps: deps/Makefile
	@$(MAKE) -C deps/ all
deps/Makefile: .erlang.mk
	mkdir -p deps
	@$(MAKE) -f erlang.mk deps
	cp $(ROOT)/make/Makefile.deps deps/Makefile

core:
	@$(MAKE) -j$(JOBS) -C core/ all

apps: core
	@$(MAKE) -j$(JOBS) -C applications/ all

kazoo: apps

$(RELX):
	wget 'https://github.com/erlware/relx/releases/download/v3.23.0/relx' -O $@
	chmod +x $@

clean-release:
	$(if $(wildcard _rel/), rm -r _rel/)

build-release: $(RELX) clean-release rel/relx.config rel/relx.config.script rel/sys.config rel/vm.args
	$(RELX) --config rel/relx.config -V 2 release --relname 'kazoo'
build-dev-release: $(RELX) clean-release rel/dev.relx.config rel/dev.relx.config.script rel/dev.vm.args rel/dev.sys.config
	$(RELX) --dev-mode true --config rel/dev.relx.config -V 2 release --relname 'kazoo'
build-ci-release: $(RELX) clean-release rel/ci.relx.config rel/ci.relx.config.script rel/ci.sys.config rel/ci.vm.args
	$(RELX) --config rel/ci.relx.config -V 2 release --relname 'kazoo'
build-dist-release: $(RELX) clean-release rel/dist.relx.config rel/dist.relx.config.script rel/dist.vm.args rel/dist.sys.config
	$(RELX) --config rel/dist.relx.config -V 2 release --relname 'kazoo'
tar-release: $(RELX) rel/relx.config rel/relx.config.script rel/sys.config rel/vm.args
	$(RELX) --config rel/relx.config -V 2 release tar --relname 'kazoo'

## More ACTs at //github.com/erlware/relx/priv/templates/extended_bin
release: ACT ?= console # start | attach | stop | console | foreground
release: REL ?= kazoo_apps # kazoo_apps | ecallmgr | …
release: COOKIE ?= change_me
release:
	NODE_NAME="$(REL)" COOKIE="$(COOKIE)" $(ROOT)/scripts/dev/kazoo.sh $(ACT) "$$@"

install: compile build-release
	cp -a _rel/kazoo /opt

read-release-cookie: REL ?= kazoo_apps
read-release-cookie:
	@NODE_NAME='$(REL)' _rel/kazoo/bin/kazoo escript lib/kazoo_config-*/priv/read-cookie.escript "$$@"

fixture_shell: ERL_CRASH_DUMP = "$(ROOT)/$(shell date +%s)_ecallmgr_erl_crash.dump"
fixture_shell: ERL_LIBS = "$(ROOT)/deps:$(ROOT)/core:$(ROOT)/applications:$(shell echo $(ROOT)/deps/rabbitmq_erlang_client-*/deps)"
fixture_shell: NODE_NAME ?= fixturedb
fixture_shell:
	@ERL_CRASH_DUMP="$(ERL_CRASH_DUMP)" ERL_LIBS="$(ERL_LIBS)" KAZOO_CONFIG=$(ROOT)/rel/config-test.ini \
		erl -name '$(NODE_NAME)' -s reloader "$$@"

DIALYZER ?= dialyzer
DIALYZER += --statistics --no_native
PLT ?= .kazoo.plt

OTP_APPS ?= erts kernel stdlib crypto public_key ssl asn1 inets xmerl
$(PLT): DEPS_SRCS  ?= $(shell find $(ROOT)/deps -name src )
# $(PLT): CORE_EBINS ?= $(shell find $(ROOT)/core -name ebin)
$(PLT):
	@$(DIALYZER) --build_plt --output_plt $(PLT) \
	    --apps $(OTP_APPS) \
	    -r $(DEPS_SRCS)
	@for ebin in $(CORE_EBINS); do \
	    $(DIALYZER) --add_to_plt --plt $(PLT) --output_plt $(PLT) -r $$ebin; \
	done
build-plt: $(PLT)

dialyze-kazoo: TO_DIALYZE  = $(shell find $(ROOT)/applications $(ROOT)/core -name ebin)
dialyze-kazoo: dialyze
dialyze-apps:  TO_DIALYZE  = $(shell find $(ROOT)/applications -name ebin)
dialyze-apps: dialyze
dialyze-core:  TO_DIALYZE  = $(shell find $(ROOT)/core         -name ebin)
dialyze-core: dialyze-it
dialyze:       TO_DIALYZE ?= $(shell find $(ROOT)/applications -name ebin)
dialyze: dialyze-it

dialyze-it: $(PLT)
	ERL_LIBS=deps:core:applications $(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt $(TO_DIALYZE)

xref: TO_XREF ?= $(shell find $(ROOT)/applications $(ROOT)/core $(ROOT)/deps -name ebin)
xref:
	@$(ROOT)/scripts/check-xref.escript $(TO_XREF)

xref_release: TO_XREF = $(shell find $(ROOT)/_rel/kazoo/lib -name ebin)
xref_release:
	@$(ROOT)/scripts/check-xref.escript $(TO_XREF)

sup_completion: sup_completion_file = $(ROOT)/sup.bash
sup_completion:
	@$(if $(wildcard $(sup_completion_file)), rm $(sup_completion_file))
	@$(ROOT)/core/sup/priv/build-autocomplete.escript $(sup_completion_file) applications/ core/
	@echo SUP Bash completion file written at $(sup_completion_file)

$(ELVIS):
	wget 'https://github.com/inaka/elvis/releases/download/0.2.12/elvis' -O $@
	chmod +x $@

elvis: $(ELVIS)
	$(ELVIS) --config make/elvis.config rock

ci: clean compile xref build-plt diff sup_completion build-ci-release compile-test eunit elvis

diff: export TO_DIALYZE = $(shell git diff --name-only master... -- $(ROOT)/applications/ $(ROOT)/core/)
diff: dialyze-it

bump-copyright:
	@$(ROOT)/scripts/bump-copyright-year.sh $(shell find applications core -iname '*.erl' -or -iname '*.hrl')

FMT_SHA = 237604a566879bda46d55d9e74e3e66daf1b557a
$(FMT):
	wget -qO - 'https://codeload.github.com/fenollp/erlang-formatter/tar.gz/$(FMT_SHA)' | tar -vxz -C $(ROOT)/make/
	mv $(ROOT)/make/erlang-formatter-$(FMT_SHA) $(ROOT)/make/erlang-formatter

fmt-all: $(FMT)
	@$(FMT) $(shell find core applications scripts -name "*.erl" -or -name "*.hrl" -or -name "*.escript")

fmt: TO_FMT ?= $(shell git --no-pager diff --name-only HEAD origin/master -- "*.erl" "*.hrl" "*.escript")
fmt: $(FMT)
	@$(if $(TO_FMT), @$(FMT) $(TO_FMT))

clean-fmt:
	@$(if $(FMT), rm -rf $(shell dirname $(FMT)))

app_applications:
	ERL_LIBS=deps:core:applications $(ROOT)/scripts/apps_of_app.escript -a $(shell find applications -name *.app.src)

code_checks:
	@ERL_LIBS=deps/:core/:applications/ $(ROOT)/scripts/no_raw_json.escript
	@$(ROOT)/scripts/check-spelling.bash
	@$(ROOT)/scripts/kz_diaspora.bash
	@$(ROOT)/scripts/edocify.escript

apis:
	@ERL_LIBS=deps/:core/:applications/ $(ROOT)/scripts/generate-schemas.escript
	@$(ROOT)/scripts/format-json.sh $(shell find applications core -wholename '*/schemas/*.json')
	@ERL_LIBS=deps/:core/:applications/ $(ROOT)/scripts/generate-api-endpoints.escript
	@$(ROOT)/scripts/generate-doc-schemas.sh `grep -rl '#### Schema' core/ applications/ | grep -v '.erl'`
	@$(ROOT)/scripts/format-json.sh applications/crossbar/priv/api/swagger.json
	@$(ROOT)/scripts/format-json.sh $(shell find applications core -wholename '*/api/*.json')
	@ERL_LIBS=deps/:core/:applications/ $(ROOT)/scripts/generate-fs-headers-hrl.escript
	@ERL_LIBS=deps/:core/:applications/ $(ROOT)/scripts/generate-kzd-builders.escript

schemas:
	@ERL_LIBS=deps/:core/:applications/ $(ROOT)/scripts/generate-schemas.escript

DOCS_ROOT=$(ROOT)/doc/mkdocs
docs: docs-validate docs-report docs-setup docs-build

docs-validate:
	@$(ROOT)/scripts/check-scripts-readme.bash
	@$(ROOT)/scripts/empty_schema_descriptions.bash

docs-report:
	@$(ROOT)/scripts/reconcile_docs_to_index.bash

docs-setup:
	@$(ROOT)/scripts/validate_mkdocs.py
	@$(ROOT)/scripts/setup_docs.bash
	@mkdir -p $(DOCS_ROOT)/theme

docs-build:
	@$(MAKE) -C $(DOCS_ROOT) DOCS_ROOT=$(DOCS_ROOT) docs-build

docs-clean:
	@$(MAKE) -C $(DOCS_ROOT) DOCS_ROOT=$(DOCS_ROOT) clean

docs-serve: docs-setup docs-build
	@$(MAKE) -C $(DOCS_ROOT) DOCS_ROOT=$(DOCS_ROOT) docs-serve

fs-headers:
	@ERL_LIBS=deps/:core/:applications/ $(ROOT)/scripts/generate-fs-headers-hrl.escript

validate-swagger:
	@$(ROOT)/scripts/validate-swagger.sh

sdks:
	@$(ROOT)/scripts/make-swag.sh

validate-schemas:
	@$(ROOT)/scripts/validate-schemas.sh $(ROOT)/applications/crossbar/priv/couchdb/schemas

CHANGED := $(shell git --no-pager diff --name-only HEAD origin/master -- applications core scripts)
CHANGED_SWAGGER := $(shell git --no-pager diff --name-only HEAD origin/master -- applications/crossbar/priv/api/swagger.json)
PIP2 := $(shell { command -v pip || command -v pip2; } 2>/dev/null)

circle-pre:
ifneq ($(PIP2),)
## needs root access
	@echo $(CHANGED)
	@$(PIP2) install --upgrade pip
	@$(PIP2) install PyYAML mkdocs pyembed-markdown jsonschema
else
	$(error "pip/pip2 is not available, please install python2-pip package")
endif

circle-docs:
	@./scripts/state-of-docs.sh || true
	@$(ROOT)/scripts/state-of-edoc.escript
	@$(MAKE) apis
	@$(MAKE) docs

circle-codechecks:
	@./scripts/code_checks.bash $(CHANGED)
	@$(MAKE) code_checks
	@$(MAKE) app_applications
	@./scripts/validate-js.sh $(CHANGED)

circle-fmt:
	@$(MAKE) fmt
	@$(MAKE) elvis

circle-build:
	@$(MAKE) clean clean-deps deps kazoo xref sup_completion

circle-schemas:
	@$(MAKE) validate-schemas
	@$(if $(CHANGED_SWAGGER), $(MAKE) circle-swagger)

circle-swagger:
	@-$(MAKE) validate-swagger

circle-unstaged:
	echo Unstaged changes!
	git status --porcelain
	git --no-pager diff
	echo 'Maybe try `make apis` and see if that fixes anything ;)'
	exit 1

circle-dialyze: build-plt
circle-dialyze:
	@TO_DIALYZE="$(CHANGED)" $(MAKE) dialyze-it

circle-release:
	@$(MAKE) build-ci-release

circle: circle-pre circle-fmt circle-build circle-codechecks circle-docs circle-schemas circle-dialyze circle-release
	@$(if $(git status --porcelain | wc -l), $(MAKE) circle-unstaged)
