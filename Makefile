ROOT = $(shell cd "$(dirname '.')" && pwd -P)
RELX = $(ROOT)/deps/relx
ELVIS = $(ROOT)/deps/elvis
TAGS = $(ROOT)/TAGS
ERLANG_MK_COMMIT = d30dda39b08e6ed9e12b44533889eaf90aba86de

BASE_BRANCH := $(shell cat $(ROOT)/.base_branch)

## list files changed for more focused checks
ifeq ($(strip $(CHANGED)),)
	CHANGED := $(shell git --no-pager diff --name-only HEAD $(BASE_BRANCH) -- applications core scripts)
else
	CHANGED := $(CHANGED)
endif
CHANGED_SWAGGER := $(shell git --no-pager diff --name-only HEAD $(BASE_BRANCH) -- applications/crossbar/priv/api/swagger.json)

# You can override this when calling make, e.g. make JOBS=1
# to prevent parallel builds, or make JOBS="8".
JOBS ?= 1

.PHONY: kazoo deps core apps \
	apis \
	build-release build-ci-release tar-release release read-release-cookie \
	bump-copyright \
	changed changed_swagger \
	circle \
	clean clean-test clean-release clean-kazoo clean-core clean-apps \
	code_checks \
	coverage-report \
	dialyze dialyze-apps dialyze-core dialyze-kazoo dialyze-hard dialyze-changed \
	dialyze-it dialyze-it-hard dialyze-it-changed \
	diff \
	docs \
	elvis \
	fixture_shell \
	fs-headers \
	install \
	sdks \
	validate-js \
	validate-schemas \
	validate-swagger \
	xref xref_release

all: compile

changed:
	@echo "$(CHANGED)"

changed_swagger:
	@echo "$(CHANGED_SWAGGER)"

compile: ACTION = all
compile: deps kazoo

clean: clean-kazoo
	$(if $(wildcard *crash.dump), rm *crash.dump)
	$(if $(wildcard scripts/log/*), rm -rf scripts/log/*)
	$(if $(wildcard rel/dev-vm.args), rm rel/dev-vm.args)

clean-kazoo: clean-core clean-apps
clean-core:
	@$(MAKE) -j$(JOBS) -C core/ clean
clean-apps:
	@$(MAKE) -j$(JOBS) -C applications/ clean

clean-test: clean-test-core clean-test-apps
clean-test-core:
	@$(MAKE) -j$(JOBS) -C core/ clean-test
clean-test-apps:
	@$(MAKE) -j$(JOBS) -C applications/ clean-test

compile-test: ERLC_OPTS += +nowarn_missing_spec
compile-test: deps compile-test-core compile-test-apps
compile-test-core:
	@$(MAKE) -j$(JOBS) -C core/ compile-test
compile-test-apps:
	@$(MAKE) -j$(JOBS) -C applications/ compile-test

eunit: eunit-core eunit-apps

eunit-core:
	@$(MAKE) -j$(JOBS) -C core/ eunit
eunit-apps:
	@$(MAKE) -j$(JOBS) -C applications/ eunit

proper: ERLC_OPTS += -DPROPER
proper: proper-core proper-apps
proper-core:
	@$(MAKE) -j$(JOBS) -C core/ proper
proper-apps:
	@$(MAKE) -j$(JOBS) -C applications/ proper

test: ERLC_OPTS += -DPROPER
test: ERLC_OPTS += +nowarn_missing_spec
test: test-core test-apps
test-core:
	@$(MAKE) -j$(JOBS) -C core/ test
test-apps:
	@$(MAKE) -j$(JOBS) -C applications/ test

coverage-report:
	$(ROOT)/scripts/cover.escript

check: ERLC_OPTS += -DPROPER
check: compile-test eunit clean-kazoo kazoo

clean-deps: clean-deps-hash
	$(if $(wildcard deps/), rm -rf deps/)
	$(if $(wildcard .erlang.mk/), rm -r .erlang.mk/)

clean-deps-hash:
	$(if $(wildcard make/.deps.mk.*), rm make/.deps.mk.*)

.erlang.mk:
	wget 'https://raw.githubusercontent.com/ninenines/erlang.mk/2018.03.01/erlang.mk' -O $(ROOT)/erlang.mk
	@ERLANG_MK_COMMIT=$(ERLANG_MK_COMMIT) $(MAKE) -f erlang.mk erlang-mk

DEPS_HASH := $(shell md5sum make/deps.mk | cut -d' ' -f1)
DEPS_HASH_FILE := make/.deps.mk.$(DEPS_HASH)

deps: $(DEPS_HASH_FILE)

$(DEPS_HASH_FILE):
	@$(MAKE) clean-deps
	@$(MAKE) deps/Makefile
	@$(MAKE) -C deps/ all
	touch $(DEPS_HASH_FILE)

deps/Makefile: .erlang.mk clean-plt
	mkdir -p deps
	@$(MAKE) -f erlang.mk deps
	cp $(ROOT)/make/Makefile.deps deps/Makefile

core:
	@$(MAKE) -j$(JOBS) -C core/ all

apps: core
	@$(MAKE) -j$(JOBS) -C applications/ all

kazoo: deps apps $(TAGS)

tags: $(TAGS)

$(TAGS):
	ERL_LIBS=deps:core:applications ./scripts/tags.escript $(TAGS)

clean-tags:
	$(if $(wildcard $(TAGS)), rm $(TAGS))

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
EXCLUDE_DEPS = $(ROOT)/deps/erlang_localtime/ebin
$(PLT): DEPS_EBIN ?= $(filter-out $(EXCLUDE_DEPS),$(wildcard $(ROOT)/deps/*/ebin))
# $(PLT): CORE_EBINS ?= $(shell find $(ROOT)/core -name ebin)
$(PLT):
	@-$(DIALYZER) --build_plt --output_plt $(PLT) \
	     --apps $(OTP_APPS) \
	     -r $(DEPS_EBIN)
	@for ebin in $(CORE_EBINS); do \
	     $(DIALYZER) --add_to_plt --plt $(PLT) --output_plt $(PLT) -r $$ebin; \
	 done
build-plt: $(PLT)

clean-plt:
	@rm -f $(PLT)

dialyze-kazoo: TO_DIALYZE  = $(shell find $(ROOT)/applications $(ROOT)/core -name ebin)
dialyze-kazoo: dialyze
dialyze-apps:  TO_DIALYZE  = $(shell find $(ROOT)/applications -name ebin)
dialyze-apps: dialyze
dialyze-core:  TO_DIALYZE  = $(shell find $(ROOT)/core         -name ebin)
dialyze-core: dialyze-it
dialyze:       TO_DIALYZE ?= $(shell find $(ROOT)/applications -name ebin)
dialyze: dialyze-it

dialyze-changed: TO_DIALYZE = $(CHANGED)
dialyze-changed: dialyze-it-changed

dialyze-hard: TO_DIALYZE = $(CHANGED)
dialyze-hard: dialyze-it-hard

dialyze-it: $(PLT)
	@ERL_LIBS=deps:core:applications $(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt $(filter %.beam %.erl %/ebin,$(TO_DIALYZE))

dialyze-it-hard: $(PLT)
	@ERL_LIBS=deps:core:applications $(if $(DEBUG),time -v) $(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt --hard $(filter %.beam %.erl %/ebin,$(TO_DIALYZE))

dialyze-it-changed: $(PLT)
ifeq ($(strip $(filter %.beam %.erl %/ebin,$(TO_DIALYZE))),)
	@echo "no erlang changes to dialyze"
else
	@ERL_LIBS=deps:core:applications $(if $(DEBUG),time -v) $(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt --bulk $(filter %.beam %.erl %/ebin,$(TO_DIALYZE))
endif

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

diff: export TO_DIALYZE = $(shell git diff --name-only $(BASE_BRANCH)... -- $(ROOT)/applications/ $(ROOT)/core/)
diff: dialyze-it

bump-copyright:
	@$(ROOT)/scripts/bump-copyright-year.sh $(shell find applications core -iname '*.erl' -or -iname '*.hrl')

app_applications:
	ERL_LIBS=deps:core:applications $(ROOT)/scripts/apps_of_app.escript -a $(shell find applications -name *.app.src)

code_checks:
	@$(ROOT)/scripts/code_checks.bash $(CHANGED)
	@ERL_LIBS=deps/:core/:applications/ $(ROOT)/scripts/no_raw_json.escript
	@$(ROOT)/scripts/check-spelling.bash
	@$(ROOT)/scripts/kz_diaspora.bash
	@$(ROOT)/scripts/edocify.escript
	@$(ROOT)/scripts/kzd_module_check.bash
	@$(ROOT)/scripts/check-loglines.bash

apis:
	@ERL_LIBS=deps/:core/:applications/ $(ROOT)/scripts/generate-schemas.escript
	@$(ROOT)/scripts/format-json.sh $(shell find applications core -wholename '*/schemas/*.json')
	@ERL_LIBS=deps/:core/:applications/ $(ROOT)/scripts/generate-api-endpoints.escript
	@$(ROOT)/scripts/generate-doc-schemas.sh `egrep -rl '(#+) Schema' core/ applications/ | grep -v '.[h|e]rl'`
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
	@$(ROOT)/scripts/check-ref-docs.bash

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

validate-js:
	@./scripts/validate-js.sh $(find {core,applications}/*/priv/**/* -name *.json)

sdks:
	@$(ROOT)/scripts/make-swag.sh

validate-schemas:
	@$(ROOT)/scripts/validate-schemas.sh $(ROOT)/applications/crossbar/priv/couchdb/schemas

include make/splchk.mk
include make/ci.mk
include make/fmt.mk

circle: ci
