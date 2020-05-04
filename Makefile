ROOT = $(shell cd "$(dirname '.')" && pwd -P)

DEPS_DIR = $(ROOT)/deps
CORE_DIR = $(ROOT)/core
APPS_DIR = $(ROOT)/applications

RELX = $(DEPS_DIR)/relx
ELVIS = $(DEPS_DIR)/elvis
TAGS = $(ROOT)/TAGS
ERLANG_LS = $(ROOT)/erlang_ls.config
PLT = $(ROOT)/.kazoo.plt

ERLANG_MK = $(ROOT)/erlang.mk
ERLANG_MK_COMMIT = 89f2eca925b3f19b2409f9d0e71cf8108e5bd5eb
DOT_ERLANG_MK = $(ROOT)/.erlang.mk

BASE_BRANCH := $(shell cat $(ROOT)/.base_branch)

DEPS_HASH := $(shell md5sum $(ROOT)/make/deps.mk | cut -d' ' -f1)
DEPS_HASH_FILE := $(ROOT)/make/.deps.mk.$(DEPS_HASH)

APPS_HASH := $(shell md5sum $(ROOT)/make/apps.mk | cut -d' ' -f1)
APPS_HASH_FILE := $(ROOT)/make/.apps.mk.$(APPS_HASH)

CORE_HASH := $(shell md5sum $(ROOT)/make/Makefile.core | cut -d' ' -f1)
CORE_HASH_FILE := $(ROOT)/make/.core.mk.$(CORE_HASH)

APPS := $(dir $(wildcard $(APPS_DIR)/*/.git))
CORE := $(wildcard $(CORE_DIR))

## list files changed for more focused checks
ifeq ($(strip $(CHANGED)),)
	CHANGED := $(strip $(shell $(ROOT)/scripts/check-changed.bash $(ROOT) $(CORE) $(APPS)))
else
	CHANGED := $(CHANGED)
endif

CHANGED_SWAGGER ?= $(shell git -C $(APPS_DIR)/crossbar --no-pager diff --name-only HEAD $(BASE_BRANCH) -- priv/api/swagger.json)
CHANGED_ERL=$(filter %.hrl %.erl %.escript,$(CHANGED))
CHANGED_JSON=$(filter %.json,$(CHANGED))
CHANGED_YML=$(filter %.yml,$(CHANGED))

# You can override this when calling make, e.g. make JOBS=1
# to prevent parallel builds, or make JOBS="8".
JOBS ?= 1

.PHONY: all
all: prerequisites compile

.PHONY: changed
changed:
	@echo "changed: $(CHANGED)"
	@echo "changed ERL: $(CHANGED_ERL)"
	@echo "changed JSON: $(CHANGED_JSON)"
	@echo "changed YML: $(CHANGED_YML)"

.PHONY: changed_swagger
changed_swagger:
	@echo "$(CHANGED_SWAGGER)"

prerequisites: make-dependency-check

make-dependency-check:
	$(ROOT)/scripts/make-prerequisite.sh

.PHONY: compile-lean compile-lean-core compile-lean-apps
compile-lean: ACTION = compile-lean
compile-lean: deps compile-lean-core compile-lean-apps
compile-lean-core:
	@$(MAKE) -j$(JOBS) -C core/ compile-lean
compile-lean-apps:
	@$(MAKE) -j$(JOBS) -C applications/ compile-lean

.PHONY: compile
compile: ACTION = all
compile: deps kazoo

.PHONY: sparkly-clean
sparkly-clean: clean-apps clean-kazoo clean-release clean-deps
	@(rm -rf $(APPS_DIR) $(CORE_DIR))

.PHONY: clean
clean: clean-core clean-apps
	$(if $(wildcard *crash.dump), rm *crash.dump)
	$(if $(wildcard scripts/log/*), rm -rf scripts/log/*)
	$(if $(wildcard rel/dev-vm.args), rm rel/dev-vm.args)

.PHONY: clean-kazoo
clean-kazoo:
	@$(ls -d $(APPS_DIR)/* | xargs rm -rf)
	@$(rm -rf $(CORE_DIR))
	@$(if $(wildcard $(CORE_HASH_FILE)), rm -rf $(CORE_HASH_FILE))
	@$(if $(wildcard $(APPS_HASH_FILE)), rm -rf $(APPS_HASH_FILE))

.PHONY: clean-core
clean-core:
	@$(if $(wildcard $(CORE_DIR)),$(MAKE) -j$(JOBS) -C $(CORE_DIR) clean)
	@$(if $(wildcard $(CORE_HASH_FILE)),rm $(CORE_HASH_FILE))

.PHONY: clean-test
clean-test: clean-test-core clean-test-apps

.PHONY: clean-test-core
clean-test-core:
	@$(MAKE) -j$(JOBS) -C $(CORE_DIR) clean-test

.PHONY: clean-test-apps
clean-test-apps:
	@$(MAKE) -j$(JOBS) -C $(APPS_DIR) clean-test

.PHONY: compile-proper
compile-proper: ERLC_OPTS += -DPROPER
compile-proper: compile-test

.PHONY: compile-test
compile-test: ERLC_OPTS += +nowarn_missing_spec
compile-test: compile-test-core compile-test-apps

.PHONY: compile-test-core
compile-test-core: deps $(CORE_HASH_FILE)
	@$(MAKE) -j$(JOBS) -C $(CORE_DIR) compile-test-direct

.PHONY: compile-test-apps
compile-test-apps: deps fetch-apps
	@$(MAKE) -j$(JOBS) -C $(APPS_DIR) compile-test-direct

.PHONY: eunit
eunit: eunit-core eunit-apps

.PHONY: eunit-core
eunit-core: deps $(CORE_HASH_FILE)
	@$(MAKE) -j$(JOBS) -C $(CORE_DIR) eunit

.PHONY: eunit-apps
eunit-apps: deps fetch-apps
	@$(MAKE) -j$(JOBS) -C $(APPS_DIR) eunit

.PHONY: proper
proper: ERLC_OPTS += -DPROPER
proper: proper-core proper-apps

.PHONY: proper-core
proper-core: deps $(CORE_HASH_FILE)
	@$(MAKE) -j$(JOBS) -C $(CORE_DIR) proper

.PHONY: proper-apps
proper-apps: deps fetch-apps
	@$(MAKE) -j$(JOBS) -C $(APPS_DIR) proper

.PHONY: test
test: ERLC_OPTS += -DPROPER
test: ERLC_OPTS += +nowarn_missing_spec
test: test-core test-apps

.PHONY: test-core
test-core: deps $(CORE_HASH_FILE)
	@$(MAKE) -j$(JOBS) -C $(CORE_DIR) test

.PHONY: test-apps
test-apps: deps fetch-apps
	@$(MAKE) -j$(JOBS) -C $(APPS_DIR) test

.PHONY: coverage-report
coverage-report:
	$(ROOT)/scripts/cover.escript

.PHONY: check
check: ERLC_OPTS += -DPROPER
check: compile-test eunit clean-kazoo kazoo

.PHONY: clean-deps
clean-deps: clean-deps-hash
	@$(if $(wildcard $(DEPS_DIR)), rm -rf $(DEPS_DIR))
	@$(if $(wildcard $(DOT_ERLANG_MK)), rm -rf $(DOT_ERLANG_MK))
	@$(if $(wildcard $(ERLANG_MK)), rm -r $(ERLANG_MK))
	@echo "Cleaned deps-related files"

.PHONY: clean-deps-hash
clean-deps-hash:
	$(if $(wildcard $(ROOT)/make/.deps.mk.*), rm $(ROOT)/make/.deps.mk.*)

.PHONY=dot_erlang_mk
dot_erlang_mk: $(DOT_ERLANG_MK)

$(DOT_ERLANG_MK): $(ERLANG_MK)
	@ERLANG_MK_COMMIT=$(ERLANG_MK_COMMIT) $(MAKE) -f $(ERLANG_MK) erlang.mk

$(ERLANG_MK):
	@wget 'https://raw.githubusercontent.com/ninenines/erlang.mk/2018.03.01/erlang.mk' -O $(ERLANG_MK)

.PHONY: deps
deps: $(DEPS_HASH_FILE)

$(DEPS_HASH_FILE):
	@$(MAKE) clean-deps
	@$(MAKE) $(DEPS_DIR)/Makefile
	@$(MAKE) -C $(DEPS_DIR)/ all
	touch $(DEPS_HASH_FILE)

$(DEPS_DIR)/Makefile: $(DOT_ERLANG_MK) clean-plt
	mkdir -p deps
	@$(MAKE) -f $(ERLANG_MK) deps
	cp $(ROOT)/make/Makefile.deps $(DEPS_DIR)/Makefile

# Target: core
# 1. make sure the 'deps' target is built
# 2. make sure the core repo has been fetched (using the hash of the make/Makefile.core as a check)
# Once satisfied, compile all the dirs under core/
.PHONY: core
core: deps $(CORE_HASH_FILE)
	@$(MAKE) -j$(JOBS) -C $(CORE_DIR) all

# Target: fetch-core
# Alias for $(CORE_DIR)Makefile to fetch the core apps
fetch-core: $(CORE_DIR)/Makefile

# Target: core hash file
# 1. Make sure erlang.mk is setup
# 2. Make sure core/Makefile exists
# Once satisfied, create the core hash file
$(CORE_HASH_FILE): $(CORE_DIR)/Makefile
	@touch $(CORE_HASH_FILE)

# Target: core/Makefile
# Using make/Makefile.core, use erlang.mk to fetch the kazoo-core repo (which includes the Makefile needed)
$(CORE_DIR)/Makefile: .erlang.mk
	@NO_AUTOPATCH_ERLANG_MK=1 $(MAKE) -f $(ROOT)/make/Makefile.core fetch-deps

# Target: apps
# Since kazoo-core comes with a Makefile for building all apps under it, erlang.mk is used to fetch deps
# Since each app is fetched independently, use erlang.mk to call the Makefile in each fetched app
# 1. Make sure core is built
# 2. Check the apps hash file (hash of make/apps.mk)
# Once satisfied, use erlang.mk to fetch the apps (via the erlang.mk deps target), and compile all the apps
.PHONY: apps
apps: core fetch-apps
	@$(MAKE) -j$(JOBS) -C $(APPS_DIR) all

fetch-apps: $(APPS_HASH_FILE) $(APPS_DIR)/Makefile
	@NO_AUTOPATCH_ERLANG_MK=1 $(MAKE) -f $(ROOT)/make/Makefile.apps -C $(APPS_DIR) fetch-deps

# Target: apps hash file
# 1. Make sure elrang.mk is setup
# Once satisfied, create the applications directory and the apps hash file
$(APPS_HASH_FILE): .erlang.mk
	@touch $(APPS_HASH_FILE)

$(APPS_DIR)/Makefile:
	@$(shell mkdir -p $(APPS_DIR))
	@cp $(ROOT)/make/Makefile.applications $(APPS_DIR)/Makefile

.PHONY: clean-apps
clean-apps:
	@$(if $(wildcard $(APPS_DIR)/Makefile),$(MAKE) -j$(JOBS) -C $(APPS_DIR) clean)
	@$(if $(wildcard $(APPS_HASH_FILE)),rm $(APPS_HASH_FILE))

.PHONY: kazoo
kazoo: deps apps $(TAGS)

.PHONY: tags
tags: $(TAGS)

$(TAGS):
	ERL_LIBS=$(DEPS_DIR):$(CORE_DIR):$(APPS_DIR) ./scripts/tags.escript $(TAGS)

.PHONY: clean-tags
clean-tags:
	$(if $(wildcard $(TAGS)), rm $(TAGS))

erlang-ls: $(ERLANG_LS)

$(ERLANG_LS):
	@touch $(ERLANG_LS)
	@echo "plt_path: $(PLT)" >> $(ERLANG_LS)
	@echo "apps_dirs: " >> $(ERLANG_LS)
	@echo " - $(ROOT)/core/*" >> $(ERLANG_LS)
	@echo " - $(ROOT)/applications/*" >> $(ERLANG_LS)
	@echo "deps_dirs: " >> $(ERLANG_LS)
	@echo " - $(ROOT)/deps/*" >> $(ERLANG_LS)
	@echo "generated $(ERLANG_LS)"

clean-erlang-ls:
	@rm $(ERLANG_LS)

$(RELX):
	wget 'https://erlang.mk/res/relx-v3.27.0' -O $@
	chmod +x $@

.PHONY: clean-release
clean-release:
	$(if $(wildcard _rel/), rm -r _rel/)

.PHONY: build-release
build-release: $(RELX) clean-release rel/relx.config rel/relx.config.script rel/sys.config rel/vm.args
	$(RELX) --config rel/relx.config -V 2 release --relname 'kazoo'

.PHONY: build-dev-release
build-dev-release: $(RELX) clean-release rel/dev.relx.config rel/dev.relx.config.script rel/dev.vm.args rel/dev.sys.config
	$(RELX) --dev-mode true --config rel/dev.relx.config -V 2 release --relname 'kazoo'

.PHONY: build-ci-release
build-ci-release: $(RELX) clean-release rel/ci.relx.config rel/ci.relx.config.script rel/ci.sys.config rel/ci.vm.args
	$(RELX) --config rel/ci.relx.config -V 2 release --relname 'kazoo'

.PHONY: build-dist-release
build-dist-release: $(RELX) clean-release rel/dist.relx.config rel/dist.relx.config.script rel/dist.vm.args rel/dist.sys.config
	$(RELX) --config rel/dist.relx.config -V 2 release --relname 'kazoo'

.PHONY: tar-release
tar-release: $(RELX) rel/relx.config rel/relx.config.script rel/sys.config rel/vm.args
	$(RELX) --config rel/relx.config -V 2 release tar --relname 'kazoo'

## More ACTs at //github.com/erlware/relx/priv/templates/extended_bin
.PHONY: release
release: ACT ?= console # start | attach | stop | console | foreground
release: REL ?= kazoo_apps # kazoo_apps | ecallmgr | …
release: COOKIE ?= change_me
release:
	NODE_NAME="$(REL)" COOKIE="$(COOKIE)" $(ROOT)/scripts/dev/kazoo.sh $(ACT) "$$@"

.PHONY: install
install: compile build-release
	cp -a _rel/kazoo /opt

.PHONY: read-release-cookie
read-release-cookie: REL ?= kazoo_apps
read-release-cookie:
	@NODE_NAME='$(REL)' _rel/kazoo/bin/kazoo escript lib/kazoo_config-*/priv/read-cookie.escript "$$@"

.PHONY: fixture_shell
fixture_shell: ERL_CRASH_DUMP = "$(ROOT)/$(shell date +%s)_ecallmgr_erl_crash.dump"
fixture_shell: ERL_LIBS = "$(DEPS_DIR):$(CORE_DIR):$(APPS_DIR):$(shell echo $(DEPS_DIR)/rabbitmq_erlang_client-*/deps)"
fixture_shell: NODE_NAME ?= fixturedb
fixture_shell:
	@ERL_CRASH_DUMP="$(ERL_CRASH_DUMP)" ERL_LIBS="$(ERL_LIBS)" KAZOO_CONFIG=$(ROOT)/rel/config-test.ini \
		erl -name '$(NODE_NAME)' -s reloader "$$@"

DIALYZER ?= dialyzer
DIALYZER += --statistics --no_native

OTP_APPS ?= erts kernel stdlib crypto public_key ssl asn1 inets xmerl


EXCLUDE_DEPS = $(DEPS_DIR)/erlang_localtime/ebin
$(PLT): DEPS_EBIN ?= $(filter-out $(EXCLUDE_DEPS),$(wildcard $(DEPS_DIR)/*/ebin))
# $(PLT): CORE_EBINS ?= $(shell find $(CORE_DIR) -name ebin)
$(PLT):
	@-$(DIALYZER) --build_plt --output_plt $(PLT) \
	     --apps $(OTP_APPS) \
	     -r $(DEPS_EBIN)
	@for ebin in $(CORE_EBINS); do \
	     $(DIALYZER) --add_to_plt --plt $(PLT) --output_plt $(PLT) -r $$ebin; \
	 done

.PHONY: build-plt
build-plt: $(PLT)

.PHONY: clean-plt
clean-plt:
	@rm -f $(PLT)

.PHONY: dialyze-kazoo
dialyze-kazoo: TO_DIALYZE  = $(shell find $(APPS_DIR) $(CORE_DIR) -name ebin)
dialyze-kazoo: dialyze

.PHONY: dialzye-apps
dialyze-apps:  TO_DIALYZE  = $(shell find $(APPS_DIR) -name ebin)
dialyze-apps: dialyze

.PHONY: dialyze-core
dialyze-core:  TO_DIALYZE  = $(shell find $(CORE_DIR)         -name ebin)
dialyze-core: dialyze-it

.PHONY: dialyze
dialyze:       TO_DIALYZE ?= $(shell find $(APPS_DIR) -name ebin)
dialyze: dialyze-it

.PHONY: dialyze-changed
dialyze-changed: TO_DIALYZE = $(strip $(filter %.beam %.erl %/ebin,$(CHANGED)))
dialyze-changed: dialyze-it-changed

.PHONY: dialyze-hard
dialyze-hard: TO_DIALYZE = $(CHANGED)
dialyze-hard: dialyze-it-hard

.PHONY: dialyze-id
dialyze-it: $(PLT)
	@echo ":: dialyzing"
	@ERL_LIBS=$(DEPS_DIR):$(CORE_DIR):$(APPS_DIR) $(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt $(filter %.beam %.erl %/ebin,$(TO_DIALYZE)) && echo "dialyzer is happy!"

.PHONY: dialyze-it-hard
dialyze-it-hard: $(PLT)
	@echo ":: dialyzing hard"
	@ERL_LIBS=$(DEPS_DIR):$(CORE_DIR):$(APPS_DIR) $(if $(DEBUG),time -v) $(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt --hard $(filter %.beam %.erl %/ebin,$(TO_DIALYZE)) && echo "dialyzer is happy!"

.PHONY: dialyze-it-changed
dialyze-it-changed: $(PLT)
	@if [ -n "$(TO_DIALYZE)" ]; then \
		echo "dialyzing changes against $(BASE_BRANCH)" ; \
		ERL_LIBS=deps:core:applications $(if $(DEBUG),time -v) $(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt --bulk $(TO_DIALYZE) && echo "dialyzer is happy!"; \
	else \
		echo "no erlang changes to dialyze"; \
	fi

.PHONY: xref
xref: TO_XREF ?= $(shell find $(APPS_DIR) $(CORE_DIR) $(DEPS_DIR) -name ebin)
xref:
	@$(ROOT)/scripts/check-xref.escript $(TO_XREF)

.PHONY: xref_release
xref_release: TO_XREF = $(shell find $(ROOT)/_rel/kazoo/lib -name ebin)
xref_release:
	@$(ROOT)/scripts/check-xref.escript $(TO_XREF)

.PHONY: sup_completion
sup_completion: sup_completion_file = $(ROOT)/sup.bash
sup_completion:
	@$(if $(wildcard $(sup_completion_file)), rm $(sup_completion_file))
	@$(CORE_DIR)/sup/priv/build-autocomplete.escript $(sup_completion_file) $(APPS_DIR) $(CORE_DIR)
	@echo SUP Bash completion file written at $(sup_completion_file)

$(ELVIS):
	wget 'https://github.com/inaka/elvis/releases/download/0.2.12/elvis' -O $@
	chmod +x $@

.PHONY: elvis
elvis: $(ELVIS)
	$(ELVIS) --config $(ROOT)/make/elvis.config rock

.PHONY: ci
ci: clean compile xref build-plt diff sup_completion build-ci-release compile-test eunit elvis

.PHONY: diff
diff: export TO_DIALYZE = $(CHANGED)
diff: dialyze-it

.PHONY: bump-copyright
bump-copyright:
	@$(ROOT)/scripts/bump-copyright-year.py $(shell find $(APPS_DIR) $(CORE_DIR) -name '*.erl')

.PHONY: app_applications
app_applications:
	ERL_LIBS=$(DEPS_DIR):$(CORE_DIR):$(APPS_DIR) $(ROOT)/scripts/apps_of_app.escript -a $(shell find $(APPS_DIR) -name *.app.src)

.PHONY: code_checks
code_checks:
	@printf ":: Check for copyright year\n\n"
	@$(ROOT)/scripts/bump-copyright-year.py $(CHANGED_ERL)
	@printf "\n:: Check code\n\n"
	@$(ROOT)/scripts/code_checks.bash $(CHANGED_ERL)
	@printf "\n:: Check for raw JSON usage\n\n"
	@ERL_LIBS=$(DEPS_DIR):$(CORE_DIR):$(APPS_DIR) $(ROOT)/scripts/no_raw_json.escript $(CHANGED_ERL)
	@printf "\n:: Check for spelling\n\n"
	@$(ROOT)/scripts/check-spelling.bash
	@printf "\n:: Check for Kazoo diaspora\n\n"
	@$(ROOT)/scripts/kz_diaspora.bash
	@printf "\n:: Check for Edoc\n\n"
	@$(ROOT)/scripts/edocify.escript
	@printf "\n:: Check for Kazoo document accessors\n\n"
	@$(ROOT)/scripts/kzd_module_check.bash
	@printf "\n:: Check for proper log message usage\n\n"
	@$(ROOT)/scripts/check-loglines.bash
	@printf "\n:: Check for Erlang 21 new stacktrace syntax\n\n"
	@$(ROOT)/scripts/check-stacktrace.py $(CHANGED_ERL)

.PHONY: raw_json_check
raw_json_check:
	@ERL_LIBS=$(DEPS_DIR):$(CORE_DIR):$(APPS_DIR) $(ROOT)/scripts/no_raw_json.escript

.PHONY: check_stacktrace
check_stacktrace:
	@$(ROOT)/scripts/check-stacktrace.py $(shell grep -rl "get_stacktrace" scripts $(APPS_DIR) $(CORE_DIR) --include "*.[e|h]rl" --exclude "kz_types.hrl")

## FIXME: `format-couchdb-view.py` command is a copy-paste from fmt.mk
## Adding this format couchdb view target to circleci steps for every app is painful
## also this formatting is better to be done before validate-js ci step to make sure
## the view is still in correct shape
apis: schemas
	@ERL_LIBS=$(DEPS_DIR):$(CORE_DIR):$(APPS_DIR) $(ROOT)/scripts/generate-api-endpoints.escript
	@$(ROOT)/scripts/generate-doc-schemas.py `egrep -rl '(#+) Schema' core/ applications/ | grep -v '.[h|e]rl'`
	@$(ROOT)/scripts/format-json.py $(APPS_DIR)/crossbar/priv/api/swagger.json
	@$(ROOT)/scripts/format-json.py $(shell find $(APPS_DIR) $(CORE_DIR) -wholename '*/api/*.json')
	@ERL_LIBS=$(DEPS_DIR):$(CORE_DIR):$(APPS_DIR) $(ROOT)/scripts/generate-fs-headers-hrl.escript
	@ERL_LIBS=$(DEPS_DIR):$(CORE_DIR):$(APPS_DIR) $(ROOT)/scripts/generate-kzd-builders.escript
	@$(ROOT)/scripts/format-couchdb-views.py $(shell find $(CORE_DIR)/kazoo_apps/priv/couchdb/account -name '*.json')
	@$(ROOT)/scripts/format-couchdb-views.py $(shell find $(APPS_DIR) $(CORE_DIR) -wholename '*/couchdb/views/*.json')

.PHONY: schemas
schemas:
	@ERL_LIBS=$(DEPS_DIR):$(CORE_DIR):$(APPS_DIR) $(ROOT)/scripts/generate-schemas.escript $(CHANGED_ERL)
	@$(ROOT)/scripts/format-json.py $(shell find $(APPS_DIR) $(CORE_DIR) -wholename '*/schemas/*.json')

DOCS_ROOT=$(ROOT)/doc/mkdocs
.PHONY: docs
docs: docs-validate docs-report docs-setup docs-build

.PHONY: admonitions
admonitions:
	@ERL_LIBS=$(DEPS_DIR):$(CORE_DIR):$(APPS_DIR) $(ROOT)/scripts/check-admonitions.escript $(shell grep -rlE '^!!! ' scripts $(APPS_DIR) $(CORE_DIR) doc)

.PHONY: docs-validate
docs-validate:
	@$(ROOT)/scripts/check-scripts-readme.bash
	@$(ROOT)/scripts/empty_schema_descriptions.bash
	@$(ROOT)/scripts/check-ref-docs.bash
	@ERL_LIBS=$(DEPS_DIR):$(CORE_DIR):$(APPS_DIR) $(ROOT)/scripts/check-admonitions.escript $(CHANGED)

.PHONY: docs-report
docs-report:
	@$(ROOT)/scripts/reconcile_docs_to_index.bash

.PHONY: docs-setup
docs-setup:
	@$(ROOT)/scripts/validate_mkdocs.py
	@$(ROOT)/scripts/setup_docs.bash

.PHONY: docs-build
docs-build:
	@$(MAKE) -C $(DOCS_ROOT) DOCS_ROOT=$(DOCS_ROOT) docs-build

.PHONY: docs-clean
docs-clean:
	@$(MAKE) -C $(DOCS_ROOT) DOCS_ROOT=$(DOCS_ROOT) clean

.PHONY: docs-serve
docs-serve: docs-setup docs-build
	@$(MAKE) -C $(DOCS_ROOT) YML=$(YML) DOCS_ROOT=$(DOCS_ROOT) docs-serve

.PHONY: fs-headers
fs-headers:
	@ERL_LIBS=$(DEPS_DIR):$(CORE_DIR):$(APPS_DIR) $(ROOT)/scripts/generate-fs-headers-hrl.escript

.PHONY: validate-swagger
validate-swagger:
	@$(ROOT)/scripts/validate-swagger.py

.PHONY: validate-js
validate-js:
	@$(ROOT)/scripts/validate-js.py $(CHANGED_JSON)

.PHONY: sdks
sdks:
	@$(ROOT)/scripts/make-swag.sh

.PHONY: validate-schemas
validate-schemas:
	@$(ROOT)/scripts/validate-schemas.py $(APPS_DIR)/crossbar/priv/couchdb/schemas

include $(ROOT)/make/splchk.mk
include $(ROOT)/make/ci.mk
include $(ROOT)/make/fmt.mk
include $(ROOT)/make/pest.mk

circle: ci
