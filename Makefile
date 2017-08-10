ROOT = $(shell readlink -f .)
RELX = $(ROOT)/deps/relx
ELVIS = $(ROOT)/deps/elvis
FMT = $(ROOT)/make/erlang-formatter-master/fmt.sh

KAZOODIRS = core/Makefile applications/Makefile

.PHONY: $(KAZOODIRS) deps core apps xref xref_release dialyze dialyze-it dialyze-apps dialyze-core dialyze-kazoo clean clean-test clean-release build-release build-ci-release tar-release release read-release-cookie elvis install ci diff fmt bump-copyright apis validate-swagger sdks coverage-report fs-headers docs validate-schemas

all: compile rel/dev-vm.args

compile: ACTION = all
compile: deps $(KAZOODIRS)

$(KAZOODIRS):
	$(MAKE) -C $(@D) $(ACTION)

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

.erlang.mk:
	wget 'https://raw.githubusercontent.com/ninenines/erlang.mk/2017.07.06/erlang.mk' -O $(ROOT)/erlang.mk

deps: deps/Makefile
	$(MAKE) -C deps/ all
deps/Makefile: .erlang.mk
	mkdir -p deps
	$(MAKE) -f erlang.mk deps
	cp $(ROOT)/make/Makefile.deps deps/Makefile

core:
	$(MAKE) -C core/ all

apps:
	$(MAKE) -C applications/ all

kazoo: core apps


$(RELX):
	wget 'https://github.com/erlware/relx/releases/download/v3.23.0/relx' -O $@
	chmod +x $@

clean-release:
	$(if $(wildcard _rel/), rm -r _rel/)
	$(if $(wildcard rel/relx.config rel/vm.args rel/dev-vm.args), \
	  rm $(wildcard rel/relx.config rel/vm.args rel/dev-vm.args)  )

build-release: $(RELX) clean-release rel/relx.config rel/vm.args
	$(RELX) --config rel/relx.config -V 2 release --relname 'kazoo'
	patch _rel/'kazoo'/bin/'kazoo' -i rel/relx.patch
build-all-release: build-release
	for path in applications/*/; do \
	  app=$$(echo $$path | cut -d/ -f2) ; \
	  if [ $$app = 'skel' ]; then continue; fi ; \
	  $(RELX) --config rel/relx.config -V 2 release --relname $$app ; \
	  patch _rel/$$app/bin/$$app -i rel/relx.patch ; \
	done
build-dev-release: $(RELX) clean-release rel/relx.config-dev rel/vm.args
	$(RELX) --dev-mode true --config rel/relx.config -V 2 release --relname 'kazoo'
	patch _rel/kazoo/bin/kazoo -i rel/relx.patch
build-ci-release: $(RELX) clean-release rel/relx.config rel/vm.args
	$(RELX) --config rel/relx.config -V 2 release --relname 'kazoo' --sys_config rel/ci-sys.config
	patch _rel/kazoo/bin/kazoo -i rel/relx.patch
tar-release: $(RELX) rel/relx.config rel/vm.args
	$(RELX) --config rel/relx.config -V 2 release tar --relname 'kazoo'
rel/relx.config: rel/relx.config.src
	$(ROOT)/scripts/src2any.escript $<
rel/relx.config-dev: export KAZOO_DEV='true'
rel/relx.config-dev: rel/relx.config.src
	$(ROOT)/scripts/src2any.escript $<

rel/dev-vm.args: rel/args  # Used by scripts/dev-start-*.sh
	cp $^ $@
rel/vm.args: rel/args rel/dev-vm.args
	( echo '-setcookie $${COOKIE}'; cat $<; echo '-name $${NODE_NAME}' ) > $@

## More ACTs at //github.com/erlware/relx/priv/templates/extended_bin
release: ACT ?= console # start | attach | stop | console | foreground
release: REL ?= kazoo_apps # kazoo_apps | ecallmgr | …
ifneq ($(findstring kazoo_apps,$(REL)),kazoo_apps)
release: export KAZOO_APPS = 'ecallmgr'
endif
release:
	@NODE_NAME='$(REL)' COOKIE='change_me' $(ROOT)/scripts/dev/kazoo.sh $(ACT) "$$@"

install: compile build-release
	cp -a _rel/kazoo /opt

read-release-cookie: REL ?= kazoo_apps
read-release-cookie:
	@NODE_NAME='$(REL)' _rel/kazoo/bin/kazoo escript lib/kazoo_config-*/priv/read-cookie.escript "$$@"

DIALYZER ?= dialyzer
DIZLYZER += --statistics --no_native
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
dialyze-core: dialyze
dialyze:       TO_DIALYZE ?= $(shell find $(ROOT)/applications -name ebin)
dialyze: dialyze-it

dialyze-it: $(PLT)
	@if [ -n "$(TO_DIALYZE)" ]; then $(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt $(TO_DIALYZE); fi;

xref: TO_XREF ?= $(shell find $(ROOT)/applications $(ROOT)/core $(ROOT)/deps -name ebin)
xref:
	@$(ROOT)/scripts/check-xref.escript $(TO_XREF)

xref_release: TO_XREF = $(shell find $(ROOT)/_rel/kazoo/lib -name ebin)
xref_release:
	@$(ROOT)/scripts/check-xref.escript $(TO_XREF)


sup_completion: sup_completion_file = $(ROOT)/sup.bash
sup_completion: kazoo
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

$(FMT):
	wget -qO - 'https://codeload.github.com/fenollp/erlang-formatter/tar.gz/master' | tar xz -C $(ROOT)/make/

fmt: TO_FMT ?= $(shell find applications core -iname '*.erl' -or -iname '*.hrl' -or -iname '*.app.src')
fmt: $(FMT)
	@$(FMT) $(TO_FMT)

code_checks:
	@ERL_LIBS=deps/:core/:applications/ $(ROOT)/scripts/no_raw_json.escript
	@$(ROOT)/scripts/kz_diaspora.bash

apis:
	@ERL_LIBS=deps/:core/:applications/ $(ROOT)/scripts/generate-schemas.escript
	@$(ROOT)/scripts/format-json.sh applications/crossbar/priv/couchdb/schemas/*.json
	@ERL_LIBS=deps/:core/:applications/ $(ROOT)/scripts/generate-api-endpoints.escript
	@$(ROOT)/scripts/generate-doc-schemas.sh `grep -rl '#### Schema' core/ applications/ | grep -v '.erl'`
	@$(ROOT)/scripts/format-json.sh applications/crossbar/priv/api/swagger.json
	@$(ROOT)/scripts/format-json.sh applications/crossbar/priv/api/*.json
	@ERL_LIBS=deps/:core/:applications/ $(ROOT)/scripts/generate-fs-headers-hrl.escript

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
