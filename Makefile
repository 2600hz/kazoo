ROOT = .
RELX = $(ROOT)/make/relx/relx
ELVIS = $(ROOT)/make/elvis/elvis

KAZOODIRS = core/Makefile applications/Makefile

.PHONY: $(KAZOODIRS) deps core apps xref xref_release dialyze dialyze-apps dialyze-core dialyze-kazoo clean clean-test clean-release build-release build-ci-release tar-release release read-release-cookie elvis install

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

clean-test: ACTION = clean-test
clean-test: $(KAZOODIRS)

clean-kazoo: ACTION = clean
clean-kazoo: $(KAZOODIRS)

compile-test: ACTION = compile-test
compile-test: $(KAZOODIRS)

eunit: ACTION = eunit
eunit: $(KAZOODIRS)

proper: ACTION = eunit
proper: ERLC_OPTS += -DPROPER
proper: $(KAZOODIRS)

test: ACTION = test
test: ERLC_OPTS += -DPROPER
test: $(KAZOODIRS)

check: ERLC_OPTS += -DPROPER
check: compile-test eunit clean-kazoo kazoo

clean-deps:
	$(if $(wildcard deps/), $(MAKE) -C deps/ clean)
	$(if $(wildcard deps/), rm -r deps/)

.erlang.mk: ERLANGMK_VERSION = '2.0.0-pre.2'
.erlang.mk:
	wget 'https://raw.githubusercontent.com/ninenines/erlang.mk/$(ERLANGMK_VERSION)/erlang.mk' -O $(ROOT)/erlang.mk

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


clean-release:
	$(if $(wildcard _rel/), rm -r _rel/)
	$(if $(wildcard rel/relx.config rel/vm.args rel/dev-vm.args), \
	  rm $(wildcard rel/relx.config rel/vm.args rel/dev-vm.args)  )

build-release: clean-release rel/relx.config rel/vm.args
	$(RELX) --config rel/relx.config -V 2 release --relname 'kazoo'
build-ci-release: clean-release rel/relx.config rel/vm.args
	$(RELX) --config rel/relx.config -V 2 release --relname 'kazoo' --sys_config rel/ci-sys.config
tar-release: rel/relx.config rel/vm.args
	$(RELX) --config rel/relx.config -V 2 release tar --relname 'kazoo'
rel/relx.config: rel/relx.config.src
	$(ROOT)/scripts/src2any.escript $<

rel/dev-vm.args: rel/args  # Used by scripts/dev-start-*.sh
	cp $^ $@
rel/vm.args: rel/args rel/dev-vm.args
	( cat $<; echo '$${KZname}' ) > $@

## More ACTs at //github.com/erlware/relx/priv/templates/extended_bin
release: ACT ?= console # start | attach | stop | console | foreground
release: REL ?= kazoo_apps # kazoo_apps | ecallmgr | …
ifneq ($(findstring kazoo_apps,$(REL)),kazoo_apps)
release: export KAZOO_APPS = 'ecallmgr'
endif
release:
	@RELX_REPLACE_OS_VARS=true KZname='-name $(REL)' _rel/kazoo/bin/kazoo $(ACT) "$$@"

install: compile build-release
	cp -a _rel/kazoo /opt

read-release-cookie: REL ?= kazoo_apps
read-release-cookie:
	@RELX_REPLACE_OS_VARS=true KZname='-name $(REL)' _rel/kazoo/bin/kazoo escript lib/kazoo_config-*/priv/read-cookie.escript "$$@"

DIALYZER ?= dialyzer
PLT ?= .kazoo.plt
OTP_APPS ?= erts kernel stdlib crypto public_key ssl asn1 inets
$(PLT): DEPS_SRCS  ?= $(shell find $(ROOT)/deps -name src )
# $(PLT): CORE_EBINS ?= $(shell find $(ROOT)/core -name ebin)
$(PLT):
	@$(DIALYZER) --no_native --build_plt --output_plt $(PLT) \
	    --apps $(OTP_APPS) \
	    -r $(DEPS_SRCS)
	@for ebin in $(CORE_EBINS); do \
	    $(DIALYZER) --no_native --add_to_plt --plt $(PLT) --output_plt $(PLT) -r $$ebin; \
	done
build-plt: $(PLT)

dialyze-kazoo: TO_DIALYZE  = $(shell find $(ROOT)/applications $(ROOT)/core -name ebin)
dialyze-kazoo: dialyze
dialyze-apps:  TO_DIALYZE  = $(shell find $(ROOT)/applications -name ebin)
dialyze-apps: dialyze
dialyze-core:  TO_DIALYZE  = $(shell find $(ROOT)/core         -name ebin)
dialyze-core: dialyze
dialyze:       TO_DIALYZE ?= $(shell find $(ROOT)/applications -name ebin)
dialyze: $(PLT)
	@$(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt $(TO_DIALYZE)


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


elvis:
	$(ELVIS) --config make/elvis.config rock
