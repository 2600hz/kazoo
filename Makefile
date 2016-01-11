ROOT = .

KAZOODIRS = core/Makefile \
	    applications/Makefile

MAKEDIRS = deps/Makefile \
	   core/Makefile \
	   applications/Makefile

.PHONY: $(MAKEDIRS) core deps apps xref xref_releases dialyze dialyze-apps dialyze-core dialyze-kazoo clean clean-releases releases rl

all: compile

compile: ACTION = all
compile: $(MAKEDIRS)

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)

clean: ACTION = clean
clean: $(MAKEDIRS)
	$(if $(wildcard *crash.dump), rm *crash.dump)
	$(if $(wildcard scripts/log/*), rm -rf scripts/log/*)

clean-releases:
	$(if $(wildcard _rel/), rm -r _rel/)
	$(if $(wildcard rel/relx.config), rm rel/relx.config)

clean-test: ACTION = clean-test
clean-test: $(KAZOODIRS)

eunit: ACTION = test
eunit: $(KAZOODIRS)

proper: ACTION = test
proper: ERLC_OPTS += -DPROPER
proper: $(KAZOODIRS)

test: ACTION = test
test: ERLC_OPTS += -DPROPER
test: $(KAZOODIRS)

core:
	$(MAKE) -C core/ all
deps:
	$(MAKE) -C deps/ all
apps:
	$(MAKE) -C applications/ all

kazoo: core apps


REL_WHAPPS = whistle_apps
REL_ECLMGR = ecallmgr
releases: RELX ?= $(ROOT)/utils/relx/relx
releases: rel/relx.config
	$(RELX) --config $< -V 2 release --relname kazoo_$(REL_WHAPPS)
	$(RELX) --config $< -V 2 release --relname kazoo_$(REL_ECLMGR)
rel/relx.config: rel/relx.config.src
	$(ROOT)/scripts/src2any.escript $<

whapps_start: ACT = start
whapps_start: rl

ecallmgr_start: ACT = start
ecallmgr_start: REL = $(REL_ECLMGR)
ecallmgr_start: rl

## More at //relx/priv/templates/extended_bin
rl: ACT ?= foreground # start attach stop console
rl: REL ?= $(REL_WHAPPS)
rl:
	RELX_REPLACE_OS_VARS=true KZname=$(REL) _rel/kazoo_$(REL)/bin/kazoo_$(REL) $(ACT) "$$@"


DIALYZER ?= dialyzer
PLT ?= .kazoo.plt
$(PLT): DEPS_SRCS  ?= $(shell find $(ROOT)/deps -name src )
# $(PLT): CORE_EBINS ?= $(shell find $(ROOT)/core -name ebin)
$(PLT):
	@$(DIALYZER) --no_native --build_plt --output_plt $(PLT) \
	    --apps erts kernel stdlib crypto public_key ssl \
	    -r $(DEPS_SRCS)
	@for ebin in $(CORE_EBINS); do \
	    $(DIALYZER) --no_native --add_to_plt --plt $(PLT) --output_plt $(PLT) -r $$ebin; \
	done
build-plt: $(PLT)

dialyze-kazoo: TO_DIALYZE  = $(shell find $(ROOT)/applications -name ebin) $(shell find $(ROOT)/core -name ebin)
dialyze-kazoo: dialyze
dialyze-apps:  TO_DIALYZE  = $(shell find $(ROOT)/applications -name ebin)
dialyze-apps: dialyze
dialyze-core:  TO_DIALYZE  = $(shell find $(ROOT)/core         -name ebin)
dialyze-core: dialyze
dialyze:       TO_DIALYZE ?= $(shell find $(ROOT)/applications -name ebin)
dialyze: $(PLT)
	@$(ROOT)/scripts/check-dialyzer.escript $(ROOT)/.kazoo.plt $(TO_DIALYZE)


xref: TO_XREF ?= $(shell find $(ROOT) -name ebin)
xref:
	@$(ROOT)/scripts/check-xref.escript $(TO_XREF)

xref_releases: TO_XREF = $(shell find $(ROOT)/_rel/kazoo_$(REL_WHAPPS)/lib) $(shell find $(ROOT)/_rel/kazoo_$(REL_ECLMGR)/lib)
xref_releases: xref


sup_completion: sup_completion_file = $(ROOT)/sup.bash
sup_completion: kazoo
	@$(if $(wildcard $(sup_completion_file)), rm $(sup_completion_file))
	@$(ROOT)/scripts/sup-build-autocomplete.escript $(sup_completion_file) applications/ core/
	@echo SUP Bash completion file written at $(sup_completion_file)
