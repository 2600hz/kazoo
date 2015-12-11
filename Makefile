ROOT = .

KAZOODIRS = core/Makefile \
	    applications/Makefile

MAKEDIRS = deps/Makefile \
	   core/Makefile \
	   applications/Makefile

.PHONY: $(MAKEDIRS) core deps apps xref dialyze dialyze-apps dialyze-core dialyze-kazoo

all : compile

compile: ACTION = all
compile: $(MAKEDIRS)

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)

clean: ACTION = clean
clean: $(MAKEDIRS)
	$(if $(wildcard *crash.dump), rm *crash.dump)
	$(if $(wildcard scripts/log/*), rm -rf scripts/log/*)

clean-test : ACTION = clean-test
clean-test : $(KAZOODIRS)

eunit: ACTION = test
eunit: $(KAZOODIRS)

proper: ACTION = test
proper: ERLC_OPTS += -DPROPER
proper: $(KAZOODIRS)

test: ACTION = test
test: ERLC_OPTS += -DPROPER
test: $(KAZOODIRS)

core:
	$(MAKE) -C core all
deps:
	$(MAKE) -C deps all
apps:
	$(MAKE) -C applications all

kazoo: core apps

DIALYZER ?= dialyzer
PLT ?= $(ROOT)/.kazoo.plt
$(PLT): DEPS_SRCS  ?= $(shell find $(ROOT)/deps -name src  -print)
# $(PLT): CORE_EBINS ?= $(shell find $(ROOT)/core -name ebin -print)
$(PLT):
	@$(DIALYZER) --no_native --build_plt --output_plt $(PLT) \
	    --apps erts kernel stdlib crypto public_key ssl \
	    -r $(DEPS_SRCS)
	@for ebin in $(CORE_EBINS); do \
	    $(DIALYZER) --no_native --add_to_plt --plt $(PLT) --output_plt $(PLT) -r $$ebin; \
	done
build-plt: $(PLT)

dialyze-kazoo: TO_DIALYZE  = $(shell find $(ROOT)/applications -name ebin -print) $(shell find $(ROOT)/core -name ebin -print)
dialyze-kazoo: dialyze
dialyze-apps:  TO_DIALYZE  = $(shell find $(ROOT)/applications -name ebin -print)
dialyze-apps: dialyze
dialyze-core:  TO_DIALYZE  = $(shell find $(ROOT)/core         -name ebin -print)
dialyze-core: dialyze
dialyze:       TO_DIALYZE ?= $(shell find $(ROOT)/applications -name ebin -print)
dialyze: $(PLT)
	@$(ROOT)/scripts/check-dialyzer.escript $(TO_DIALYZE)

xref: EBINS = $(shell find $(ROOT) -name ebin -print)
xref:
	@$(ROOT)/scripts/check-xref.escript $(EBINS)

sup_completion: sup_completion_file = $(ROOT)/sup.bash
sup_completion: kazoo
	@$(if $(wildcard $(sup_completion_file)), rm $(sup_completion_file))
	@$(ROOT)/scripts/sup-build-autocomplete.escript $(sup_completion_file) applications/ core/
	@echo SUP Bash completion file written at $(sup_completion_file)
