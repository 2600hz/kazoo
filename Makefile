ROOT = .

KAZOODIRS = core/Makefile \
	    applications/Makefile

MAKEDIRS = deps/Makefile \
	   core/Makefile \
	   applications/Makefile

.PHONY: $(MAKEDIRS) core deps apps

all : compile

compile: ACTION = all
compile: $(MAKEDIRS)

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)

clean: ACTION = clean
clean: $(MAKEDIRS)
	rm -f *crash.dump
	rm -rf scripts/log/*

clean-test : ACTION = clean-test
clean-test : $(KAZOODIRS)

eunit: ACTION = test
eunit: ERLC_OPTS += -DTEST
eunit: $(KAZOODIRS)

proper: ACTION = test
proper: ERLC_OPTS += -DPROPER
proper: $(KAZOODIRS)

test: ACTION = test
test: ERLC_OPTS += -DTEST -DPROPER
test: $(KAZOODIRS)

core:
	$(MAKE) -C core all
deps:
	$(MAKE) -C deps all
apps:
	$(MAKE) -C applications all

kazoo: core apps

PLT = $(ROOT)/.kazoo.plt
build-plt: DIALYZER ?= dialyzer
build-plt: DEPS_DIRS = $(wildcard $(ROOT)/deps/*-*/)
build-plt:
	$(DIALYZER) --no_native --build_plt --output_plt $(PLT) \
	    --apps erts kernel stdlib crypto public_key ssl -r $(DEPS_DIRS)
dialyze: DIALYZER_DIRS = $(wildcard $(ROOT)/applications/*/) $(wildcard $(ROOT)/core/*/)
dialyze: DIALYZER_OPTS ?= -Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
dialyze:
	$(DIALYZER) --no_native --plt $(PLT) -r $(DIALYZER_DIRS) $(DIALYZER_OPTS)

xref: EBINS = $(shell find $(ROOT) -name ebin -print)
xref:
	@$(ROOT)/scripts/check-xref.escript $(EBINS)
