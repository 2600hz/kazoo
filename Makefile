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

DIALYZER ?= dialyzer
PLT ?= $(ROOT)/.kazoo.plt
$(PLT): DEPS_DIRS ?= $(shell find $(ROOT)/deps -name src -print)
$(PLT):
	$(DIALYZER) --no_native --build_plt --output_plt $(PLT) \
	    --apps erts kernel stdlib crypto public_key ssl \
	    -r $(DEPS_DIRS)
build-plt: $(PLT)

dialyze: TO_DIALYZE ?= $(shell find $(ROOT)/applications -name ebin -print)
dialyze: $(PLT)
	$(ROOT)/scripts/check-dialyzer.escript $(TO_DIALYZE)

xref: EBINS = $(shell find $(ROOT) -name ebin -print)
xref:
	@$(ROOT)/scripts/check-xref.escript $(EBINS)
