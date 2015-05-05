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

build-plt :
	@$(DIALYZER) --build_plt --output_plt $(ROOT)/.platform_dialyzer.plt \
		--apps erts kernel stdlib crypto public_key ssl

xref: EBINS = $(shell find $(ROOT) -name ebin -print)
xref:
	@$(ROOT)/scripts/check-xref.escript $(EBINS)
