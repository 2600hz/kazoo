ROOT = ..
DIALYZER = dialyzer
REBAR = $(ROOT)/bin/rebar

MAKEDIRS = lib/*/Makefile \
	   apps/*/Makefile

DIRS =  . \
	lib/whistle_number_manager-1.0.0 \
	lib/whistle_couch-1.0.0 \
	lib/whistle_services-1.0.0

.PHONY: $(MAKEDIRS)

all : compile

compile: ACTION = all
compile: $(MAKEDIRS)
	@$(REBAR) compile

top_only:
	@$(REBAR) compile

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)

deps : ACTION = get-deps
deps : $(MAKEDIRS)
	@$(REBAR) deps

clean : ACTION = clean
clean : $(MAKEDIRS)
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f *crash.dump

test : clean app eunit

eunit :
	@$(REBAR) eunit skip_deps=true

build-plt :
	@$(DIALYZER) --build_plt --output_plt $(ROOT)/.platform_dialyzer.plt \
                --apps erts kernel stdlib sasl inets crypto public_key ssl

dialyze :
	@$(DIALYZER) $(foreach DIR,$(DIRS),$(DIR)/ebin) \
                --plt $(ROOT)/.platform_dialyzer.plt --no_native \
                -Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc skip_deps=true
