ROOT = .
DIALYZER = dialyzer
REBAR = $(ROOT)/utils/rebar/rebar

MAKEDIRS = deps/Makefile \
	   core/Makefile \
	   applications/Makefile

DIRS = $(ROOT)/core/whistle-1.0.0 \
	   $(ROOT)/core/whistle_amqp-1.0.0 \
	   $(ROOT)/core/whistle_apps-1.0.0

.PHONY: $(MAKEDIRS)

all : compile

compile: ACTION = all
compile: $(MAKEDIRS)

$(MAKEDIRS):
	$(MAKE) -C $(@D) $(ACTION)

deps : ACTION = get-deps
deps : $(MAKEDIRS)

clean : ACTION = clean
clean : $(MAKEDIRS)
	rm -f test/*.beam
	rm -f *crash.dump

test : clean app eunit

eunit :
	@$(REBAR) eunit skip_deps=true

build-plt :
	@$(DIALYZER) --build_plt --output_plt $(ROOT)/.platform_dialyzer.plt \
		--apps erts kernel stdlib crypto public_key ssl

dialyze :
	@$(DIALYZER) $(foreach DIR,$(DIRS),$(DIR)/ebin) \
                --plt $(ROOT)/.platform_dialyzer.plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc skip_deps=true

update:
	./bin/git_update.sh
