ROOT = .
DIALYZER = dialyzer
REBAR = $(ROOT)/bin/rebar

MAKEDIRS = */Makefile

DIRS = $(ROOT)/lib/whistle-1.0.0 \
       $(ROOT)/lib/whistle_couch-1.0.0 \
       $(ROOT)/lib/whistle_amqp-1.0.0 \
       $(ROOT)/lib/whistle_number_manager-1.0.0 \
       $(ROOT)/ecallmgr \
       $(ROOT)/whistle_apps

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
	rm -f erl_crash.dump

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

update:
	./bin/git_update.sh
