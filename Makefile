DIALYZER = dialyzer
REBAR = rebar

DIRS = lib/whistle-1.0.0 lib/whistle_couch-1.0.0 lib/whistle_amqp-1.0.0 lib/whistle_number_manager-1.0.0 ecallmgr whistle_apps

all: app

app:
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

tests: clean app eunit ct

inttests: clean app eunit intct

eunit:
	@$(REBAR) eunit skip_deps=true

build-plt:
	@$(DIALYZER) --build_plt --output_plt .platform_dialyzer.plt \
		--apps kernel stdlib sasl inets crypto public_key ssl

dialyze: 
	@$(DIALYZER) $(foreach DIR,$(DIRS),--src $(DIR)/src) \
                --plt .platform_dialyzer.plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc skip_deps=true
