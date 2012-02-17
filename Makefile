DIALYZER = dialyzer
REBAR = rebar

all: app

app: deps
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
	@$(DIALYZER) --src ecallmgr/src/ --src whistle_apps/src/ --src lib/whistle-1.0.0/src/ \
	        --src lib/whistle_couch-1.0.0/src/ --src lib/whistle_amqp-1.0.0/src/ \
                --src lib/whistle_number_manager-1.0.0/src/ \
                --plt .platform_dialyzer.plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc skip_deps=true
