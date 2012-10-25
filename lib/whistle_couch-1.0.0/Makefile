ROOT = ../..
DIALYZER = dialyzer
REBAR = $(ROOT)/bin/rebar

DIRS =   . \
	 $(ROOT)/lib/whistle-1.0.0 \
	 $(ROOT)/lib/couchbeam-0.8.0 \
	 $(ROOT)/lib/ibrowse-4.0.0

all: app

app:
	@$(REBAR) compile

deps:
	@$(REBAR) get-deps

clean:
	@$(REBAR) clean
	rm -f test/*.beam
	rm -f erl_crash.dump

test: clean app eunit

eunit:
	@$(REBAR) eunit skip_deps=true

build-plt:
	@$(DIALYZER) --build_plt --output_plt $(ROOT)/.platform_dialyzer.plt \
		--apps kernel stdlib sasl inets crypto public_key ssl

dialyze: 
	@$(DIALYZER) $(foreach DIR,$(DIRS),$(DIR)/ebin) \
                --plt $(ROOT)/.platform_dialyzer.plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs

docs:
	@$(REBAR) doc skip_deps=true
