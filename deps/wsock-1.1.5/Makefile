ROOT = ../..
REBAR = $(ROOT)/utils/rebar/rebar
PROJECT = wsock
DIALYZER = dialyzer

all: app

deps:
	@$(REBAR) get-deps

app: deps
	@$(REBAR) compile

clean: 
	@$(REBAR) clean
	rm -f erl_crash.dump



build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps erts kernel stdlib sasl tools inets crypto public_key ssl eunit

dialyze: app
	@$(DIALYZER) ebin/nksip*.beam --plt .$(PROJECT).plt \
	-Werror_handling  #-Wunmatched_returns -Wrace_conditions -Wunderspecs



