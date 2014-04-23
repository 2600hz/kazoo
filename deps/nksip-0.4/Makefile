ROOT = ../..
REBAR = $(ROOT)/utils/rebar/rebar
PROJECT = nksip
DIALYZER = dialyzer

all: app

deps:
	@$(REBAR) get-deps

app: deps
	@$(REBAR) compile

clean: clean-docs clean-logs
	@$(REBAR) clean
	rm -f erl_crash.dump

clean-logs:
	rm -rf log 
	rm -rf samples/nksip_loadtest/log 
	rm -rf samples/nksip_pbx/log 
	rm -rf samples/nksip_tutorial/log 

docs: clean-docs
	@$(REBAR) doc skip_deps=true

clean-docs:
	rm -f doc/*.css doc/*.html \
	      doc/*.png doc/edoc-info

tests: app eunit

eunit:
	@$(REBAR) eunit skip_deps=true

build-plt:
	@$(DIALYZER) --build_plt --output_plt .$(PROJECT).plt \
		--apps erts kernel stdlib sasl tools inets crypto public_key ssl eunit

dialyze: app
	@$(DIALYZER) ebin/nksip*.beam --plt .$(PROJECT).plt \
	-Werror_handling  #-Wunmatched_returns -Wrace_conditions -Wunderspecs

shell: 
	erl -config priv/app.config -args_file priv/vm.args 


build_tests:
	erlc -pa ebin -pa deps/lager/ebin -o ebin -I include \
	+export_all +debug_info test/*.erl


