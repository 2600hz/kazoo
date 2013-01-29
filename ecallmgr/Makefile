PROJECT=ecallmgr
ROOT = ..
DIALYZER = dialyzer

ERLC_OPTS = -Werror +debug_info +warn_export_all # +bin_opt_info 

DIRS = . \
	$(ROOT)/lib/whistle-1.0.0 \
	$(ROOT)/lib/whistle_amqp-1.0.0

.PHONY: all compile clean

all: compile

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl
	@mkdir -p ebin/
	erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ -pa $(ROOT)/lib \
		src/switchblade_transform.erl $?

clean:
	rm -r ebin/*
	rm -f test/*.beam
	rm -f erl_crash.dump

test: clean app eunit

eunit:
	erl -noshell -pa ebin -eval "eunit:test()" -s init stop

build-plt:
	@$(DIALYZER) --build_plt --output_plt $(ROOT)/.platform_dialyzer.plt \
		--apps kernel stdlib sasl inets crypto public_key ssl

dialyze: 
	@$(DIALYZER) $(foreach DIR,$(DIRS),$(DIR)/ebin) \
                --plt $(ROOT)/.platform_dialyzer.plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
