PROJECT = blackhole
ROOT = ../..
REBAR = $(ROOT)/utils/rebar/rebar
DIALYZER = dialyzer

EBINS = $(shell find $(ROOT)/core -maxdepth 2 -name ebin -print) $(shell find $(ROOT)/deps -maxdepth 2 -name ebin -print)
PA = $(foreach EBIN,$(EBINS),-pa $(EBIN))

ERLC_OPTS = +debug_info +warn_export_all -I$(ROOT)/core -I$(ROOT)/deps $(PA)
     # +bin_opt_info

DIRS =  . \
    $(ROOT)/core/whistle-1.0.0 \
    $(ROOT)/core/whistle_amqp-1.0.0 \
    $(ROOT)/core/whistle_couch-1.0.0 \
    $(ROOT)/core/whistle_apps-1.0.0

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
	erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ $?

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/*.erl
	@mkdir -p test/
	erlc -v $(ERLC_OPTS)  -o test/ -pa test/  $?

clean:
	rm -f ebin/*
	rm -f test/*.beam test/$(PROJECT).app
	rm -f erl_crash.dump

test: clean compile-test eunit

eunit: compile-test
	erl -noshell -pa test -eval "eunit:test([$(MODULES)], [verbose])" -s init stop

dialyze:
	@$(DIALYZER) $(foreach DIR,$(DIRS),$(DIR)/ebin) \
		--plt $(ROOT)/.platform_dialyzer.plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
