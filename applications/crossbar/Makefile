PROJECT = crossbar
ROOT = ../..
REBAR = $(ROOT)/utils/rebar/rebar
DIALYZER = dialyzer

EBINS = $(shell find $(ROOT)/core -maxdepth 2 -name ebin -print) $(shell find $(ROOT)/deps -maxdepth 2 -name ebin -print)
PA = $(foreach EBIN,$(EBINS),-pa $(EBIN))

ERLC_OPTS = +debug_info +warn_export_all -I$(ROOT)/core -I$(ROOT)/deps $(PA)
     # +bin_opt_info

ERL_LIBS = $(subst $(eval) ,:,$(wildcard $(ROOT)/deps/rabbitmq_client-*/deps))

DIRS =  . \
    $(ROOT)/core/whistle-1.0.0

.PHONY: all compile clean

all: compile

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')
CB_MODULES = $(shell ls src/modules/*.erl | sed 's/src\/modules\///;s/\.erl/,/' | sed '$$s/.$$//')
CB_MODULES_V1 = $(shell ls src/modules_v1/*.erl | sed 's/src\/modules_v1\///;s/\.erl/,/' | sed '$$s/.$$//')
CB_MODULES_V2 = $(shell ls src/modules_v2/*.erl | sed 's/src\/modules_v2\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(CB_MODULES),$(CB_MODULES_V1),$(CB_MODULES_V2)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl src/*/*.erl
	@mkdir -p ebin/
	ERL_LIBS=$(ERL_LIBS) erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ $?

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(CB_MODULES,$(CB_MODULES_V1),$(CB_MODULES_V2))\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/*.erl src/modules/*.erl
	@mkdir -p test/
	ERL_LIBS=$(ERL_LIBS) erlc -v $(ERLC_OPTS)  -o test/ -pa test/  $?

clean:
	rm -f ebin/*
	rm -f test/*.beam test/$(PROJECT).app
	rm -f erl_crash.dump

test: clean compile-test eunit

eunit: compile-test
	erl -noshell -pa test -eval "eunit:test([$(MODULES),$(CB_MODULES,$(CB_MODULES_V1),$(CB_MODULES_V2))], [verbose])" -s init stop

dialyze:
	@$(DIALYZER) $(foreach DIR,$(DIRS),$(DIR)/ebin) \
		--statistics\
		--plt $(ROOT)/.platform_dialyzer.plt --no_native \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
