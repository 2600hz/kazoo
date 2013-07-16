PROJECT = whistle
ROOT = ../..
REBAR = $(ROOT)/utils/rebar/rebar
DIALYZER = dialyzer

EBINS = $(shell find $(ROOT)/core -maxdepth 2 -name ebin -print) $(shell find $(ROOT)/deps -maxdepth 2 -name ebin -print)
PA = $(foreach EBIN,$(EBINS),-pa $(EBIN))

ERLC_OPTS = +debug_info +warn_export_all -I$(ROOT)/core -I$(ROOT)/deps $(PA)
         # +bin_opt_info

ERL_LIBS = $(subst $(eval) ,:,$(wildcard $(ROOT)/deps/rabbitmq_client-*/deps))

DIRS = . \
	$(ROOT)/core/whistle_amqp-1.0.0

.PHONY: all compile clean

all: compile

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')
API_MODULES = $(shell ls src/api/*.erl | sed 's/src\/api\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(API_MODULES)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/kazoo_transform.erl src/*.erl src/api/*.erl
	@mkdir -p ebin/
	ERL_LIBS=$(ERL_LIBS) erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ $(PA) $?

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(API_MODULES)\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/kazoo_transform.erl src/*.erl src/api/*.erl
	@mkdir -p test/
	ERL_LIBS=$(ERL_LIBS) erlc -v $(ERLC_OPTS) -DTEST -o test/ -pa test/ $(PA) $?

clean:
	rm -f ebin/*
	rm -f test/*.beam test/$(PROJECT).app
	rm -f erl_crash.dump

test: clean compile-test eunit

eunit: compile-test
	erl -noshell $(PA) \
	-pa test -eval "eunit:test([$(MODULES),$(API_MODULES)], [verbose])" -s init stop

dialyze: 
	@$(DIALYZER) $(foreach DIR,$(DIRS),$(DIR)/ebin) \
		--statistics \
                $(PA) --plt $(ROOT)/.platform_dialyzer.plt \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
