PROJECT = whistle_number_manager
ROOT = ../..
DIALYZER = dialyzer

LIB_EBINS = $(shell find $(ROOT)/lib -maxdepth 2 -name ebin -print)
PA = $(foreach LIB_EBIN,$(LIB_EBINS),-pa $(LIB_EBIN))

ERLC_OPTS = +debug_info +warn_export_all -I$(ROOT)/lib $(PA)
	 # +bin_opt_info

DIRS = . \
	$(ROOT)/lib/whistle_amqp-1.0.0

.PHONY: all compile clean

all: compile

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')
CARRIERS = $(shell ls src/carriers/*.erl | sed 's/src\/carriers\///;s/\.erl/,/' | sed '$$s/.$$//')
PROVIDERS = $(shell ls src/providers/*.erl | sed 's/src\/providers\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(CARRIERS),$(PROVIDERS)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl src/carriers/*.erl src/providers/*.erl
	@mkdir -p ebin/
	erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ -pa $(ROOT)/lib $?

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(CARRIERS),$(PROVIDERS)\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/*.erl src/carriers/*.erl src/providers/*.erl
	@mkdir -p test/
	erlc -v $(ERLC_OPTS) -DTEST -o test/ -pa test/ -pa $(ROOT)/lib $?

clean:
	rm -f ebin/*
	rm -f test/*
	rm -f erl_crash.dump

test: clean compile-test eunit

eunit: compile-test
	erl -noshell $(PA) \
	-pa test -eval "eunit:test([$(MODULES),$(CARRIERS),$(PROVIDERS)], [verbose])" -s init stop

dialyze: 
	@$(DIALYZER) $(foreach DIR,$(DIRS),$(DIR)/ebin) \
                $(PA) --plt $(ROOT)/.platform_dialyzer.plt \
		-Werror_handling -Wrace_conditions -Wunmatched_returns # -Wunderspecs
