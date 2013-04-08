PROJECT = pivot

ROOT = ../../..
DIALYZER = dialyzer

LIB_EBINS = $(shell find $(ROOT)/lib -maxdepth 2 -name ebin -print)
WHAPPS_LIB_EBINS = $(shell find $(ROOT)/whistle_apps/lib -maxdepth 2 -name ebin -print)

DIRS =  . \
        $(ROOT)/lib/whistle-1.0.0 \
	$(ROOT)/lib/whistle_amqp-1.0.0 \
        $(ROOT)/whistle_apps/lib/whistle_couch-1.0.0 \
        $(ROOT)/whistle_apps

ERLC_OPTS = +debug_info +warn_export_all \
        $(foreach LIB_EBIN,$(LIB_EBINS),-pa $(LIB_EBIN)) \
        $(foreach WHAPPS_LIB_EBIN,$(WHAPPS_LIB_EBINS),-pa $(WHAPPS_LIB_EBIN)) \
        -I $(ROOT)/lib -I $(ROOT)/whistle_apps/lib # +bin_opt_info 

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
