PROJECT = erlydtl
ROOT = ../..

EBINS = $(shell find $(ROOT)/deps/merl-* -maxdepth 2 -name ebin -print)
PA = $(foreach EBIN,$(EBINS),-pa $(EBIN))

ERLC_OPTS += +debug_info +warn_export_all $(PA) -Iinclude

.PHONY: all compile clean

all: compile

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')
FILTERS = $(shell ls src/filter_lib/*.erl | sed 's/src\/filter_lib\///;s/\.erl/,/' | sed '$$s/.$$//')
I18N = $(shell ls src/i18n/*.erl | sed 's/src\/i18n\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(FILTERS),$(I18N)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.yrl
	erlc -o src/ $?
	-@$(MAKE) ebin/beam

ebin/beam: src/*.erl src/*/*.erl
	@mkdir -p ebin/
	erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ $?

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(FILTERS),$(I18N)\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/*.erl src/*/*.erl
	@mkdir -p test/
	erlc -v $(ERLC_OPTS)  -o test/ -pa test/  $?

clean:
	rm -f ebin/*
	rm -f src/erlydtl_parser.erl
	rm -f test/*.beam test/$(PROJECT).app
	rm -f erl_crash.dump

test: clean compile-test eunit

eunit: compile-test
	erl -noshell -pa test -eval "eunit:test([$(MODULES),$(FILTERS),$(I18N)], [verbose])" -s init stop
