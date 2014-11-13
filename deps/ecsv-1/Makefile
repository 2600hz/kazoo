PROJECT = ecsv
ROOT = ../..

ERLC_OPTS = -Werror +debug_info +warn_export_all +warn_unused_vars +warn_unused_import +warn_exported_vars

.PHONY: all compile clean

all: compile

MODULES = $(shell ls src/*.[e,y]rl | sed 's/src\///;s/\.[e,y]rl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.yrl
	erlc -v $(ERLC_OPTS) -o src/ -pa ebin/ $?
	-@$(MAKE) ebin/beam

ebin/beam: src/*.erl
	@mkdir -p ebin/
	erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ $?

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES)\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/*.yrl
	@mkdir -p test/
	erlc -v $(ERLC_OPTS) -o src/ -pa test/ $?

clean:
	rm -f ebin/*
	rm -f test/*.beam test/$(PROJECT).app
	rm -f erl_crash.dump

test: clean compile-test eunit

eunit: compile-test
	erl -noshell -pa test -eval "eunit:test([$(MODULES)], [verbose])" -s init stop
