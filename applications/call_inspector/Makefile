PROJECT = call_inspector
ROOT = ../..

EBINS = $(shell find $(ROOT)/core/whistle-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/lager-* -maxdepth 2 -name ebin -print)
PA = $(foreach EBIN,$(EBINS),-pa $(EBIN))

ERLC_OPTS += -Werror +debug_info +warn_export_all $(PA)
ELIBS = $(ERL_LIBS):$(subst $(eval) ,:,$(wildcard $(ROOT)/core))

TEST_EBINS = $(shell find $(ROOT)/deps/mochiweb-* -maxdepth 2 -name ebin -print) \
             $(shell find $(ROOT)/deps/ejson-* -maxdepth 2 -name ebin -print)
TEST_PA = $(foreach EBIN,$(TEST_EBINS),-pa $(EBIN))

.PHONY: all compile clean

all: compile

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')
TEST_MODULES = $(shell ls test/*.erl | sed 's/test\///;s/\.erl/,/' | sed '$$s/.$$//')
CI_ANALYZERS = $(shell ls src/analyzers/*.erl | sed 's/src\/analyzers\///;s/\.erl/,/' | sed '$$s/.$$//')
CI_PARSERS = $(shell ls src/parsers/*.erl | sed 's/src\/parsers\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(CI_ANALYZERS),$(CI_PARSERS)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl  src/*/*.erl
	@mkdir -p ebin/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ $?

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(CI_ANALYZERS),$(CI_PARSERS),$(TEST_MODULES)\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: ERLC_OPTS += +export_all
test/$(PROJECT).app: src/*.erl src/*/*.erl test/*.erl
	@mkdir -p test/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) $(TEST_PA) -DTEST -o test/ -pa test/ $?

clean:
	rm -f ebin/*
	rm -f erl_crash.dump

clean-test:
	rm -f test/*.beam test/$(PROJECT).app

test: clean-test compile-test eunit

eunit: compile-test
	erl -noshell $(PA) $(TEST_PA) -pa test -eval "case eunit:test([$(MODULES),$(CI_ANALYZERS),$(CI_PARSERS)], [verbose]) of 'ok' -> init:stop(); _ -> init:stop(1) end."
