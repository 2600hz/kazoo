PROJECT = nksip
ROOT = ../..

EBINS = $(shell find $(ROOT)/deps/lager-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/wsock-* -maxdepth 2 -name ebin -print)
PA = $(foreach EBIN,$(EBINS),-pa $(EBIN))

TEST_EBINS = $(shell find $(ROOT)/deps/goldrush-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/lager_syslog-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/erlang-syslog-* -maxdepth 2 -name ebin -print)
TEST_PA = $(foreach TEST_EBIN,$(TEST_EBINS),-pa $(TEST_EBIN))

ERLC_OPTS = +"{parse_transform, lager_transform}" +debug_info -Iinclude -Iplugins/include $(PA)

.PHONY: all compile clean

all: compile

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')
PLUGIN_MODULES = $(shell ls plugins/src/*.erl | sed 's/plugins\/src\///;s/\.erl/,/' | sed '$$s/.$$//')
TEST_MODULES = $(shell ls test/*.erl | sed 's/test\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(PLUGIN_MODULES)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl plugins/src/*.erl
	@mkdir -p ebin/
	erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ $?

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(TEST_MODULES),$(MODULES),$(PLUGIN_MODULES)\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/*.erl plugins/src/*.erl test/*.erl
	@mkdir -p test/
	erlc -v $(ERLC_OPTS) -DTEST -o test/ -pa test/ $?

clean:
	rm -f ebin/*
	rm -f erl_crash.dump

clean-test:
	rm -f test/*.beam test/$(PROJECT).app

test: clean-test compile-test eunit

eunit: compile-test
	erl -noshell $(PA) $(TEST_PA) -pa test -config test/app.config -eval "case eunit:test([$(TEST_MODULES),$(MODULES)], [verbose]) of 'ok' -> init:stop(); _ -> init:stop(1) end."
