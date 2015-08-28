PROJECT = whistle_services
ROOT = ../..

EBINS = $(shell find $(ROOT)/core/whistle-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/lager-* -maxdepth 2 -name ebin -print)
PA = $(foreach EBIN,$(EBINS),-pa $(EBIN))

TEST_EBINS = $(shell find $(ROOT)/core/whistle-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_couch-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/kazoo_documents-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/ejson-* -maxdepth 2 -name ebin -print)
TEST_PA = $(foreach TEST_EBIN,$(TEST_EBINS),-pa $(TEST_EBIN))

ERLC_OPTS += -Werror +debug_info +warn_export_all $(PA)
ELIBS = $(ERL_LIBS):$(subst $(eval) ,:,$(wildcard $(ROOT)/core))

.PHONY: all compile clean

all: compile

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')
TEST_MODULES = $(shell ls test/*.erl | sed 's/test\///;s/\.erl/,/' | sed '$$s/.$$//')
BK_MODULES = $(shell ls src/bookkeepers/*.erl | sed 's/src\/bookkeepers\///;s/\.erl/,/' | sed '$$s/.$$//')
SRV_MODULES = $(shell ls src/services/*.erl | sed 's/src\/services\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app json
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(BK_MODULES),$(SRV_MODULES)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl src/bookkeepers/*.erl src/services/*.erl
	@mkdir -p ebin/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) -o ebin/ -pa ebin/ $?

json: JSON = $(shell find priv -name *.json -print)
json:
	@$(ROOT)/scripts/format-json.sh $(JSON)

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(BK_MODULES),$(SRV_MODULES),$(TEST_MODULES)\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/*.erl src/bookkeepers/*.erl src/services/*.erl test/*.erl
	@mkdir -p test/
	ERL_LIBS=$(ELIBS) erlc -v $(ERLC_OPTS) -DTEST -o test/ -pa test/ $?

clean:
	rm -f ebin/*
	rm -f erl_crash.dump

clean-test:
	rm -f test/*.beam test/$(PROJECT).app

test: clean-test compile-test eunit

eunit: compile-test
	erl -noshell $(TEST_PA) $(PA) -pa test -eval "case eunit:test([$(TEST_MODULES),$(MODULES),$(BK_MODULES),$(SRV_MODULES)], [verbose]) of 'ok' -> init:stop(); _ -> init:stop(1) end."
