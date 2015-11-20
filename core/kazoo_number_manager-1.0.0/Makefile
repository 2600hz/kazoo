PROJECT = kazoo_number_manager
ROOT = ../..

EBINS = $(shell find $(ROOT)/deps/lager-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/kazoo_documents-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/kazoo_web-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_couch-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_services-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/whistle_transactions-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/kazoo_xml-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/cowlib-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/deps/ejson-* -maxdepth 2 -name ebin -print) \
	$(shell find $(ROOT)/core/kazoo_number_manager-* -maxdepth 2 -name ebin -print)
PA = $(foreach EBIN,$(EBINS),-pa $(EBIN))

ERLC_OPTS = -Werror +debug_info $(PA)

.PHONY: all compile clean

all: compile

MODULES = $(shell ls src/*.erl | sed 's/src\///;s/\.erl/,/' | sed '$$s/.$$//')
CARRIERS = $(shell ls src/carriers/*.erl | sed 's/src\/carriers\///;s/\.erl/,/' | sed '$$s/.$$//')
PROVIDERS = $(shell ls src/providers/*.erl | sed 's/src\/providers\///;s/\.erl/,/' | sed '$$s/.$$//')
CONVERTERS = $(shell ls src/converters/*.erl | sed 's/src\/converters\///;s/\.erl/,/' | sed '$$s/.$$//')
TEST_MODULES = $(shell ls test/*.erl | sed 's/test\///;s/\.erl/,/' | sed '$$s/.$$//')

compile: ebin/$(PROJECT).app json
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(CARRIERS),$(PROVIDERS),$(CONVERTERS)\]}/' \
		> ebin/$(PROJECT).app
	-@$(MAKE) ebin/$(PROJECT).app

ebin/$(PROJECT).app: src/*.erl src/carriers/*.erl src/providers/*.erl src/converters/*.erl
	@mkdir -p ebin/
	erlc -v $(ERLC_OPTS) +warn_export_all -o ebin/ -pa ebin/ $?

json: JSON = $(shell find priv/ -name *.json -print)
json:
	@for jobj in $(JSON); do \
		echo checking $$jobj; \
		python -c 'import sys,json; json.encoder.FLOAT_REPR=str; data=json.load(open("'$$jobj'")); print json.dumps(data, sort_keys=True, indent=4, separators=(",", ": "))' >$$jobj~ && mv $$jobj~ $$jobj; \
	done

compile-test: test/$(PROJECT).app
	@cat src/$(PROJECT).app.src \
		| sed 's/{modules, \[\]}/{modules, \[$(MODULES),$(CARRIERS),$(PROVIDERS),$(CONVERTERS),$(TEST_MODULES)\]}/' \
		> test/$(PROJECT).app
	-@$(MAKE) test/$(PROJECT).app

test/$(PROJECT).app: src/*.erl src/carriers/*.erl src/providers/*.erl src/converters/*.erl test/*.erl
	@mkdir -p test/
	erlc -v $(ERLC_OPTS) -DTEST -o test/ -pa test/ $?

clean:
	rm -f ebin/*
	rm -f erl_crash.dump

clean-test:
	rm -f test/*.beam test/$(PROJECT).app test/*.html

test: clean-test compile-test eunit

eunit: compile-test
	erl -noshell $(PA) -pa test -eval "case eunit:test([$(TEST_MODULES),$(MODULES),$(CARRIERS),$(PROVIDERS),$(CONVERTERS)], [verbose]) of 'ok' -> init:stop(); _ -> init:stop(1) end."

cover: clean-test compile-test
	erl -noshell $(PA) -pa test -s cover -eval "Fs = [$(TEST_MODULES),$(MODULES),$(CARRIERS),$(PROVIDERS),$(CONVERTERS)], Each = fun(F, L) -> lists:foreach(fun(I) -> F(I) end, L) end, [cover:compile_module(F, [{'d', 'TEST', 'true'}]) || F <- filelib:fold_files(code:lib_dir(kazoo_number_manager, src), \"\\.erl\$\", true, fun(F, A) -> [F|A] end, [])], Cover = fun() -> Each(fun(F) -> cover:analyse_to_file(F, [\"test/\", atom_to_list(F), \".html\"], [html]) end, Fs) end, case eunit:test(Fs, [verbose]) of 'ok' -> Cover(), init:stop(); _ -> Cover(), init:stop(1) end."
