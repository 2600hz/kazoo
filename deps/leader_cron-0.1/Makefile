REBAR=./rebar
PLT=.leader_cron_plt
DEPS=$(wildcard deps/*/ebin)

.PHONY: doc

all: get-deps
	$(REBAR) compile

clean: clean-doc
	$(REBAR) clean
	rm -rf log logs

get-deps:
	$(REBAR) get-deps

check: all eunit ct

eunit: all
	@echo These tests take a few minutes...
	$(REBAR) eunit skip_deps=true

ct: all
	$(REBAR) ct skip_deps=true

xref:  all
	$(REBAR) xref

dialyzer:  all
	dialyzer -q --plt $(PLT) -Wno_undefined_callbacks ebin


build_plt: all
	dialyzer --build_plt --output_plt $(PLT) \
		--apps erts kernel stdlib $(DEPS)

doc:
	$(REBAR) doc

clean-doc:
	rm -rf doc
