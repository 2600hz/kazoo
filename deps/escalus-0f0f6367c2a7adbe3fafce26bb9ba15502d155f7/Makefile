.PHONY: all compile test clean

all: compile

compile: rebar
	./rebar get-deps compile

test: rebar compile
	./rebar skip_deps=true eunit

clean: rebar
	./rebar clean

# Usage: make ct SUITE=user_db_module_SUITE
#
ct:	compile logs
	./run_ct SUITE=$(SUITE)

logs:
	mkdir -p logs

rebar:
	wget https://github.com/rebar/rebar/releases/download/2.2.0/rebar
	chmod u+x rebar

deps := $(wildcard deps/*/ebin)

dialyzer/erlang.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/erlang.plt \
	-o dialyzer/erlang.log --apps kernel stdlib crypto common_test ssl erts; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/deps.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/deps.plt \
	-o dialyzer/deps.log $(deps); \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/escalus.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/escalus.plt \
	-o dialyzer/escalus.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

erlang_plt: dialyzer/erlang.plt
	@dialyzer --plt dialyzer/erlang.plt --check_plt -o dialyzer/erlang.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

deps_plt: dialyzer/deps.plt
	@dialyzer --plt dialyzer/deps.plt --check_plt -o dialyzer/deps.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

escalus_plt: dialyzer/escalus.plt
	@dialyzer --plt dialyzer/escalus.plt --check_plt -o dialyzer/escalus.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer: erlang_plt deps_plt escalus_plt
	@dialyzer --plts dialyzer/*.plt --no_check_plt \
	--get_warnings -o dialyzer/error.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

MIM := deps/mongooseim
MIM_REL := ${MIM}/rel/mongooseim

mongooseim-start: ${MIM_REL}
	${MIM_REL}/bin/mongooseimctl start && ${MIM_REL}/bin/mongooseimctl started

mongooseim-stop: ${MIM_REL}
	${MIM_REL}/bin/mongooseimctl stop && ${MIM_REL}/bin/mongooseimctl stopped > /dev/null

extra-deps: ${MIM}

${MIM_REL}: ${MIM}
	cd ${MIM} && make rel

${MIM}:
	ESCALUS_EXTRA_DEPS=mongooseim ./rebar get-deps
