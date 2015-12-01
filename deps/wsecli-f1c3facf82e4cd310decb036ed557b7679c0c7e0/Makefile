.PHONY: all compile test clean

all: compile

compile: 
	./rebar get-deps compile

test: rebar compile
	./rebar skip_deps=true eunit ct

clean: rebar
	./rebar clean

rebar:
	wget https://github.com/rebar/rebar/releases/download/2.2.0/rebar
	chmod u+x rebar

shell:
	erl -pa ebin deps/*/ebin -s reloader

deps := $(wildcard deps/*/ebin)

dialyzer/erlang.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/erlang.plt \
	-o dialyzer/erlang.log --apps kernel stdlib crypto common_test ssl erts \
   	inets; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/deps.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/deps.plt \
	-o dialyzer/deps.log $(deps); \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer/wsecli.plt:
	@mkdir -p dialyzer
	@dialyzer --build_plt --output_plt dialyzer/wsecli.plt \
	-o dialyzer/wsecli.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

erlang_plt: dialyzer/erlang.plt
	@dialyzer --plt dialyzer/erlang.plt --check_plt -o dialyzer/erlang.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

deps_plt: dialyzer/deps.plt
	@dialyzer --plt dialyzer/deps.plt --check_plt -o dialyzer/deps.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

wsecli_plt: dialyzer/wsecli.plt
	@dialyzer --plt dialyzer/wsecli.plt --check_plt -o dialyzer/wsecli.log; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi

dialyzer: erlang_plt deps_plt wsecli_plt
	@dialyzer --plts dialyzer/*.plt --no_check_plt \
	--get_warnings -o dialyzer/error.log ebin; \
	status=$$? ; if [ $$status -ne 2 ]; then exit $$status; else exit 0; fi
