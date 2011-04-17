all: deps compile

deps:
	@./rebar get-deps

compile:
	@./rebar compile

test: force
	@./rebar eunit

clean:
	@./rebar clean

force: ;
