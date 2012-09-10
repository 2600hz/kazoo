REBAR=`which rebar || printf ./rebar`

all: get-deps compile

get-deps:
	@$(REBAR) get-deps

compile:
	@$(REBAR) compile

ct:
	@$(REBAR) skip_deps=true ct

eunit:
	@$(REBAR) skip_deps=true eunit

test: eunit ct

clean:
	@$(REBAR) clean
