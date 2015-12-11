ROOT = ../..
REBAR = $(ROOT)/utils/rebar/rebar

all: compile

compile:
	@$(REBAR) compile

test:
	@rm -rf .eunit
	@mkdir -p .eunit
	@$(REBAR) skip_deps=true eunit

clean:
	@$(REBAR) clean
