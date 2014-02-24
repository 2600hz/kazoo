ROOT = ../..
REBAR = $(ROOT)/utils/rebar/rebar

ERL=erl
ERLC=erlc

all: compile test

compile:
	@$(REBAR) compile

test:
	@$(REBAR) eunit

clean:
	@$(REBAR) clean
