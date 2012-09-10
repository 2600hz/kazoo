REBAR=`which rebar || echo ./rebar`

all: compile

compile:
	@$(REBAR) compile

test: force
	@$(REBAR) eunit

clean:
	@$(REBAR) clean

force: ;
