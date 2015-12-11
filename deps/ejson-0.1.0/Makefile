ROOT = ../..
REBAR = $(ROOT)/utils/rebar/rebar

%.beam: %.erl
	erlc -o t/ $<

all: app

app: compile

compile:
	@mkdir -p ebin
	@$(REBAR) compile

deps:
	@rebar get-deps

check: t/etap.beam t/util.beam
	@prove t/*.t

check_verbose: t/etap.beam t/util.beam
	@prove -v t/*.t

clean:
	@$(REBAR) clean
	@rm -f t/*.beam
